#######################################
########## TANZANIA 2010-11 ###########
#######################################

if(Sys.info()["user"] == "Tomas"){
  dataPath <- "C:/Users/Tomas/Documents/LEI/data/TZA/2010/Data"
} else {
  dataPath <- "N:/Internationaal Beleid  (IB)/Projecten/2285000066 Africa Maize Yield Gap/SurveyData/TZA/2010/Data"
}

geoPath <- "D:/Data/Projects/TZAYG/Cache"

# load packages
library(haven)
library(stringr)
library(reshape2)
library(dplyr)

options(scipen=999)

#######################################
############## LOCATION ###############
#######################################

# file containing the zone, region and district

location <- read_dta(file.path(dataPath, "TZNPS2HH1DTA/HH_SEC_A.dta")) %>%
  select(y2_hhid, REGCODE = region, DISCODE = district, rural = y2_rural)
location$rural <- as.integer(location$rural)

# match up with the names from the survey (prepared in a seperate file)

ZONEREGDIS <- read.csv(file.path(paste0(dataPath,"/../../.."), "Other/Spatial/TZA/ZONEREGDIS.csv"))

# join with household identifications

location <- left_join(location, ZONEREGDIS)

rm(ZONEREGDIS)

#######################################
########### SOCIO/ECONOMIC ############
#######################################

HH10 <- read_dta(file.path(dataPath, "TZNPS2HH1DTA/HH_SEC_B.dta")) %>%
  select(y2_hhid, indidy2, status=hh_b05, sex=hh_b02,
         yob=hh_b03_1, age=hh_b04, years=hh_b25)

HH10$years <- as.numeric(HH10$years)
HH10$years <- ifelse(HH10$years %in% 99, HH10$age, HH10$years)
HH10$status <- as_factor(HH10$status)
HH10$sex <- toupper(as_factor(HH10$sex))
HH10$yob <- as.integer(HH10$yob)

# make a new variable cage (cut age = cage) which splits
# individuals according to their age group with
# breaks at 15, 55 and the max age

HH10$cage <- cut(HH10$age, breaks = c(0, 15, 55, max(HH10$age, na.rm=TRUE)),
                 labels=c("0-15", "16-55", "56+"), include.lowest = TRUE, right = TRUE)

# education of household members and sum
# of education of all household members
# between the ages of 15 and 55

ed <- read_dta(file.path(dataPath, "TZNPS2HH1DTA/HH_SEC_C.dta")) %>%
  select(y2_hhid, indidy2, ed_any=hh_c03, start=hh_c04, end=hh_c08)

ed$ed_any <- as_factor(ed$ed_any) # ever went to school
ed$end <- as.integer(as.character(ed$end))
ed$end <- ifelse(ed$end %in% 9999, NA, ed$end)

# join with HH10 dataframe
HH10 <- left_join(HH10, ed)
HH10$education <- HH10$end - (HH10$yob + HH10$start)
HH10$education <- ifelse(HH10$ed_any %in% "No", 0, HH10$education)
HH10 <- select(HH10, -start, -end, -yob)

# remove negative years of education (56 obs)
HH10$education <- ifelse(HH10$education < 0, NA, HH10$education)

# summarise the data: get sum of education
# of household members 15-55 and number of
# household members 15:55

HH10_x <- group_by(HH10, y2_hhid) %>%
  summarise(education1555=sum(education[cage %in% "16-55"], na.rm=T),
            N1555=sum(cage %in% "16-55"))
HH10 <- left_join(HH10, HH10_x); rm(HH10_x)

# -------------------------------------
# death in the family
# -------------------------------------

death <- read_dta(file.path(dataPath, "TZNPS2HH2DTA/HH_SEC_S.dta")) %>%
  select(y2_hhid) %>% mutate(death=1) %>% unique

# -------------------------------------
# membership to a credit group
# -------------------------------------

credit <- read_dta(file.path(dataPath, "TZNPS2HH2DTA/HH_SEC_O2.dta")) %>%
  select(y2_hhid) %>% unique() %>% mutate(SACCO = 1)

HH10 <- left_join(HH10, death) 
HH10 <- left_join(HH10, credit) 

rm(ed, credit, death)

#######################################
############### OUTPUT ################
#######################################

oput <- read_dta(file.path(dataPath, "TZNPS2AGRDTA/AG_SEC4A.dta")) %>%
  select(y2_hhid, plotnum, crop_code=zaocode, one_crop=ag4a_01,
         crop_share=ag4a_02, inter_crop=ag4a_04,
         harv_area=ag4a_08, harv_area2=ag4a_09,
         harv_area3=ag4a_10, crop_qty_harv=ag4a_15, valu=ag4a_16,
         hybrd=ag4a_23)

oput$crop_share <- as_factor(oput$crop_share)
oput$crop_code <- as.integer(oput$crop_code)
oput$harv_area2 <- as_factor(oput$harv_area2)
oput$harv_area3 <- toupper(as_factor(oput$harv_area3))

# harv area is in acres -> change to hectares
oput$harv_area <- oput$harv_area*0.404686

# -------------------------------------
# create dummy variables for crop groups
# (fruit, cash crops (permanent),
# Cereals/Tubers/Roots, cash crops (not permanent),
# vegetables, legumes)
# -------------------------------------

fruit <- c(70:74, 76:85, 97:99, 67, 38, 39)
cashCropsPerm <- c(53:61, 63:66, 18, 34, 21, 75, 44:46) # permanent cash crops
CTR <- c(11:17, 22:27) # Cereals, Tubers, Roots
cashCropNPerm <- c(50, 51, 53, 62, 19) # non permanent cash crops
vegetables <- c(86:96, 100, 101)
legumes <- c(31, 32, 33, 35, 36, 37, 41, 42, 43, 47, 48)

oput_x <- group_by(oput, y2_hhid, plotnum) %>%
  summarise(crop_count=length(unique(crop_code[!is.na(crop_code)])),
            fruit=ifelse(any(crop_code %in% fruit), 1, 0),
            cashCropsPerm=ifelse(any(crop_code %in% cashCropsPerm), 1, 0),
            CTR=ifelse(any(crop_code %in% CTR), 1, 0),
            cashCropNPerm=ifelse(any(crop_code %in% cashCropNPerm), 1, 0),
            vegetables=ifelse(any(crop_code %in% vegetables), 1, 0),
            legume=ifelse(any(crop_code %in% legumes), 1, 0),
            maize_=ifelse(any(crop_code %in% 11), 1, 0), # maize has crop code 11
            wheat=ifelse(any(crop_code %in% 16), 1, 0)) # wheat has crop code 16

oput <- left_join(oput, oput_x); rm(oput_x)

# who responded they produced zero crop, or did not respond (NA)

oput <- oput[! is.na(oput$crop_qty_harv) & !oput$crop_qty_harv %in% 0, ]
oput$crop_price <- oput$valu/oput$crop_qty_harv
oput$valu <- NULL 

rm(list=c("legumes", "cashCropNPerm", "cashCropsPerm",
          "CTR", "fruit", "vegetables"))

#######################################
############# CHEMICAL ################
#######################################

plot <- read_dta(file.path(dataPath, "TZNPS2AGRDTA/AG_SEC3A.dta")) %>%
  dplyr::select(y2_hhid, plotnum, main_crop=zaocode, soil=ag3a_09, slope_farmer=ag3a_16, irrig=ag3a_17, title=ag3a_27,
                manure=ag3a_39, pest=ag3a_58, pest_q=ag3a_60_1, pest_q_unit=ag3a_60_2, fallow_year=ag3a_21, fallow=ag3a_22)

plot$main_crop <- as.integer(plot$main_crop)
plot$soil <- factor(plot$soil, levels=c(1,2,3,4), labels=c("Sandy", "Loam", "Clay", "Other"))
plot$slope_farmer <- factor(plot$slope_farmer, levels=c(1,2,3,4), labels=c("Flat bottom", "Flat top", "Slightly sloped", "Very steep"))
plot$pest_q_unit <- as_factor(plot$pest_q_unit)

plot$pest_q <- ifelse(plot$pest_q_unit %in% c("LITRE", "KG"), plot$pest_q,
                      ifelse(plot$pest_q_unit %in% "MILLILITRE", plot$pest_q*0.001, NA))

# two questions on fallow - make sure they match up correctly
# fallow value of 98 means subject did not know how long plot
# was left fallow

plot$fallow_year <- ifelse(plot$fallow_year %in% 98, NA, plot$fallow_year)
plot$fallow <- ifelse(plot$fallow_year %in% 0, 0, plot$fallow )
plot$fallow <- ifelse(is.na(plot$fallow_year), NA, plot$fallow)
plot <- dplyr::select(plot, -fallow_year, - pest_q_unit)

fert1 <- read_dta(file.path(dataPath, "TZNPS2AGRDTA/AG_SEC3A.dta")) %>%
  dplyr::select(y2_hhid, plotnum, typ=ag3a_46, qty=ag3a_47, vouch=ag3a_48, valu=ag3a_49)

fert2 <- read_dta(file.path(dataPath, "TZNPS2AGRDTA/AG_SEC3A.dta")) %>%
  dplyr::select(y2_hhid, plotnum, typ=ag3a_53, qty=ag3a_54, vouch=ag3a_55, valu=ag3a_56)

fert1$typ <- as_factor(fert1$typ)
fert1$vouch <- ifelse(fert1$vouch %in% 2, 0, fert1$vouch)

fert2$typ <- as_factor(fert2$typ)
fert2$vouch <- ifelse(fert2$vouch %in% 2, 0, fert2$vouch)

levels(fert1$typ) <- levels(fert2$typ) <-
  c("DAP", "UREA", "TSP", "CAN", "SA", "generic NPK (TZA)", "MRP")

# -------------------------------------
# reorganize data so that observations
# on fertilizer type occupy a single row
# fertilizer is unit of observation
# Data on NPK composition from Sheahan et al (2014), Food Policy
# -------------------------------------

conv <- read.csv(file.path(paste0(dataPath,"/../../.."), "Other/Fertilizer/Fert_comp.csv")) %>%
  transmute(typ=Fert_type2, n=N_share/100, p=P_share/100) %>%
  filter(typ %in% levels(fert1$typ))

fert1 <- left_join(fert1, conv)
fert2 <- left_join(fert2, conv)

# -------------------------------------
# organize fertilizer data for analysis

fert <- rbind(fert1, fert2)

fert <- mutate(fert,
               Vfert=valu/qty,
               Qn=qty*n,
               Qp=qty*p)

fert$Pn <- fert$Vfert/fert$n

# Compute subsidised, non-subsidised and mix fertilizer prices per plot
# As valu or N is sometimes 0, all prices that are 0 are set to NA

fertnosub   <- filter(fert, vouch==0) %>%
  group_by(y2_hhid, plotnum) %>%
  summarise(N=sum(Qn, na.rm=TRUE),
            WPnnosub=sum((Qn/N)*Pn, na.rm=TRUE)) %>%
  select(-N) %>%
  mutate(WPnnosub = replace(WPnnosub, WPnnosub==0, NA))


fertsub <- filter(fert, vouch==1) %>%
  group_by(y2_hhid, plotnum) %>%
  summarise(N=sum(Qn, na.rm=TRUE),
            WPnsub=sum((Qn/N)*Pn, na.rm=TRUE)) %>%
  select(-N) %>% 
  mutate(WPnsub = replace(WPnsub, WPnsub==0, NA))

fertmix <- filter(fert, vouch %in% c(0,1)) %>%
  group_by(y2_hhid, plotnum) %>%
  summarise(N=sum(Qn, na.rm=TRUE),
            P=sum(Qp, na.rm=TRUE),
            WPn=sum((Qn/N)*Pn, na.rm=TRUE)) %>%
  mutate(WPn = replace(WPn, WPn==0, NA))

# join back with the rest of the data and set N and P to 0 for NA values

plot <- left_join(plot, fertmix) %>%
  left_join(., fertnosub) %>%
  left_join(., fertsub)

rm(list=c("fert1", "fert2", "fert", "fertsub", "fertnosub", "fertmix", "conv"))

#######################################
############### LABOUR ################
#######################################

lab <- read_dta(file.path(dataPath, "\\TZNPS2AGRDTA/AG_SEC3A.dta")) %>%
  dplyr::select( y2_hhid, plotnum, ag3a_70_id1:ag3a_72_9 )

# remove houshold labour IDs and question ag3a_71 which we don't need
bad <- grep( "ag3a_70_id", names( lab ) )
lab <- lab[, -bad]
lab <- dplyr::select( lab, -ag3a_71 )

# remove variables that refer to wage paid for hired labour
# this could be added later to consider input costs
bad <- names( lab )[( length( lab )-15 ):length( lab )][seq( from=4, to=16, by=4 )]
lab <- lab[, -which( names( lab ) %in% bad )]

# create a dataframe with just family and hired labour
lab <- transmute( lab, y2_hhid, plotnum,
                  fam_lab_days=rowSums( lab[, 3:26], na.rm=TRUE ),
                  hir_lab_days=rowSums( lab[, 27:ncol( lab )], na.rm=TRUE ) )

lab <- transmute(lab, y2_hhid, plotnum, lab=fam_lab_days + hir_lab_days)

# doesn't make sense to have 0 labour on a plot so set values to zero
lab$lab <- ifelse(lab$lab %in% 0, NA, lab$lab)

rm(bad)

#######################################
############### GEO ###################
#######################################

# add Michiel's geo files
geo10 <- readRDS(file.path(geoPath, "TZA_geo_2010.rds")) 

#######################################
############### AREAs #################
#######################################

areas <- read_dta(file.path(dataPath, "areas_tza_y2_imputed.dta")) %>%  
  select(y2_hhid=case_id, plotnum,
                area_farmer=area_sr, area_gps=area_gps_mi_50)

areas$area_gps <- ifelse(areas$area_gps %in% 0, NA, areas$area_gps)

# add an ownership variable if the household owns
# a plot 

own <- read_dta(file.path(dataPath, "TZNPS2AGRDTA/AG_SEC3A.dta")) %>%
    select(y2_hhid, plotnum, own=ag3a_24)
own$own <- ifelse(own$own %in% 1 | own$own %in% 5, 1, 0)
areas <- left_join(areas, own); rm(own)

# calcualte the households total land holdings
areaTotal <- group_by(areas, y2_hhid) %>%
  summarise(area_tot = sum(area_gps))

areaTotal$area_tot <- ifelse(areaTotal$area_tot %in% 0, NA, areaTotal$area_tot)

#######################################
############### ASSETS ################
#######################################

implmt <- read_dta(file.path(dataPath, "TZNPS2AGRDTA/AG_SEC11.dta")) %>%
  dplyr::select(y2_hhid, itemcode, qty=ag11_01, valu=ag11_02) %>%
  filter(!qty %in% 0, !is.na(qty), !valu %in% 0, !is.na(valu)) %>%
  transmute(y2_hhid, valu=qty*valu) %>%
  group_by(y2_hhid) %>%
  summarise(asset=sum(valu))

# -------------------------------------
# Livestock assets
# -------------------------------------

# classifications from wave 3 classification table
LR <- c("BULLS", "COWS", "STEERS", "HEIFERS", "MALE-CALVES", "FEMALE-CALVES")
SR <- c("GOATS", "SHEEP")
PIGS <- c("PIGS")
POULTRY <- c("CHICKENS", "DUCKS", "TURKEYS")
OTHER <- c("RABBITS", "HORSES", "DOG", "OTHER")

# read in the data
lvstock <- read_dta(file.path(dataPath, "TZNPS2AGRDTA/AG_SEC10A.dta")) %>%
  select(y2_hhid, animal = lvstkcode, owned = ag10a_02, indigQty = ag10a_05_1,
         improvBeefQty = ag10a_05_2, improvDairyQty = ag10a_05_3 )
lvstock$owned <- ifelse(lvstock$owned %in% 1, 1, 0)
lvstock$animal <- as_factor(lvstock$animal)

# remove white space
lvstock$animal <- gsub(" ", "-", lvstock$animal)

# count the number of animals of each class a household owns
lvstock_x <- select(lvstock, y2_hhid, animal, indigQty, improvBeefQty, improvDairyQty) %>%
  melt(id = c("y2_hhid", "animal")) %>%
  group_by(y2_hhid, animal) %>%
  mutate(class = ifelse(animal %in% LR, "LR",
                        ifelse(animal %in% SR, "SR", 
                               ifelse(animal %in% PIGS, "PIGS_",
                                      ifelse(animal %in% POULTRY, "POULTRY",
                                             ifelse(animal %in% OTHER, "OTHER_")))))) %>%
  group_by(y2_hhid, class) %>%
  summarise(n=sum(value, na.rm=TRUE)) %>%
  dcast(y2_hhid ~ class)

# count the number of each animal a household owns
lvstock_y <- select(lvstock, y2_hhid, animal, indigQty, improvBeefQty, improvDairyQty) %>%
  melt(id = c("y2_hhid", "animal")) %>%
  group_by(y2_hhid, animal) %>%
  summarise(n=sum(value, na.rm=TRUE)) %>%
  dcast(y2_hhid ~ animal)

# join together
lvstock <- left_join(lvstock_x, lvstock_y)

rm("LR", "SR", "lvstock_x", "lvstock_y", "OTHER", "PIGS", "POULTRY")

#######################################
########## TRANSPORT COSTS ############
#######################################

tc <- read_dta(file.path(dataPath, "TZNPS2AGRDTA/AG_SEC5a.dta")) %>% 
  dplyr::select(y2_hhid, crop_code=zaocode, trans=ag5a_15, trans_dist=ag5a_16, trans_cost=ag5a_19)

#######################################
############ PANEL KEY ################
#######################################

# key for joing individuals and households across years
key <- read_dta(file.path(dataPath, "../../2012/Data/NPSY3.PANEL.KEY.dta")) %>%
  select(-UPI3) %>% rename(hhid2008 = y1_hhid, hhid2012=y3_hhid)

bad <- is.na(as.numeric(key$hhid2008))
key$hhid2008[bad] <- NA

bad <- is.na(as.numeric(key$y2_hhid))
key$y2_hhid[bad] <- NA

key$hhid2012 <- zap_empty(key$hhid2012)

#######################################
########### CROSS SECTION #############
#######################################

# joins at the household level (y2_hhid)

TZA2010 <- left_join(location, HH10); rm(location); rm(HH10)
TZA2010 <- left_join(TZA2010, key); rm(key)
TZA2010 <- left_join(TZA2010, implmt); rm(implmt)
TZA2010 <- left_join(TZA2010, areaTotal); rm(areaTotal)
TZA2010 <- left_join(TZA2010, lvstock); rm(lvstock)

# joins at the plot level

TZA2010 <- left_join(TZA2010, geo10); rm(geo10)
TZA2010 <- left_join(TZA2010, plot); rm(plot)
TZA2010 <- left_join(TZA2010, oput); rm(oput)
TZA2010 <- left_join(TZA2010, tc); rm(tc)
TZA2010 <- left_join(TZA2010, lab); rm(lab)
TZA2010 <- left_join(TZA2010, areas); rm(areas)

TZA2010 <- rename(TZA2010, hhid2010=y2_hhid)
TZA2010 <- mutate(TZA2010, surveyyear=2010)

rm("bad", "dataPath")
