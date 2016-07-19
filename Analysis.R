#######################################
###### ANALYSIS of TZA panel data #####
#######################################
######## Optimal Fertilizer  ##########
#######################################

# file path decision: Michiel or Tom
if(Sys.info()["user"] == "Tomas"){
  filePath <- "C:/Users/Tomas/Documents/LEI/pro-gap/"
} else {
  filePath <- "N:/Internationaal Beleid  (IB)/Projecten/2285000066 Africa Maize Yield Gap/SurveyData/Code"
}

# comment to test

#######################################
############## READ DATA ##############
#######################################

detach(package:dplyr)
source(file.path(filePath, "/TZA/TZA_2010PP.r"))
source(file.path(filePath, "/TZA/TZA_2012PP.r"))

#######################################
############## PACKAGES ETC ###########
#######################################

dataPath <- "N:/Internationaal Beleid  (IB)/Projecten/2285000066 Africa Maize Yield Gap/SurveyData/"
#surveyPath <- "W:\\LEI\\Internationaal Beleid  (IB)\\Projecten\\2285000066 Africa Maize Yield Gap\\SurveyData\\"
wdPath <- "D:\\Dropbox\\Michiel_research\\2285000066 Africa Maize Yield Gap"
setwd(wdPath)

library(plyr)
library(dplyr)
library(ggplot2)
library(stargazer)
library(haven)
library(tidyr)
library(xtable)
library(sandwich)
library(lmtest)

source("Analysis/TZA/Code/winsor.R")
source("Analysis/TZA/Code/waterfall_plot.R")

options(scipen=999)


#######################################
###### POOLED DATABASE ################
#######################################

# get all name variables that are common to the three waves
good <- Reduce(intersect, list(names(TZA2010), names(TZA2012)))

# select only those names common in both waves
TZA2010_2 <- TZA2010[, good]
TZA2012_2 <- TZA2012[, good]

# new full dataset
dbP <- rbind(TZA2010_2, TZA2012_2) %>%
  dplyr::select(hhid2010, indidy2, hhid2012, indidy3, everything())


#######################################
############## CLEANING ###############
#######################################

# Select maize
dbP <- filter(dbP, crop_code == 11 & status == "HEAD"); #rm(TZA2010, TZA2012, TZA2010_2, TZA2012_2)

# Create rel_harv_area variable
dbP$area_farmer[dbP$area_farmer %in% 0] <- NA
dbP$harv_area[dbP$harv_area %in% 0] <- NA

dbP <- dbP %>% 
  mutate( sh_harv_area = harv_area/area_farmer,
          sh_harv_area = ifelse(sh_harv_area >1, 1, sh_harv_area), # for some farmers the harvested area > plot size. Set to 1
          rel_harv_area = sh_harv_area * area_gps)
         

# Create id for plots
dbP <- dbP %>% 
  mutate(id=1:dim(.)[1]) 


# Cleaning and analysis depends strongly on which measure is chosen for area, which is the denominator for many variables.
# there are three possible yield variables. 
# that can be created for the last two waves of data. 
# 1. yld1: above uses the full gps areas as denominator
# 2. yld2: uses harvested area as denominator
# 3. yld3: Uses relative harvest area to correct gps area
# To simplify the code we set these values in this part. Subsequent analysis code can then be used for any definition of yield.

# We choose rel_harv_area (yld3)
dbP <- dbP %>% 
  mutate(area = area_gps, 
         yld = crop_qty_harv/area,
         N = N/area)
         

# Remove Zanzibar
dbP <- filter(dbP, !(ZONE %in% c("ZANZIBAR")))

# cap yield at 16040 kg/ha, the highest potential yield in TZA (not water limited)
dbP <- filter(dbP, yld <=16040)

# Sample includes very small plots <0.005 ha that bias result upwards and very large >35 ha. 
# As we focus on small scale farmers we restrict area size
dbP <- filter(dbP, area_gps >=0.05 & area_gps <=10 & area_tot <= 10)

# restrict attention to plots that use N < 1000. There large outliers that are removed.
dbP <- dplyr::filter(dbP, N < 1000)

# Remove hhid that have split and moved. We keep households that have split and/or moved within the village as we expect them to own and work the same plots

# Construct a key that links different years
# Potential problem is the splitting of hh over time.
# To investigate this furter we add information on which hh are split off and which are original y2 hh.
# 
# # Input files
# key1 <- read_dta(file.path(dataPath, "\\2012\\Data\\NPSY3.PANEL.KEY.dta")) %>% rename(hhid2012=y3_hhid, hhid2010=y2_hhid)
# key2 <- read_dta(file.path(dataPath, "\\2012\\Data\\HH_SEC_A.dta")) %>%
#    dplyr::select(hhid2012=y3_hhid, hhtype = hh_a10, hhloc = hh_a11) %>%
#    mutate(hhtype = factor(hhtype, levels = c(1 ,2), labels = c("Original", "Split-off")),
#           hhloc = factor(hhloc, levels = c(1,2,3), labels = c("In same location", "Local tracking", "Distance tracking")))
#  
# # Create key
# key1$hhid2010 <- zap_empty(key1$hhid2010)
# key1$hhid2012 <- zap_empty(key1$hhid2012)
# # 
# key <- dplyr::select(key1, hhid2010, hhid2012) %>%
#    na.omit() %>%
#    unique() %>%
#    left_join(., key2) %>%
#    arrange(hhid2012)
#  
# # Remove hh that have merged between y2 and y3
# dupl2012 <- dplyr::select(key, hhid2012) %>% do(filter(., duplicated(.)))
# key <- key[!(key$hhid2012 %in% dupl2012$hhid2012),]
# saveRDS(key, file = "Analysis\\TZA\\Data\\key.rds")

key <- readRDS("Analysis\\TZA\\Data\\key.rds") %>% filter(hhloc != "Distance tracking")

good <- dbP$hhid2012 %in% key$hhid2012
dbP <- dbP[good,]

# Make sure that all plots have a hhid number 
dbP$hhid <- ifelse(is.na(dbP$hhid2012), dbP$hhid2010, dbP$hhid2012)


######################################
##### Get plot specific pricess ######
######################################

# Winsor prices
PPrices <- dbP %>%
            select(ZONE, REGNAME, DISCODE, surveyyear, hhid, plotnum, id, crop_price, WPn, WPnnosub, WPnsub) %>%
            mutate(Pm_P = winsor2(crop_price),
                   Pn_P = winsor2(WPn),
                   Pnns_P = winsor2(WPnnosub),
                   Pns_P = winsor2(WPnsub)) %>%
            select(-crop_price, -WPn, -WPnnosub, -WPnsub)
  
# Load and merge price data averaged at district, region, zone and country
Prices <- readRDS("./Analysis/TZA/Data/Prices.rds")
Prices <- Prices %>% group_by(type) %>%
  dplyr::select(ZONE, REGNAME, DISCODE, surveyyear, price, type) %>%
  spread(type, price) %>% 
  mutate(surveyyear = as.numeric(as.character(surveyyear)))

# We take Pn as the price for fertilizer. Pn is the average of subsidised and non subsidised prices
# If a farmer only uses subsidised fertilizer, Pn is the subsidised prices and similar for marker prices.

# Merge with panel data
PPrices <- left_join(PPrices, Prices) %>%
            mutate(Pm = ifelse(is.na(Pm_P), Pm, Pm_P),
                   Pn = ifelse(is.na(Pn_P), Pn, Pn_P)) %>%
            select(-Pm_P, -Pn_P, -Pns_P, -Pnns_P, -Pns, -Pnns)


######################################
######## Modify and add variables ####
######################################

db0 <- dbP

# Select relevant variables and complete cases
db0 <- db0 %>% 
  dplyr::select(hhid, id, ZONE, REGCODE, REGNAME, DISCODE, plotnum,
                yld, N, P, area, area_tot, AEZ, asset, lab, hybrd, manure, pest, soil, legume, irrig, 
                sex, age, 
                crop_count, surveyyear, 
                SOC, SOC2, ph, ph2, RootDepth, 
                hh_slope, hh_elevation, nut, nrc, fs,
                rain_year, rain_wq, 
                rural, area_gps, lat, lon) %>%
  do(filter(., complete.cases(.)))

# Following Burke
db0$phdum[db0$ph < 55] <- 1
db0$phdum[db0$ph >= 55 & db0$ph <=70] <- 2 # Neutral and best suited for crops
db0$phdum[db0$ph > 70] <- 3
db0$phdum <- factor(db0$phdum)


db0$phdum2[db0$ph2 < 55] <- 1
db0$phdum2[db0$ph2 >= 55 & db0$ph2 <=70] <- 2
db0$phdum2[db0$ph2 > 70] <- 3
db0$phdum2 <- factor(db0$phdum2)

# Recode AEZ into 3 zones
db0$AEZ2 <- db0$AEZ
db0$AEZ2 <- mapvalues(db0$AEZ2, from = c("Tropic - cool / humid"), to = c("Tropic - cool / subhumid"))
db0$AEZ2 <- mapvalues(db0$AEZ2, from = c("Tropic - warm / humid"), to = c("Tropic - warm / subhumid"))
db0$AEZ2 <- mapvalues(db0$AEZ2, from = c("Tropic - cool / semiarid"), to = c("Semi-arid"))
db0$AEZ2 <- mapvalues(db0$AEZ2, from = c("Tropic - warm / semiarid"), to = c("Semi-arid"))
db0$AEZ2 <- factor(db0$AEZ2)


# Crop count > 1
db0$crop_count2[db0$crop_count==1] <- 1
db0$crop_count2[db0$crop_count>1] <- 0


# -------------------------------------
# Inflate 2011 prices to 2013 prices: assets
# using inflation rate for 2011 and 2013. These years were selected as the main part of the survey takes place in these years.
# from world bank:
# http://data.worldbank.org/indicator/FP.CPI.TOTL.ZG/countries/TZ?display=graph
# -------------------------------------

inflation <- read.csv(file.path(paste0(dataPath,"Other/Inflation/inflation.csv")))
rate2011 <- inflation$inflation[inflation$code=="TZ" & inflation$year==2011]/100
rate2012 <- inflation$inflation[inflation$code=="TZ" & inflation$year==2012]/100

inflate <- (1 + rate2011)*(1 + rate2012)

db0 <- mutate(db0, asset = ifelse(surveyyear == 2010, asset*inflate, asset))

# additional variables
db0 <- db0 %>% mutate (yesN = ifelse(N>0, 1,0), # Dummy when plot does not use fertilizer, following approach of Battese (1997)
                       noN = ifelse(N<=0, 1,0), # Dummy when plot does not use fertilizer, following approach of Battese (1997)
                       N2 = N*N,
                       P = P/area,
                       P2 = P*P,
                       lab=lab/area,
                       assetph=asset/area_tot,
                       logasset = log(assetph),
                       rain_wq2 = rain_wq*rain_wq,
                       rain_year2 = rain_year*rain_year,
                       surveyyear2 = replace(surveyyear==2010, 1, 0),
                       nut = factor(nut, levels = c(0, 1, 2, 3, 7), labels = c("ocean", "no or slight constraint", "moderate constraint", "severe constraint", "water"))
)



# drop unused levels
db0 <- droplevels(db0)

# add separate dummy for soil instead of factor for CRE
db0 <- cbind(db0, model.matrix( ~ soil - 1, data=db0)) 



# Add CRE variables
db0 <- ddply(db0, .(hhid), transform,
             lab_bar=mean(lab, na.rm=TRUE),
             N_bar=mean(N, na.rm=TRUE),
             P_bar=mean(P, na.rm=TRUE),
             noN_bar=mean(noN, na.rm=TRUE),
             logasset_bar = mean(logasset, na.rm=TRUE),
             area_bar=mean(area, na.rm=TRUE),
             manure_bar=mean(manure, na.rm=TRUE),
             irrig_bar=mean(irrig, na.rm = TRUE),
             pest_bar=mean(pest, na.rm = TRUE),
             hybrd_bar=mean(hybrd, na.rm = TRUE),
             legume_bar=mean(legume, na.rm=TRUE),
             crop_count_bar=mean(crop_count2, na.rm=TRUE),
             soilSandy_bar=mean(soilSandy, na.rm=TRUE),
             soilLoam_bar=mean(soilLoam, na.rm=TRUE),
             soilClay_bar=mean(soilClay, na.rm=TRUE),
             soilOther_bar=mean(soilOther, na.rm=TRUE))


# Drop unused levels (e.g. Zanzibar in zone), which are giving problems with sfa
db0 <- droplevels(db0)

#######################################
############## ANALYSIS ###############
#######################################


# Yield response function estimation
# NB it appears that there is only limited variation between SOC2 and phdum2=1 and AEZ2 so phdum21 drops out.
# Cannot introduce too many interaction terms with N because of limited N application and variability.
# N is only used in several regions. We run regressions for the whole country and regions where fertilizer is used (see Nuse above).


# Full sample
# Interaction terms for soil constraints, P values and others
OLS0 <- lm(yld ~ noN + N + N2 + P + P2 + logasset + lab + area + 
             hybrd + manure + pest + legume +
             soil + 
             SOC2 + phdum2 + nut + AEZ2 + fs +
             sex + age + 
             crop_count + surveyyear2 + rural + 
             rain_wq + 
             REGNAME +
             noN_bar + N_bar + logasset_bar +  lab_bar + area_bar + 
             hybrd_bar + manure_bar + pest_bar +  legume_bar +
             soilSandy_bar + soilLoam_bar + soilClay_bar + soilOther_bar +
             crop_count_bar
           ,data = db0)

# http://jakeruss.com/cheatsheets/stargazer.html
# Robust - white - sandwhich se.
#coeftest(OLS1, df = Inf, vcov = vcovHC(OLS1, type = "HC1"))
cov0        <- vcovHC(OLS0, type = "HC1")
robust.se0   <- sqrt(diag(cov0))

stargazer(OLS0, type = "text", 
          se = list(robust.se0), intercept.bottom = FALSE, digits = 2, digits.extra = 2)


OLS1 <- lm(yld ~ noN + N:AEZ2 + N2:AEZ2 + 
             N:soil + 
             N:SOC2 + N:phdum2 +
             rain_wq +
             sex + age +
             logasset + lab + area + 
             hybrd + manure + pest + legume +
             hh_elevation + hh_slope +
             crop_count + surveyyear2 + rural + 
             REGNAME +
             noN_bar + N_bar + logasset_bar +  lab_bar + area_bar + 
             hybrd_bar + manure_bar + pest_bar +  legume_bar +
             soilSandy_bar + soilLoam_bar + soilClay_bar + soilOther_bar +
             crop_count_bar
             ,data = db0)


cov1        <- vcovHC(OLS1, type = "HC1")
robust.se1   <- sqrt(diag(cov1))


OLS2 <- lm(yld ~ noN + N:AEZ2 + N2:AEZ2 + 
             N:soil + 
             SOC2 + phdum2 +
             #N:P +
             rain_wq + I(rain_wq*rain_wq) +
             logasset + lab + area + 
             hybrd + manure + pest + legume +
             hh_elevation + hh_slope +
             crop_count + surveyyear2 +  
             REGNAME +
             noN_bar + N_bar + logasset_bar +  lab_bar + area_bar + 
             hybrd_bar + manure_bar + pest_bar +  legume_bar +
             soilSandy_bar + soilLoam_bar + soilClay_bar + soilOther_bar +
             crop_count_bar
           ,data = db0)

cov2        <- vcovHC(OLS2, type = "HC1")
robust.se2   <- sqrt(diag(cov2))

# Adjust F statistic 
wald1 <- waldtest(OLS1, cov1)

stargazer(OLS1, OLS2, type = "text", 
          se = list(robust.se1, robust.se2), intercept.bottom = FALSE, digits = 2, digits.extra = 2)

table(db0$rain_wq, db0$AEZ2)
ggplot(data = db0, aes(x=rain_wq, y = yld)) + geom_point() + facet_wrap(~AEZ2)






# Calculate on transport cost of fertilizer per zone/region
# CHECK definition of transport cost
load(file.path(dataPath, "TZA_HH12_data.RData"))

transportcost <- filter(vo, inputname %in% c("UREA", "DAP") & input ==1 & !is.na(inputcost) & !is.na(trancost) & trancost>0) %>%
  group_by(zone, region) %>%
  summarize(transhare = mean(trancost/(inputcost+trancost)*100),
            N=n())

transportcost2 <- filter(vo, inputname %in% c("UREA", "DAP") & input ==1 & !is.na(inputcost) & !is.na(trancost) & trancost>0 ) %>%
  mutate(transhare = trancost/(inputcost+trancost)*100,
         N=n())


# Merge price data
load(file.path(dataPath, "Prices.RData")) 
Prices <- Prices %>% dplyr:: select(zone, region, surveyyear, price, type) %>%
  spread(type, price)

db0.1 <- left_join(db0, Prices)


# Average Marginal Product
model <- OLS1
b1_tc = coef(model)["AEZ2Tropic-cool / sub-humid:N"]
b1_sa = coef(model)["AEZ2Semi-arid:N"]
b1_tw = coef(model)["AEZ2Tropic-warm / sub-humid:N"]

b2_tc = 2*coef(model)["AEZ2Tropic-cool / sub-humid:N2"]
b2_sa = 2*coef(model)["AEZ2Semi-arid:N2"]
b2_tw = 2*coef(model)["AEZ2Tropic-warm / sub-humid:N2"]

#b3_SOC = coef(model)["N:SOC2"]

#b4_phdum22 = coef(model)["N:phdum22"]
#b4_phdum23 = coef(model)["N:phdum23"]

b5_soilLoam = coef(model)["N:soilLoam"]
b5_soilClay = coef(model)["N:soilClay"]
b5_soilOther = coef(model)["N:soilOther"]

# b6_modcons = coef(model)["N:soil05moderate constraint"]
# b6_sevcons = coef(model)["N:soil05severe constraint"]
