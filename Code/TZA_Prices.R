#######################################
###### ANALYSIS of TZA price data #####
#######################################

# CHECK Compare prices with other prices and check if they are realistic!


#######################################
############## READ DATA ##############
#######################################

source("D:/Data/Projects/OF_TZA/Code/TZA_2010.r")
source("D:/Data/Projects/OF_TZA/Code/TZA_2012.r")


#######################################
############## PACKAGES ETC ###########
#######################################

wdPath <- "D:\\Data\\Projects\\OF_TZA"
setwd(wdPath)

surveyPath <- "N:\\Internationaal Beleid  (IB)\\Projecten\\2285000066 Africa Maize Yield Gap\\SurveyData"

library(plyr)
library(dplyr)
library(ggplot2)
library(stargazer)
library(haven)
library(tidyr)
library(xtable)
library(DescTools)
library(sandwich)
library(lmtest)
library(assertive)

options(scipen=999)

# winsor code
source("Code/winsor.R")

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

# Select maize
dbP <- filter(dbP, crop_code == 11 & status == "HEAD"); rm(TZA2010, TZA2012, TZA2010_2, TZA2012_2)

# Make sure that all plots have a hhid number 
dbP$hhid <- ifelse(is.na(dbP$hhid2012), dbP$hhid2010, dbP$hhid2012)

# Data per year
TZA2010 <- filter(dbP, surveyyear == 2010)
TZA2012 <- filter(dbP, surveyyear == 2012)

#######################################
############ PROCESSING ###############
#######################################

# read in the fertilizer data, linkin location data and combine in one file
fert2010_1 <- read_dta(file.path(surveyPath, "TZA\\2010\\Data\\TZNPS2AGRDTA/AG_SEC3A.dta")) %>%
    dplyr::select(hhid2010=y2_hhid, plotnum, typ=ag3a_46, qty=ag3a_47, vouchfert=ag3a_48, valu=ag3a_49, zaocode) %>%
    mutate(surveyyear = 2010)

fert2010_2 <- read_dta(file.path(surveyPath, "TZA\\2010\\Data\\TZNPS2AGRDTA/AG_SEC3A.dta")) %>%
    dplyr::select(hhid2010=y2_hhid, plotnum, typ=ag3a_53, qty=ag3a_54, vouchfert=ag3a_55, valu=ag3a_56, zaocode) %>%
    mutate(surveyyear = 2010)

# key prepared to add info on ZONE, REGION and DISTRICT
key_2010 <- dplyr::select(TZA2010, hhid2010, ZONE, REGNAME, DISNAME, surveyyear) %>% unique() %>% do(filter(., complete.cases(.)))

fert2010 <- rbind(fert2010_1, fert2010_2) 
fert2010[] <- lapply(fert2010, strip_attributes)
fert2010 <- fert2010 %>% 
  mutate(hhid2010 = as.character(hhid2010)) %>%
  left_join(., key_2010) %>%
  dplyr::select(-hhid2010) %>%
  filter(zaocode == 11)

fert2012_1 <- read_dta(file.path(surveyPath, "TZA\\2012\\Data\\AG_SEC_3A.dta")) %>%
   dplyr::select(hhid2012=y3_hhid, plotnum, typ=ag3a_48, qty=ag3a_49, vouchfert=ag3a_50, valu=ag3a_51, zaocode = ag3a_07_2) %>%
   mutate(surveyyear = 2012)

fert2012_2 <- read_dta(file.path(surveyPath, "TZA\\2012\\Data\\AG_SEC_3A.dta")) %>%
   dplyr::select(hhid2012=y3_hhid, plotnum, typ=ag3a_55, qty=ag3a_56, vouchfert=ag3a_57, valu=ag3a_58, zaocode = ag3a_07_2) %>%
   mutate(surveyyear = 2012)

key_2012 <- dplyr::select(TZA2012, hhid2012, ZONE, REGNAME, DISNAME, surveyyear) %>% unique() %>% do(filter(., complete.cases(.)))

fert2012 <- rbind(fert2012_1, fert2012_2) 
fert2012[] <- lapply(fert2012, strip_attributes)
fert2012 <- fert2012 %>% 
  mutate(hhid2012 = as.character(hhid2012)) %>%
  left_join(., key_2012) %>%
  dplyr::select(-hhid2012) %>%
  filter(zaocode == 11)

fert <- rbind(fert2010, fert2012) %>% 
                    mutate(typ = factor(typ, levels = c(1, 2, 3, 4, 5, 6, 7), labels = c("dap", "urea", "tsp", "can", "sa", "npk", "mrp")), 
                          vouchfert = ifelse(vouchfert %in% 2, 0, vouchfert))

typ <- factor(levels(fert$typ), levels=levels(fert$typ))
n <- c(0.18, 0.46, NA, 0.26, 0.21, 0.17, NA)
p <- c(0.2, NA, 0.2056, NA, NA, 0.07412, 0.124696)
k <- c(NA, NA, NA, NA, NA, 0.1411, NA)
comp <- data.frame(typ, n, p, k)

fert <- left_join(fert, comp)
rm(list=c("comp", "typ", "n", "p", "k"))

fert <- mutate(fert,
               Vfert=valu/qty,
               Qn=qty*n,
               Qp=qty*p,
               price=Vfert/n) 

table(fert$typ)

# Construct price data.frame
# construct base dataframe with all zones, regions and districts

base <- dbP %>% 
  dplyr::select(ZONE, REGNAME, DISNAME, surveyyear) %>%
  unique() %>%
  na.omit

# Values are winsored by surveyyear, aggregates are presented for at least 5 values

# Remove all na values
fert <- fert %>% select(ZONE, REGNAME, DISNAME, price, vouchfert, surveyyear) %>%
  do(filter(., complete.cases(.)))


medianPrice_f <- function(df, level, group, type){
  prices <- df %>% 
    group_by_(.dots = c(group)) %>%
    dplyr::summarize(
      number = sum(!is.na(price)),
      price = median(price, na.rm=T)) %>%
    filter(number>=5) %>%
    mutate(level = level) %>%
    select(-number) 
  #prices <- setNames(prices, c(group, "price", "level")) 
  out <- left_join(base, prices) %>%
    mutate(type = type)
  return(out)
}


# market  prices
fertmar <- fert %>%
  filter(vouchfert %in% c(0)) %>%
  group_by(surveyyear) %>%
  mutate(price = winsor2(price))

fpCountry <- fertmar %>%
  group_by(surveyyear) %>%
  dplyr::summarize(price = median(price, na.rm=T)) %>%
  mutate(level = "country") 
fpCountry <- mutate(base, price = fpCountry$price,
                    level = "country",
                    type = "Pnm")

fpZone <- medianPrice_f(fertmar, "zone", c("surveyyear", "ZONE"), "Pnm")
fpRegion <- medianPrice_f(fertmar, "region", c("surveyyear", "ZONE", "REGNAME"), "Pnm")
fpDistrict <- medianPrice_f(fertmar, "district", c("surveyyear", "ZONE", "REGNAME", "DISNAME"), "Pnm")

fertMarPrice <- bind_rows(fpDistrict, fpRegion, fpZone, fpCountry) %>%
  na.omit %>%
  spread(level, price) %>%
  mutate(regPrice = ifelse(!is.na(district), district, 
                        ifelse(!is.na(region), region,
                               ifelse(!is.na(zone), zone, country))),
         source = ifelse(!is.na(district), "district", 
                         ifelse(!is.na(region), "region",
                                ifelse(!is.na(zone), "zone", "country"))),
         product = "fertilizer") %>%
  select(-country, -zone, -region, -district)

rm(fpCountry, fpZone, fpRegion, fpDistrict, fertmar)

# Market and subsidised prices
fertmix <- fert %>%
  filter(vouchfert %in% c(0,1)) %>%
  group_by(surveyyear) %>%
  mutate(price = winsor2(price))

fpCountry <- fertmix %>%
  group_by(surveyyear) %>%
  dplyr::summarize(price = median(price, na.rm=T)) %>%
  mutate(level = "country") 
fpCountry <- mutate(base, price = fpCountry$price,
                    level = "country",
                    type = "Pn")

fpZone <- medianPrice_f(fertmix, "zone", c("surveyyear", "ZONE"), "Pn")
fpRegion <- medianPrice_f(fertmix, "region", c("surveyyear", "ZONE", "REGNAME"), "Pn")
fpDistrict <- medianPrice_f(fertmix, "district", c("surveyyear", "ZONE", "REGNAME", "DISNAME"), "Pn")

fertMixPrice <- bind_rows(fpDistrict, fpRegion, fpZone, fpCountry) %>%
  na.omit %>%
  spread(level, price) %>%
  mutate(regPrice = ifelse(!is.na(district), district, 
                        ifelse(!is.na(region), region,
                               ifelse(!is.na(zone), zone, country))),
         source = ifelse(!is.na(district), "district", 
                         ifelse(!is.na(region), "region",
                                ifelse(!is.na(zone), "zone", "country"))),
         product = "fertilizer") %>%
  select(-country, -zone, -region, -district)

rm(fpCountry, fpZone, fpRegion, fpDistrict, fertmix)

# Subsidised prices
fertsub <- fert %>%
  filter(vouchfert %in% c(1)) %>%
  group_by(surveyyear) %>%
  mutate(price = winsor2(price))

fpCountry <- fertsub %>%
  group_by(surveyyear) %>%
  dplyr::summarize(price = median(price, na.rm=T)) %>%
  mutate(level = "country") 
fpCountry <- mutate(base, price = fpCountry$price,
                    level = "country",
                    type = "Pns")

fpZone <- medianPrice_f(fertsub, "zone", c("surveyyear", "ZONE"), "Pns")
fpRegion <- medianPrice_f(fertsub, "region", c("surveyyear", "ZONE", "REGNAME"), "Pns")
fpDistrict <- medianPrice_f(fertsub, "district", c("surveyyear", "ZONE", "REGNAME", "DISNAME"), "Pns")

fertSubPrice <- bind_rows(fpDistrict, fpRegion, fpZone, fpCountry) %>%
  na.omit %>%
  spread(level, price) %>%
  mutate(regPrice = ifelse(!is.na(district), district, 
                        ifelse(!is.na(region), region,
                               ifelse(!is.na(zone), zone, country))),
         source = ifelse(!is.na(district), "district", 
                         ifelse(!is.na(region), "region",
                                ifelse(!is.na(zone), "zone", "country"))),
         product = "fertilizer") %>%
  select(-country, -zone, -region, -district)

rm(fpCountry, fpZone, fpRegion, fpDistrict, fertsub)

# Maize prices
maize <- dbP %>% 
  dplyr::select(ZONE, REGNAME, DISNAME, surveyyear, price = crop_price) %>%
  mutate(price = winsor2(price))

fpCountry <- maize %>%
  group_by(surveyyear) %>%
  dplyr::summarize(price = median(price, na.rm=T)) %>%
  mutate(level = "country")
fpCountry <- mutate(base, price = fpCountry$price,
                    level = "country",
                    type = "Pc") 

fpZone <- medianPrice_f(maize, "zone", c("surveyyear", "ZONE"), "Pc")
fpRegion <- medianPrice_f(maize, "region", c("surveyyear", "ZONE", "REGNAME"), "Pc")
fpDistrict <- medianPrice_f(maize, "district", c("surveyyear", "ZONE", "REGNAME", "DISNAME"), "Pc")

maizePrice <- bind_rows(fpDistrict, fpRegion, fpZone, fpCountry) %>%
  na.omit %>%
  spread(level, price) %>%
  mutate(regPrice = ifelse(!is.na(district), district, 
                        ifelse(!is.na(region), region,
                               ifelse(!is.na(zone), zone, country))),
         source = ifelse(!is.na(district), "district", 
                         ifelse(!is.na(region), "region",
                                ifelse(!is.na(zone), "zone", "country"))),
         product = "maize") %>%
  select(-country, -zone, -region, -district)

rm(fpCountry, fpZone, fpRegion, fpDistrict, fertsub)

# Combine fert price data files 
regPrice <- bind_rows(fertMixPrice, fertMarPrice, fertSubPrice, maizePrice) %>% ungroup
  

# Create price file at plot level.
# Again, we winsor the prices for each type of price and per surveyyear
plotPrice <- select(dbP, hhid, plotnum, ZONE, REGNAME, DISNAME, surveyyear, Pn = WPn, Pns = WPnsub, Pnm = WPnnosub, Pc = crop_price) %>%
  gather(type, plotPrice, Pn, Pns, Pnm, Pc) %>%
  group_by(type, surveyyear) %>%
  mutate(plotPrice =winsor2(plotPrice)) %>%
  ungroup() %>% unique


# Substitute regional prices when plot level price is not available
price <- left_join(plotPrice, regPrice) %>%
          mutate(price = ifelse(is.na(plotPrice), regPrice, plotPrice)) %>%
          unique() %>% # should not be necessary but never know
          select(-source, -regPrice, -plotPrice, -product) %>%
          spread(type, price)


# Plot
ggplot(data = as.data.frame(price), aes(x = surveyyear, y = Pns)) + geom_boxplot() + facet_wrap(~ZONE)
summary(price)

# save data
saveRDS(price, "Cache/TZA_prices.rds")


