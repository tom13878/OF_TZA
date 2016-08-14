#######################################
###### ANALYSIS of TZA price data #####
#######################################

# CHECK
# Median of prices
# Compute community prices!
# Compare prices with other prices and check if they are realistic!


#######################################
############## READ DATA ##############
#######################################

detach(package:dplyr)
source("N:/Internationaal Beleid  (IB)/Projecten/2285000066 Africa Maize Yield Gap/SurveyData/Code/TZA/TZA_2010PP.r")
source("N:/Internationaal Beleid  (IB)/Projecten/2285000066 Africa Maize Yield Gap/SurveyData/Code/TZA/TZA_2012PP.r")


#######################################
############## PACKAGES ETC ###########
#######################################

wdPath <- "D:\\Dropbox\\Michiel_research\\2285000066 Africa Maize Yield Gap\\Analysis\\TZA\\"
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

key_2010 <- dplyr::select(TZA2010, hhid2010, ZONE, REGNAME, DISCODE, surveyyear) %>% unique() %>% do(filter(., complete.cases(.)))

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

key_2012 <- dplyr::select(TZA2012, hhid2012, ZONE, REGNAME, DISCODE, surveyyear) %>% unique() %>% do(filter(., complete.cases(.)))

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
       
# Construct price data.frame
# Construct base dataframe with all zones and regions
base <- dbP %>%
  select(ZONE, REGNAME, DISCODE, surveyyear) %>% 
  distinct() %>%
  filter(!(ZONE %in% c("ZANZIBAR")) & !is.na(ZONE)) 

# Remove all na values
fert <- fert %>% select(ZONE, REGNAME, DISCODE, price, vouchfert, surveyyear) %>%
  do(filter(., complete.cases(.)))

# It appears that the same geo-coordinates have households with different region and district codes, which should be impossible. 
# It also appears that many of these households do not own plots and therefore not contain relevant data.
# Nonetheless, it raised doubt over the geocodes vs region and district as stated in the survey.
# For this reason, we calculate the lowest price level not at community level but at district level and built up to zone and country level when there are missing values. and buCheck whether there are multiple ea_id codes with different regions. 

# Values are winsored and aggregates are presented for at least 5 values
# Mixed (subsidised and non-subsidised prices)
fertmix <- fert %>%
            filter(vouchfert %in% c(0,1)) %>%
            mutate(price = winsor2(price))

pricesCountry <- fertmix %>% 
  group_by(surveyyear) %>%
  dplyr::summarize(
    number = sum(!is.na(price)),
    price = mean(price, na.rm=T)) %>%
  mutate(level = "country")

pricesPerZone <- fertmix %>% 
  group_by(ZONE, surveyyear) %>%
  dplyr::summarize(
    number = sum(!is.na(price)),
    price = mean(price, na.rm=T)) %>%
  filter(number >= 5) %>%
  mutate(level = "zone")

pricesPerRegion <- fertmix %>% 
  group_by(ZONE, REGNAME, surveyyear) %>%
  dplyr::summarize(
    number = sum(!is.na(price)),
    price = mean(price, na.rm=T)) %>%
  filter(number >= 5) %>%
  mutate(level = "region")

pricesRegionInter <- fertmix %>%
  group_by(DISCODE, REGNAME, ZONE, surveyyear) %>%
  dplyr::summarize(
    number = sum(!is.na(price)),
    price = mean(price, na.rm=T)) %>%
  left_join(base, .) %>%
  filter(is.na(price) | number<5) %>%
  dplyr::select(-number, -price) %>%
  left_join(., pricesPerRegion) %>%
  filter(!is.na(price))

pricesZoneInter <- fertmix %>%
  group_by(DISCODE, REGNAME, ZONE, surveyyear) %>%
  dplyr::summarize(
    number = sum(!is.na(price)),
    price = mean(price, na.rm=T)) %>%
  left_join(base, .) %>%
  filter(is.na(price) | number<5) %>%
  dplyr::select(-number, -price) %>%
  left_join(., pricesPerRegion) %>%
  filter(is.na(price)) %>%
  dplyr::select(-number, -price, -level) %>%
  left_join(., pricesPerZone) %>%
  filter(!is.na(price))

pricesCountryInter <- fertmix %>%
  group_by(DISCODE, REGNAME, ZONE, surveyyear) %>%
  dplyr::summarize(
    number = sum(!is.na(price)),
    price = mean(price, na.rm=T)) %>%
  left_join(base, .) %>%
  filter(is.na(price) | number<5) %>%
  dplyr::select(-number, -price) %>%
  left_join(., pricesPerRegion) %>%
  filter(is.na(price)) %>%
  dplyr::select(-number, -price, -level) %>%
  left_join(., pricesPerZone) %>%
  filter(is.na(price)) %>%
  dplyr::select(-number, -price, -level) %>%
  left_join(., pricesCountry)
  
pricesPerDistrictmix <- fertmix %>%
  group_by(DISCODE, REGNAME, ZONE, surveyyear) %>%
  dplyr::summarize(
    number = sum(!is.na(price)),
    price = mean(price, na.rm=T)) %>%
  mutate(level = "district") %>%
  left_join(base, .) %>%
  filter(!is.na(price) & number>=5) %>%
  rbind(., pricesCountryInter, pricesZoneInter,pricesRegionInter) %>%
  ungroup() %>%
  mutate(product = "fertilizer", type = "Pn")

rm(pricesCountry, pricesPerZone, pricesPerRegion, pricesCountryInter, pricesZoneInter, pricesRegionInter)

# market  prices
fertmar <- fert %>%
  filter(vouchfert %in% c(0)) %>%
  mutate(price = winsor2(price))

pricesCountry <- fertmar %>% 
  filter(!is.na(REGNAME)) %>% 
  group_by(surveyyear) %>%
  dplyr::summarize(
    number = sum(!is.na(price)),
    price = mean(price, na.rm=T)) %>%
  mutate(level = "country")

pricesPerZone <- fertmar %>% 
  group_by(ZONE, surveyyear) %>%
  dplyr::summarize(
    number = sum(!is.na(price)),
    price = mean(price, na.rm=T)) %>%
  filter(number >= 5) %>%
  mutate(level = "zone")

pricesPerRegion <- fertmar %>% 
  group_by(ZONE, REGNAME, surveyyear) %>%
  dplyr::summarize(
    number = sum(!is.na(price)),
    price = mean(price, na.rm=T)) %>%
  filter(number >= 5) %>%
  mutate(level = "region")

pricesRegionInter <- fertmar %>%
  group_by(DISCODE, REGNAME, ZONE, surveyyear) %>%
  dplyr::summarize(
    number = sum(!is.na(price)),
    price = mean(price, na.rm=T)) %>%
  left_join(base, .) %>%
  filter(is.na(price) | number<5) %>%
  dplyr::select(-number, -price) %>%
  left_join(., pricesPerRegion) %>%
  filter(!is.na(price))

pricesZoneInter <- fertmar %>%
  group_by(DISCODE, REGNAME, ZONE, surveyyear) %>%
  dplyr::summarize(
    number = sum(!is.na(price)),
    price = mean(price, na.rm=T)) %>%
  left_join(base, .) %>%
  filter(is.na(price) | number<5) %>%
  dplyr::select(-number, -price) %>%
  left_join(., pricesPerRegion) %>%
  filter(is.na(price)) %>%
  dplyr::select(-number, -price, -level) %>%
  left_join(., pricesPerZone) %>%
  filter(!is.na(price))

pricesCountryInter <- fertmar %>%
  group_by(DISCODE, REGNAME, ZONE, surveyyear) %>%
  dplyr::summarize(
    number = sum(!is.na(price)),
    price = mean(price, na.rm=T)) %>%
  left_join(base, .) %>%
  filter(is.na(price) | number<5) %>%
  dplyr::select(-number, -price) %>%
  left_join(., pricesPerRegion) %>%
  filter(is.na(price)) %>%
  dplyr::select(-number, -price, -level) %>%
  left_join(., pricesPerZone) %>%
  filter(is.na(price)) %>%
  dplyr::select(-number, -price, -level) %>%
  left_join(., pricesCountry)

pricesPerDistrictmar <- fertmar %>%
  group_by(DISCODE, REGNAME, ZONE, surveyyear) %>%
  dplyr::summarize(
    number = sum(!is.na(price)),
    price = mean(price, na.rm=T)) %>%
  mutate(level = "district") %>%
  left_join(base, .) %>%
  filter(!is.na(price) & number>=5) %>%
  rbind(., pricesCountryInter, pricesZoneInter,pricesRegionInter) %>%
  ungroup() %>%
  mutate(product = "fertilizer", type = "Pnns")

rm(pricesCountry, pricesPerZone, pricesPerRegion, pricesCountryInter, pricesZoneInter, pricesRegionInter)

# Subsidised prices 
fertsub <- fert %>%
  filter(vouchfert %in% c(1)) %>%
  mutate(price = winsor2(price))

pricesCountry <- fertsub %>% 
  filter(!is.na(REGNAME)) %>% 
  group_by(surveyyear) %>%
  dplyr::summarize(
    number = sum(!is.na(price)),
    price = mean(price, na.rm=T)) %>%
  mutate(level = "country")

pricesPerZone <- fertsub %>% 
  group_by(ZONE, surveyyear) %>%
  dplyr::summarize(
    number = sum(!is.na(price)),
    price = mean(price, na.rm=T)) %>%
  filter(number >= 5) %>%
  mutate(level = "zone")

pricesPerRegion <- fertsub %>% 
  group_by(ZONE, REGNAME, surveyyear) %>%
  dplyr::summarize(
    number = sum(!is.na(price)),
    price = mean(price, na.rm=T)) %>%
  filter(number >= 5) %>%
  mutate(level = "region")

pricesRegionInter <- fertsub %>%
  group_by(DISCODE, REGNAME, ZONE, surveyyear) %>%
  dplyr::summarize(
    number = sum(!is.na(price)),
    price = mean(price, na.rm=T)) %>%
  left_join(base, .) %>%
  filter(is.na(price) | number<5) %>%
  dplyr::select(-number, -price) %>%
  left_join(., pricesPerRegion) %>%
  filter(!is.na(price))

pricesZoneInter <- fertsub %>%
  group_by(DISCODE, REGNAME, ZONE, surveyyear) %>%
  dplyr::summarize(
    number = sum(!is.na(price)),
    price = mean(price, na.rm=T)) %>%
  left_join(base, .) %>%
  filter(is.na(price) | number<5) %>%
  dplyr::select(-number, -price) %>%
  left_join(., pricesPerRegion) %>%
  filter(is.na(price)) %>%
  dplyr::select(-number, -price, -level) %>%
  left_join(., pricesPerZone) %>%
  filter(!is.na(price))

pricesCountryInter <- fertsub %>%
  group_by(DISCODE, REGNAME, ZONE, surveyyear) %>%
  dplyr::summarize(
    number = sum(!is.na(price)),
    price = mean(price, na.rm=T)) %>%
  left_join(base, .) %>%
  filter(is.na(price) | number<5) %>%
  dplyr::select(-number, -price) %>%
  left_join(., pricesPerRegion) %>%
  filter(is.na(price)) %>%
  dplyr::select(-number, -price, -level) %>%
  left_join(., pricesPerZone) %>%
  filter(is.na(price)) %>%
  dplyr::select(-number, -price, -level) %>%
  left_join(., pricesCountry)

pricesPerDistrictsub <- fertsub %>%
  group_by(DISCODE, REGNAME, ZONE, surveyyear) %>%
  dplyr::summarize(
    number = sum(!is.na(price)),
    price = mean(price, na.rm=T)) %>%
  mutate(level = "district") %>%
  left_join(base, .) %>%
  filter(!is.na(price) & number>=5) %>%
  rbind(., pricesCountryInter, pricesZoneInter,pricesRegionInter) %>%
  ungroup() %>%
  mutate(product = "fertilizer", type = "Pns")

rm(pricesCountry, pricesPerZone, pricesPerRegion, pricesCountryInter, pricesZoneInter, pricesRegionInter)

# Maize prices
# Values are winsored and aggregates are presented for at least 5 values and imputed using higher level values.
# Note that for more ea maize prices are available. We use base for fert as we need both pieces of information.

maizePrices <- dbP %>% 
  dplyr::select(ZONE, REGNAME, DISCODE, surveyyear, price = crop_price) %>%
  filter(!(ZONE %in% c("ZANZIBAR")) & !is.na(ZONE)) %>%
  mutate(price = winsor2(price))

pricesCountry <- maizePrices %>% 
  filter(!is.na(REGNAME)) %>% 
  group_by(surveyyear) %>%
  dplyr::summarize(
    number = sum(!is.na(price)),
    price = mean(price, na.rm=T)) %>%
  mutate(level = "country")

pricesPerZone <-maizePrices  %>% 
  group_by(ZONE, surveyyear) %>%
  dplyr::summarize(
    number = sum(!is.na(price)),
    price = mean(price, na.rm=T)) %>%
  filter(number >= 5) %>%
  mutate(level = "zone")

pricesPerRegion <- maizePrices  %>% 
  group_by(ZONE, REGNAME, surveyyear) %>%
  dplyr::summarize(
    number = sum(!is.na(price)),
    price = mean(price, na.rm=T)) %>%
  filter(number >= 5) %>%
  mutate(level = "region")

pricesRegionInter <- maizePrices %>%
  group_by(DISCODE, REGNAME, ZONE, surveyyear) %>%
  dplyr::summarize(
    number = sum(!is.na(price)),
    price = mean(price, na.rm=T)) %>%
  left_join(base, .) %>%
  filter(is.na(price) | number<5) %>%
  dplyr::select(-number, -price) %>%
  left_join(., pricesPerRegion) %>%
  filter(!is.na(price))

pricesZoneInter <- maizePrices  %>%
  group_by(DISCODE, REGNAME, ZONE, surveyyear) %>%
  dplyr::summarize(
    number = sum(!is.na(price)),
    price = mean(price, na.rm=T)) %>%
  left_join(base, .) %>%
  filter(is.na(price) | number<5) %>%
  dplyr::select(-number, -price) %>%
  left_join(., pricesPerRegion) %>%
  filter(is.na(price)) %>%
  dplyr::select(-number, -price, -level) %>%
  left_join(., pricesPerZone) %>%
  filter(!is.na(price))

pricesCountryInter <- maizePrices  %>%
  group_by(DISCODE, REGNAME, ZONE, surveyyear) %>%
  dplyr::summarize(
    number = sum(!is.na(price)),
    price = mean(price, na.rm=T)) %>%
  left_join(base, .) %>%
  filter(is.na(price) | number<5) %>%
  dplyr::select(-number, -price) %>%
  left_join(., pricesPerRegion) %>%
  filter(is.na(price)) %>%
  dplyr::select(-number, -price, -level) %>%
  left_join(., pricesPerZone) %>%
  filter(is.na(price)) %>%
  dplyr::select(-number, -price, -level) %>%
  left_join(., pricesCountry)

maizePricesPerDistrict <- maizePrices %>%
  group_by(DISCODE, REGNAME, ZONE, surveyyear) %>%
  dplyr::summarize(
    number = sum(!is.na(price)),
    price = mean(price, na.rm=T)) %>%
  mutate(level = "district") %>%
  left_join(base, .) %>%
  filter(!is.na(price) & number>=5) %>%
  rbind(., pricesCountryInter, pricesZoneInter,pricesRegionInter) %>%
  ungroup() %>%
  mutate(product = "maize", type = "Pm")

rm(pricesCountry, pricesPerZone, pricesPerRegion, pricesCountryInter, pricesZoneInter, pricesRegionInter)

# combine price data
Prices <- rbind(pricesPerDistrictmar, pricesPerDistrictmix, pricesPerDistrictsub, maizePricesPerDistrict) %>%
          mutate(surveyyear = as.factor(surveyyear)) %>%
          droplevels(.)

#check <- Prices %>% select(-product, - level, -number) %>% spread(type, price)

saveRDS(Prices, file = "Data\\Prices.rds")


ggplot(data = Prices) + geom_boxplot(aes(x = price)) + facet_wrap(~ZONE)
