############################
#### PREPARE TZA PANEL #####
############################

############################################
############## READ THE DATA ###############
############################################

wdPath <- "D:/Data/Projects/OF_TZA/"
setwd(wdPath)

source("Code\\TZA_2010.r")
source("Code\\TZA_2012.r")

#######################################
############## PACKAGES ETC ###########
#######################################


dataPath <- "N:/Internationaal Beleid  (IB)/Projecten/2285000066 Africa Maize Yield Gap/SurveyData/"

library(plyr)
library(dplyr)
library(ggplot2)
library(stargazer)
library(haven)
library(tidyr)
library(xtable)
library(sjmisc)

options(scipen=999)

###########################
###### POOL DATABASE ######
###########################

# get all name variables that are common to the three waves
good <- Reduce(intersect, list(names(TZA2010), names(TZA2012)))

# select only those names common in both waves
TZA2010_2 <- TZA2010[, good]
TZA2012_2 <- TZA2012[, good]

# new full dataset
dbP <- rbind(TZA2010_2, TZA2012_2) %>%
  dplyr::select(hhid2010, indidy2, hhid2012, indidy3, everything()) %>%
  remove_all_labels()

#rm(good, TZA2010, TZA2010_2, TZA2012, TZA2012_2)


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
  mutate(area = rel_harv_area, 
         yld = crop_qty_harv/area,
         N = N/area)


# Remove Zanzibar
dbP <- filter(dbP, !(ZONE %in% c("ZANZIBAR")))

# cap yield at 16040 kg/ha, the highest potential yield in TZA (not water limited)
dbP <- filter(dbP, yld <=16040)

# Sample includes very small plots <0.005 ha that bias result upwards and very large >35 ha. 
# As we focus on small scale farmers we restrict area size
dbP <- filter(dbP, area_gps >=0.05 & area_gps <=10)

# restrict attention to plots that use N < 1000. There large outliers that are removed.
# Also removes >1400 NA values
dbP <- dplyr::filter(dbP, N < 1000)

# Remove hhid that have split and moved. We keep households that have split and/or moved within the village as we expect them to own and work the same plots

# Construct a key that links different years
# Potential problem is the splitting of hh over time.
# To investigate this furter we add information on which hh are split off and which are original y2 hh.
# 
# # Input files
key1 <- read_dta(file.path(dataPath, "TZA\\2012\\Data\\NPSY3.PANEL.KEY.dta")) %>% 
  dplyr::rename(hhid2012=y3_hhid, hhid2010=y2_hhid)
key2 <- read_dta(file.path(dataPath, "TZA\\2012\\Data\\HH_SEC_A.dta")) %>%
  dplyr::select(hhid2012=y3_hhid, hhtype = hh_a10, hhloc = hh_a11) %>%
  mutate(hhtype = factor(hhtype, levels = c(1 ,2), labels = c("Original", "Split-off")),
         hhloc = factor(hhloc, levels = c(1,2,3), labels = c("In same location", "Local tracking", "Distance tracking")))

# Create key
key1$hhid2010 <- zap_empty(key1$hhid2010)
key1$hhid2012 <- zap_empty(key1$hhid2012)

key <- dplyr::select(key1, hhid2010, hhid2012) %>%
  na.omit() %>%
  unique() %>%
  left_join(., key2) %>%
  arrange(hhid2012)

# Remove hh that have merged between y2 and y3
dupl2012 <- dplyr::select(key, hhid2012) %>% do(filter(., duplicated(.)))
key <- key[!(key$hhid2012 %in% dupl2012$hhid2012),]
key <- key %>% filter(hhloc != "Distance tracking")

good <- dbP$hhid2012 %in% key$hhid2012
dbP <- dbP[good,]

# Make sure that all plots have a hhid number 
dbP$hhid <- ifelse(is.na(dbP$hhid2012), dbP$hhid2010, dbP$hhid2012)

# # Link pol data
# source(file.path(filePath, "../pol/code/pol20104analysis.R"))
# prez2010 <- rename(prez2010, REGNAME_POL=reg, DISNAME_POL=dis)
# polLink <- read.csv(file.path(filePath, "../pol/data/link_files/lsms2pol.csv"))
# polLink <- rename(polLink, REGNAME=REGNAME_LSMS, DISNAME=DISNAME_LSMS)
# polLink$NOTE <- NULL

# Write file
saveRDS(dbP, "Cache/Pooled_TZA.rds")

