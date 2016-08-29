############################
#### PREPARE TZA PANEL #####
############################

############################################
############## READ THE DATA ###############
############################################

if(Sys.info()["user"] == "Tomas"){
  wdPath <- "C:/Users/Tomas/Documents/LEI/OF_TZA/"
  setwd(wdPath)
  surveyPath <- "C:/Users/Tomas/Documents/LEI/data/"
} else {
  wdPath <- "D:/Data/Projects/OF_TZA/"
  setwd(wdPath)
  surveyPath <- "N:/Internationaal Beleid  (IB)/Projecten/2285000066 Africa Maize Yield Gap/SurveyData/"
}

source("Code\\TZA_2010.r")
source("Code\\TZA_2012.r")

#######################################
############## PACKAGES ETC ###########
#######################################

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
  dplyr::select(hhid2010, indidy2, hhid2012, indidy3, everything())

rm(good, TZA2010, TZA2010_2, TZA2012, TZA2012_2)

# Link pol data
source(file.path(wdPath, "/Code/TZA_pol20104analysis.R"))
prez2010 <- rename(prez2010, REGNAME_POL=reg, DISNAME_POL=dis)
polLink <- read.csv(file.path(wdPath, "/data/link_files/lsms2pol.csv"))
polLink <- rename(polLink, REGNAME=REGNAME_LSMS, DISNAME=DISNAME_LSMS)
polLink$NOTE <- NULL

# two steps for joining the political 
# data
# 1. merge the db0 database (LSMS) with the
# link file
dbP <- left_join(dbP, polLink)

# 2. merge the db0 database (LSMS) with
# the political results (from NEC)
dbP <- left_join(dbP, prez2010) %>%
  remove_all_labels()

# Remove hhid that have split and moved. We keep households that have split and/or moved within the village as we expect them to own and work the same plots
# Construct a key that links different years
# Potential problem is the splitting of hh over time.
# To investigate this furter we add information on which hh are split off and which are original y2 hh.

# # Input files
key1 <- read_dta(file.path(surveyPath, "TZA\\2012\\Data\\NPSY3.PANEL.KEY.dta")) %>% rename(hhid2012=y3_hhid, hhid2010=y2_hhid)
key2 <- read_dta(file.path(surveyPath, "TZA\\2012\\Data\\HH_SEC_A.dta")) %>%
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
  arrange(hhid2012) %>%
  remove_all_labels()

# Remove hh that have merged between y2 and y3
dupl2012 <- dplyr::select(key, hhid2012) %>% do(filter(., duplicated(.)))
key <- key[!(key$hhid2012 %in% dupl2012$hhid2012),]
key <- key %>% filter(hhloc != "Distance tracking")

good <- dbP$hhid2012 %in% key$hhid2012
dbP <- dbP[good,]

# Make sure that all plots have a hhid number 
dbP$hhid <- ifelse(is.na(dbP$hhid2012), dbP$hhid2010, dbP$hhid2012)

# Write file
saveRDS(dbP, "Cache/Pooled_TZA.rds")

