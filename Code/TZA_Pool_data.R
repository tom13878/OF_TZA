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
source(file.path(filePath, "../pol/code/pol20104analysis.R"))
prez2010 <- rename(prez2010, REGNAME_POL=reg, DISNAME_POL=dis)
polLink <- read.csv(file.path(filePath, "../pol/data/link_files/lsms2pol.csv"))
polLink <- rename(polLink, REGNAME=REGNAME_LSMS, DISNAME=DISNAME_LSMS)
polLink$NOTE <- NULL

# Write file
saveRDS(dbP, "Cache/Pooled_TZA.rds")

