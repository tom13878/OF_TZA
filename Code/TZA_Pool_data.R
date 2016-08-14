############################
#### PREPARE ETH PANEL #####
############################

############################################
############## READ THE DATA ###############
############################################

source("D:\\Data\\Projects\\ETHYG\\Code\\ETH_2011.r")
source("D:\\Data\\Projects\\ETHYG\\Code\\ETH_2013.r")

#######################################
############## PACKAGES ETC ###########
#######################################

dataPath <- "N:/Internationaal Beleid  (IB)/Projecten/2285000066 Africa Maize Yield Gap/SurveyData/"
wdPath <- "D:\\Data\\Projects\\ETHYG"
setwd(wdPath)

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

# in the second wave, the household
# identification number of the first
# wave is used. However these are recorded
# as an empty character string if the 
# household entered the survey in wave
# two. ("")

table(ETH2013$household_id %in% "")
ETH2013$household_id <- zap_empty(ETH2013$household_id)
table(is.na(ETH2013$household_id))

# the same is true of the individual id
table(ETH2013$individual_id %in% "")
ETH2013$individual_id <- zap_empty(ETH2013$individual_id )
table(is.na(ETH2013$individual_id))

# and the ea_id
table(ETH2013$ea_id %in% "")
ETH2013$ea_id <- zap_empty(ETH2013$ea_id)
table(is.na(ETH2013$ea_id))

# use the first wave household identification
# number. Where this is missing use the
# second wave household identification number
ETH2013$household_id <- ifelse(is.na(ETH2013$household_id), ETH2013$household_id2, ETH2013$household_id)
ETH2013$individual_id <- ifelse(is.na(ETH2013$individual_id), ETH2013$individual_id2, ETH2013$individual_id)
ETH2013$ea_id <- ifelse(is.na(ETH2013$ea_id), ETH2013$ea_id2, ETH2013$ea_id)

# -------------------------------------
# Some waves of the data have variables
# that were not available in others.
# -------------------------------------

# get all name variables that are common to both waves
good <- Reduce(intersect, list(names(ETH2011), names(ETH2013)))

# select only those names common in both waves
ETH2011_2 <- ETH2011[, good]
ETH2013_2 <- ETH2013[, good]

# new full dataset
dbP <- rbind(ETH2011_2,ETH2013_2) %>%
  select(hhid=household_id, indidy=individual_id, everything())

rm(good, ETH2011, ETH2011_2, ETH2013, ETH2013_2)

# Write file
saveRDS(dbP, "Cache/Pooled_ETH.rds")

