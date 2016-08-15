# -------------------------------------
# match wards to constituencies with the 
# 2010 legislative and presidential
# results 

library(haven)
library(rgdal)
library(dplyr)

setwd("D:/Data/Projects/OF_TZA/")
LSMSPath <- "N:/Internationaal Beleid  (IB)/Projecten/2285000066 Africa Maize Yield Gap/SurveyData/TZA/"

# read in the link file
ward_con_link2010 <- read.csv("Data/link_files/ward_con_link2010.csv")

# read in the legislative and presidenial results

leg2010 <- read.csv("Data/legislative/leg2010.csv") %>% select(-dis)
prez2010 <- readRDS("Data/prez2010/prez2010.rds") %>% select(-dis)

pol2010 <- left_join(ward_con_link2010, leg2010) %>% left_join(prez2010)

rm(list=ls()[!ls() %in% "pol2010"])

# -------------------------------------
# join political election data with the
# LSMS-ISA survey data
# -------------------------------------

# read in the gps information
gps1 <- read_dta(file.path(LSMSPath, "2010/Data/TZNPS2GEODTA/HH.Geovariables_Y2.dta")) %>%
  select(y2_hhid, longitude=lon_modified, latitude=lat_modified)

gps2 <- read_dta(file.path(LSMSPath, "2012/Data/HouseholdGeovars_Y3.dta")) %>%
  select(y3_hhid, longitude=lon_dd_mod, latitude=lat_dd_mod)

# map from GADM
TZA <- readRDS("Data/GADM_2.7_TZA_adm3.rds")

# -------------------------------------
# Overlay the gps coordinates of each
# household to match up with ward
# information from the 
# -------------------------------------

# make sure projection used in map is
# the same as in the spatial object

llCRS <- CRS(proj4string(TZA))

# 2010

# make a spatial points object
gps_mat <- cbind(gps1$longitude, gps1$latitude)
row.names(gps_mat) <- 1:nrow(gps_mat)
sp <- SpatialPoints(gps_mat, llCRS)

# overlay the map with the points
# and join with the data frame

data2010 <- over(sp, TZA) %>%
  select(reg=NAME_1, dis=NAME_2, ward=NAME_3) 
data2010 <- cbind(gps1$y2_hhid, data2010)
names(data2010) <- c("y2_hhid", "reg", "dis", "ward")

# 2012

# make a spatial points object
gps_mat <- cbind(gps2$longitude, gps2$latitude)
row.names(gps_mat) <- 1:nrow(gps_mat)
sp <- SpatialPoints(gps_mat, llCRS)

# overlay the map with the points
# and join with the data frame

data2012 <- over(sp, TZA) %>%
  select(reg=NAME_1, dis=NAME_2, ward=NAME_3) 
data2012 <- cbind(gps2$y3_hhid, data2012)
names(data2012) <- c("y3_hhid", "reg", "dis", "ward")

# make all names uppercase

data2010$reg <- toupper(data2010$reg); data2012$reg <- toupper(data2012$reg)
data2010$dis <- toupper(data2010$dis); data2012$dis <- toupper(data2012$dis)
data2010$ward <- toupper(data2010$ward); data2012$ward <- toupper(data2012$ward)

# we do not need any observations on the islands
# of Tanzania

islands <- c("KASKAZINI-UNGUJA", "ZANZIBAR SOUTH AND CENTRAL", "ZANZIBAR WEST",             
             "KASKAZINI-PEMBA", "KUSINI-PEMBA")
data2010 <- data2010[!data2010$reg %in% islands, ]
data2012 <- data2012[!data2012$reg %in% islands, ]

# join political data with the survey households
# so that political information can easily be used
# with the rest of the survey

data2010 <- left_join(data2010, pol2010)
data2012 <- left_join(data2012, pol2010)

# save data
saveRDS(data2010, "Cache/polInt2010.rds")
saveRDS(data2012, "Cache/polInt2012.rds")
