# PROJECT: IPOP/CIMMYT/DFID
# ``````````````````````````````````````````````````````````````````````````````````````````````````
# ``````````````````````````````````````````````````````````````````````````````````````````````````
# R code to link spatial data with LSMS-ISA TZA Household data
# ``````````````````````````````````````````````````````````````````````````````````````````````````
# `````````````````````````````````````````````````````````````````````````````````````````````````` 

# INSTALL PACKAGES AND SET WORKING DIRECTORY
BasePackages <- c("foreign", "stringr", "gdata", "car", "reshape2", "RColorBrewer", "plyr", "dplyr", "tidyr", "haven")
lapply(BasePackages, library, character.only = TRUE)
SpatialPackages <- c("rgdal", "gdalUtils", "ggmap", "raster", "rasterVis", "rgeos", "sp", "mapproj", "maptools", "proj4")
lapply(SpatialPackages, library, character.only = TRUE)
AdditionalPackages <- c("GSIF", "SPEI", "haven", "assertive", "countrycode")
lapply(AdditionalPackages, library, character.only = TRUE)

# OPTIONS
options(scipen = 999) # surpress scientific notation
options("stringsAsFactors" = FALSE) 
options(digits = 4)


# SET WORKING DIRECTORY
dataPath <- "N:/Internationaal Beleid  (IB)/Projecten/2285000066 Africa Maize Yield Gap/SurveyData/TZA/2012/Data"
setwd(dataPath)

# SOURCE FUNCTIONS

# SET COUNTRY AND YEAR
iso3c <- "TZA"
surveyYear <- 2012

# Download Basemap
basemapPath = paste0(dataPath, "/../../../",  "Other/Spatial/Maps", "/",iso3c)

# Obtain country coordinates for target country
Get.country.shapefile.f <- function(iso3c, lev=0, proj = "+proj=longlat +datum=WGS84"){
  
  #download country boundary as spatialpolygonDF (and rewrite as .shp for convenience)
  targetfile <- paste(iso3c, paste("_adm", lev, ".Rdata", sep=""), sep="")
  if(file.exists(paste(basemapPath, targetfile, sep="/"))){
    load(paste(basemapPath, targetfile, sep="/"))
  } else {
    gadm=getData('GADM', country=iso3c, level=lev, path=basemapPath)
  }
  
  # change projection 
  projection <- proj
  country.sp <- spTransform(gadm, CRS(projection))
  return(country.sp)
}

country.map <- Get.country.shapefile.f("TZA", 2)

# PREPARE LSMS SPATIAL DATAPOINTS
# Get y2_hhid-GIS link
HH.geo <- read_dta(file.path(dataPath, "HouseholdGeovars_Y3.dta")) %>%
            rename(lat = lat_dd_mod, lon = lon_dd_mod)
plot.geo <- read_dta(file.path(dataPath, "PlotGeovars_Y3.dta")) 
            

# Create list of plots, hh, eas and coordinates
geo.base <- left_join(HH.geo, plot.geo) %>%
                      dplyr::select(lat, lon) %>%
                      unique() %>%
                      do(filter(., complete.cases(.)))%>%
                      mutate(eaid = seq(1:length(lat))) %>%
                      as.data.frame(.)


# Create list of plot variables
# Attributes are removed so that join in dplyr works (does not work with new labels that haven package adds)
unattribute <- function(df){
  for(i in names(df)) df[[i]] = strip_attributes(df[[i]])
  return(df)
}

geo.plot <- unattribute(plot.geo)

# create list of HH level variables
geo.hh <- unattribute(HH.geo)

# Create spatial points 
standardproj<-"+proj=longlat +datum=WGS84"
geo.coord <- geo.base %>% 
              dplyr::select(lon, lat) %>%
              SpatialPoints(., proj4string=CRS(standardproj))

# REGION INFORMATION
geo.region  <- over(geo.coord, country.map) %>%
  cbind(geo.base,.) %>%
  dplyr::select(-PID, -ID_0, -ISO, -NAME_0, -NL_NAME_2, -VARNAME_2, -TYPE_2, -ENGTYPE_2)  


# # CROP CALENDAR - NOT USED
# CropCalendarPath <- "D:\\Data\\IPOP\\CropCalendar\\Processed"
# cc <-stack(paste(CropCalendarPath, "CropCalendar", sep="\\"))
# 
# # Extract data
# geo.cc <- raster::extract(cc, geo.coord) %>%
#   cbind(geo.base,.) %>%
#   rename(start_planting = Maize..Start.of.planting, end_planting = Maize..End.of.planting,
#          start_harvest = Maize..Start.of.harvest, end_harvest = Maize..End.of.harvest)
         
# MONTHLY RAINFALL DATA
montlyRainfallPath <- "D:\\Data\\IPOP\\CRU_TS_3.22\\Processed"
pet <-brick(paste(montlyRainfallPath, "MonthlyEvapotranspiration190101_201312", sep="\\"))
pre <-brick(paste(montlyRainfallPath, "MonthlyPrecipitation190101_201312", sep="\\"))

# extract data
geo.pet <- raster::extract(pet, geo.coord) %>%
  cbind(geo.base,.) %>% 
  gather(date, pet, X1901.01.16:X2013.12.16)

geo.pre <- raster::extract(pre, geo.coord) %>%
  cbind(geo.base,.) %>% 
  gather(date, pre, X1901.01.16:X2013.12.16)

# Calculate SPEI
# CHECK: http://www.fews.net/east-africa/tanzania/seasonal-calendar/december-2013
# Check: http://www.fao.org/giews/countrybrief/country/TZA/pdf/TZA.pdf
# Check: http://www.sarpn.org/documents/d0001623/

# Secondary information and a comparison with the crop calendar and rainfall data
# (see crop calendar analysis) reveals that the reference period to calculate the
# SPEI is March-May given that only data is available per month.

# We decide to take the 2012 year rainfall for the SPEI index as the majority of the households
# answer questions on this season (see note 2010 geo file).

# Funtion to calculate SPEI
spei.f <- function(data, speiperiod, sd, ed){
  spei.calc <- spei(data$speicalc, speiperiod)
  spei.calc <- spei.calc$fitted
  t <- seq(ymd(sd),ymd(ed), by='months')
  spei.df <- data.frame(speicalc=as.matrix(spei.calc), date= t) %>%
    mutate(month = month(date, label=TRUE),
           year = year(date))
  return(spei.df)
}

# Period over which SPEI is calculated corresponding with three month growth period
speiendmonth <- "May"
speiperiod <- 3
gsmonths <- c("Mar", "Apr", "May")

# Period for which SPEI is estimated. To be increased with more historical data
sd <- "1901-01-01"
ed <- "2013-12-01"

# Calculate SPEI
geo.spei <- left_join(geo.pre, geo.pet) %>%
  do(filter(., complete.cases(.)))

# Add date information. Lubridate used to crash in R because of a problem in dplyr. This is solved by using the development version.
# I use base code here.
library(lubridate)
geo.spei <- geo.spei %>%  mutate(date = gsub("X","", date))
geo.spei$date <- as.Date(geo.spei$date, "%Y.%m.%d")
geo.spei$year <- format(geo.spei$date, "%Y")
geo.spei$month <- format(geo.spei$date, "%b")
geo.spei$days_in_month = days_in_month(geo.spei$date)
geo.spei <- geo.spei %>%
  mutate(petmonth =  pet*days_in_month,
         speicalc = pre-petmonth) %>%
  arrange(eaid, year, month) %>%
  do(filter(., complete.cases(.)))%>%
  ddply(.,.(eaid, lat, lon), spei.f, speiperiod, sd, ed) %>%
  filter(month == speiendmonth & year == surveyYear) %>%
  dplyr::select(-month, - year, -date)
names(geo.spei)[4] <- "SPEI" 

# Total rainfall in growing season
# Add date information. Lubridate used to crash in R because of a problem in dplyr. This is solved by using the development version.
# I use base code here.
geo.monthlyrainfall <- geo.pre %>%  mutate(date = gsub("X","", date))
geo.monthlyrainfall$date <- as.Date(geo.monthlyrainfall$date, "%Y.%m.%d")
geo.monthlyrainfall$year <- format(geo.monthlyrainfall$date, "%Y")
geo.monthlyrainfall$month <- format(geo.monthlyrainfall$date, "%b")
geo.monthlyrainfall <- geo.monthlyrainfall %>%
  filter(month %in% gsmonths) %>%
  group_by(eaid, lat, lon, year) %>%
  summarize(gsRainfall = sum(pre, na.rm=T)) %>%
  filter(year==surveyYear) %>%
  dplyr::select(-year)


# SOIL DATA
# Extract soil data and link to GIS coordinates of plots/housholds
soilPath <- paste("D:\\Data\\IPOP\\AFSIS\\Processed",iso3c, sep="\\")
setwd(soilPath)
countryFiles1km <- list.files(pattern = "\\__1km.tif$")
soil <-stack(countryFiles1km)

# ROOT DEPTH
RootDepthPath <- paste("D:\\Data\\IPOP\\AFSIS\\Processed",iso3c, sep="\\")
setwd(RootDepthPath)
RootDepth <-raster(paste(iso3c, "_", "af_agg_ERZD_TAWCpF23mm__M_1km.tif", sep=""))

# extract data
geo.rootdepth <- raster::extract(RootDepth, geo.coord) %>%
  cbind(geo.base,.) %>%
  do(filter(., complete.cases(.))) # NA values for some regions removed
names(geo.rootdepth)[4] <- "RootDepth"

# CARBON STOCK AND PH
# extract data
geo.soil <- raster::extract(soil, geo.coord) %>% 
  cbind(geo.base,.) %>%
  gather(property, value, -eaid, - lat, -lon) %>% 
  separate(property, c("iso3c", "af", "Property", "T", "M1", "M2", "sd", "x", "grid"), sep = "_") %>%
  dplyr::select(-af, -T, -M1, -M2, -x, -grid)

# Prepare final soil variables
# Carbon stock (kg/m2) for a depth of 0-30 cm (sd1-sd3) and 0-100 cm (sd1-sd5)
# http://www.eoearth.org/view/article/156087/ for refs

# Add standard column depths
# Six standard depths following the Global-
# SoilMap.net specifications: sd1 = 2.5 cm (0-5), sd2 = 10 cm (5-15), sd3 = 22.5 cm (15-30), sd4
# = 45 cm (30-60), sd5 = 80 cm (60-100), sd6 = 150 cm (100-200). (p. 34 package documentation)
sds <- get("stsize", envir = GSIF.opts)
soilsize <- data.frame(sd = c("sd1", "sd2", "sd3", "sd4", "sd5", "sd6"), sds = sds)

# SOC sd1-sd3
geo.socsd1_sd3 <- geo.soil %>% 
  filter(Property %in% c("ORCDRC", "CRFVOL", "BLD") & sd %in% c("sd1", "sd2", "sd3")) %>%
  arrange(lat, lon, Property) %>%
  left_join(., soilsize) %>%
  spread(Property, value) %>%
  do(filter(., complete.cases(.))) %>% # NA values for some regions removed
  mutate(SOC = as.numeric(OCSKGM(ORCDRC, BLD, CRFVOL, sds*100))) %>%
  group_by(eaid, lat, lon) %>%
  summarize(SOC_sd1_sd3 = sum(SOC, na.rm=FALSE))

# SOC sd1-sd5
geo.socsd1_sd5 <- geo.soil %>% 
  filter(Property %in% c("ORCDRC", "CRFVOL", "BLD") & sd %in% c("sd1", "sd2", "sd3", "sd4", "sd5")) %>%
  arrange(lat, lon, Property) %>%
  left_join(., soilsize) %>%
  spread(Property, value) %>%
  do(filter(., complete.cases(.))) %>% # NA values for some regions removed
  mutate(SOC = as.numeric(OCSKGM(ORCDRC, BLD, CRFVOL, sds*100))) %>%
  group_by(eaid, lat, lon) %>%
  summarize(SOC_sd1_sd5 = sum(SOC, na.rm=FALSE))

geo.soc <- left_join(geo.socsd1_sd3, geo.socsd1_sd5)

# Ph sd1_sd3
geo.phsd1_sd3 <- geo.soil %>% 
  filter(Property %in% c("PHIHOX") & sd %in% c("sd1", "sd2", "sd3")) %>%
  arrange(lat, lon, Property) %>%
  left_join(., soilsize) %>%
  do(filter(., complete.cases(.))) %>% # NA values for some regions removed
  group_by(eaid, lat, lon)  %>%
  summarize(ph_sd1_sd3 = sum(value*sds)/sum(sds)) # Weighted average
  
# Ph sd1_sd5
geo.phsd1_sd5 <- geo.soil %>% 
  filter(Property %in% c("PHIHOX") & sd %in% c("sd1", "sd2", "sd3", "sd4", "sd5")) %>%
  arrange(lat, lon, Property) %>%
  left_join(., soilsize) %>%
  do(filter(., complete.cases(.))) %>% # NA values for some regions removed
  group_by(eaid, lat, lon)  %>%
  summarize(ph_sd1_sd5 = sum(value*sds)/sum(sds)) # Weighted average

geo.ph <- left_join(geo.phsd1_sd3, geo.phsd1_sd5)

# GYGA DATA
GYGApath <- "D:\\Data\\IPOP\\GYGA\\"

# # Get GYGA map
dsn=paste(GYGApath, "\\CZ_SubSaharanAfrica\\CZ_AFRGYGACNTRY.shp", sep="")
ogrListLayers(dsn)
ogrInfo(dsn, layer="CZ_AFRGYGACNTRY")
GYGA <- readOGR(dsn, layer = "CZ_AFRGYGACNTRY") %>%
  spTransform(., CRS("+proj=longlat +datum=WGS84"))

# Link yield gap data
# Yield gap data is provided in a separate file.
# in order to merge data with spatialpolygondataframe the row.names need to be the same.
# For this reason I first merge the additionald data and then create a spatialpolygondataframe
# again ensuring that the rownames are preserved.

GYGA.yield.data <- read.xls(file.path(GYGApath, "GygaRainfedMaizeSubSaharanAfrica.xlsx"), sheet="Climate zone") %>%
  rename(GRIDCODE = CLIMATEZONE, REG_NAME = COUNTRY) %>%
  mutate(iso = countrycode(REG_NAME,"country.name", "iso3c")) 
GYGA@data <- GYGA@data %>% mutate(iso = countrycode(REG_NAME,"country.name", "iso3c")) %>%
  left_join(., GYGA.yield.data)

# # Extract data
geo.GYGA <- raster::extract(GYGA, geo.coord) %>%
  dplyr::select(CROP, YA, YW, YW.YA, YP, YP.YA, iso) %>%
  cbind(geo.base,.)

# FARMING SYSTEMS 
# http://rpackages.ianhowson.com/rforge/raster/man/factor.html on factor values in raster
# http://oscarperpinan.github.io/rastervis/ for plotting raster
fsPath <- "D:\\Data\\IPOP\\FarmingSystems\\fs_2012_tx--ssa.tif_5"
fs <-raster(file.path(fsPath, "FS_2012_TX--SSA.tif"))

# plot maps
p <- crop (fs, country.map)
levelplot(p) +layer(sp.polygons(country.map, col='black')) 

# Extract factor values
geo.fs <- raster::extract(fs, geo.coord)
fs_val <- factorValues(fs, geo.fs)

# Extract data, remove numbering and turn into factor
geo.fs <- raster::extract(fs, geo.coord) %>% 
  cbind(geo.base,.) %>%
  cbind(., fs_val) %>%
  mutate(fs = str_sub(category, 4),
         fs = factor(trimws(fs))) %>%
  dplyr::select(-.,-category)

# BIND ALL SPATIAL INFORMATION
geo.total <-  left_join(geo.region, geo.monthlyrainfall) %>%
  left_join(., geo.spei) %>%
  left_join(., geo.rootdepth) %>%
  left_join(., geo.ph) %>%
  left_join(., geo.soc) %>%
  left_join(., geo.GYGA) %>%
  left_join(., geo.fs) 

# check missing values (not including GYGA data for which we know data is missing)
geo.check <- geo.total %>% dplyr::select(-CROP:-iso) %>% do(filter(., !complete.cases(.))) 
geo.check.plot <- geo.check %>% 
  dplyr::select(lon, lat) %>%
  SpatialPoints(., proj4string=CRS(standardproj))

# # Plot missing values in GoogleMaps
# # Does NOT work in explorer => set default browser to Firefox
# library(plotGoogleMaps)
# geo.check.google <- spTransform(geo.check.plot, CRS('+init=epsg:28992'))
# m <- plotGoogleMaps(geo.check.google)

# Plot missing values on map
plot(crop(pet[[1356]], country.map))
plot(country.map, add=T)
points(geo.check.plot, pch = 18, col="red")

# Data is missing for Zanzibar and for other locations that are located close to victoria lake
# Problem is missing data for pet and therefore SPEI cannot be calculated.
# Perhaps this can be improved by using nearest points.


# Merge plot and household level data
# These are not relevant for our analysis.
geo.total.plot <- left_join(geo.hh, geo.plot) %>% 
  left_join(., geo.total)

# Get AEZ labels and recode
AEZ_code <- read.csv(file.path(paste0(dataPath,"/../../.."), "Other\\Spatial\\Other\\AEZ_code.csv"))

geo.total.plot <- geo.total.plot %>%
  transmute(y3_hhid, lon, lat, plotnum, SPEI, RootDepth, region_name=NAME_1, district_name=NAME_2,
                AEZ=land03, ph=ph_sd1_sd3, ph2=ph_sd1_sd5,
                SOC=SOC_sd1_sd3, SOC2=SOC_sd1_sd5, rain_CRU=gsRainfall,
                dist_hh=plot01, 
                slope=plot02, elevation=plot03, plot_pwi=plot04, # 
                dist_road=dist01, dist_popcenter=dist02, dist_market=dist03, dist_borderpost=dist04, dist_regcap=dist05,
                nut = soil05, nrc = soil06,
                rain_year=crops07, rain_wq=crops08, fs, 
                YA, YW, YP) %>%
            mutate(AEZ = droplevels(factor(AEZ,
                                 levels = AEZ_code$code,
                                 labels = AEZ_code$AEZ)))



# Write file
saveRDS(geo.total.plot, file = "D:/Data/Projects/OF_TZA/Cache/TZA_geo_2012.rds")

