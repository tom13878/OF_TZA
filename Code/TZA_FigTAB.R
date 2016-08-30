

# Maps with GYGA yield potential and plot information
# transform shapefile in dataframe for ggplot. rownames are used as ID for the countries. Using ID gives strange results. 
# Additional data is linked back again
library(rgdal)
library(sp)
library(raster)
library(foreign)
library(gdata)
library(RColorBrewer)

# GYGA DATA
GYGApath <- "D:\\Data\\IPOP\\GYGA\\"

dsn=paste(GYGApath, "\\CZ_SubSaharanAfrica\\CZ_AFRGYGACNTRY.shp", sep="")
ogrListLayers(dsn)
ogrInfo(dsn, layer="CZ_AFRGYGACNTRY")
GYGA.Africa<-readOGR(dsn, layer = "CZ_AFRGYGACNTRY")
projection(GYGA.Africa) # check projection
GYGA.Africa <- spTransform(GYGA.Africa, CRS("+proj=longlat +datum=WGS84"))

# Get GYGA
GYGA.country.yield.data <- read.xls(paste(GYGApath, "GygaEthiopia.xlsx", sep="\\"), sheet=3)

# Select data for maize
GYGA.country.yield.data <- subset(GYGA.country.yield.data, CROP=="Rainfed maize")

# Cut out ETH from GYGA map
GYGA.country <- GYGA.Africa[GYGA.Africa$REG_NAME=="Ethiopia",]

# Link yield gap data
# in order to merge data with spatialpolygondataframe the row.names need to be the same.
# For this reason I first merge the additionald data and then create a spatialpolygondataframe
# again ensuring that the rownames are preserved.

GYGA.country.data <- as(GYGA.country, "data.frame")
GYGA.country.data$id <-row.names(GYGA.country.data)
GYGA.country.data <- merge(GYGA.country.data, GYGA.country.yield.data[,c(1:8)], by.x=c("GRIDCODE"), by.y=c("CLIMATEZONE"), all.x=TRUE, sort=FALSE)
row.names(GYGA.country.data)<-GYGA.country.data$id
GYGA.country <- SpatialPolygonsDataFrame(as(GYGA.country, "SpatialPolygons"),
                                         data=GYGA.country.data)

# Maps with GYGA yield potential and plot information
# transform shapefile in dataframe for ggplot. rownames are used as ID for the countries. Using ID gives strange results. 
# Additional data is linked back again
GYGA.country.fort<- fortify(GYGA.country) 
GYGA.country.fort <- merge(GYGA.country.fort, GYGA.country.data, by="id")
GYGA.country.fort$yieldclass <- cut(GYGA.country.fort$YW, breaks=c(6, 8.5, 11, 13.5, 16, 19))
meanYield <- ddply(db9,.(lon, lat), summarize, meanYield = (sum(Y*area)/sum(area))/1000)
meanYield$meanYield2 <- cut(meanYield$meanYield, breaks=c(0, 1, 2, 3, 11))
library(ggthemes)
GYGA_LSMS <- ggplot()+
  geom_polygon(data=GYGA.country.fort, aes(x=long, y=lat, group=group, fill=yieldclass), colour="black")+
  geom_polygon(data=subset(GYGA.country.fort, is.na(YW)), aes(x=long, y=lat, group=group), fill="white", colour="black") +
  scale_fill_discrete(name="Potential water\nlimited yield (tons)") +
  geom_point(data=meanYield, aes(x=lon, y=lat, size=(meanYield2)), colour="black")+
  scale_size_manual(name="Average yield (tons)", values=c(1, 2, 3, 4)) +
  coord_equal()+
  labs(x="", y="")+
  theme_classic() +
  theme(legend.key=element_blank(),
        line = element_blank(),
        axis.text = element_blank())

#GYGA_LSMS
ggsave(plot = GYGA_LSMS, ".\\FigTab\\GYGA_LSMS.png", height = 150, width = 200, type = "cairo-png", units="mm")

# Zonal map with community yield levels
# read in map of tanzania as a SpatialPolygonsDataFrame

countryMap <- getData('GADM', country = "ETH", level = 1) 

# Rename zones using LSMS names
countryMap@data$ZONE <- countryMap@data$NAME_1
countryMap@data$ZONE[countryMap@data$NAME_1 %in% c("Oromia")] <- "Oromiya"
countryMap@data$ZONE[countryMap@data$NAME_1 %in% c("Somali")] <- "Somalie"
countryMap@data$ZONE[countryMap@data$NAME_1 %in% c("Benshangul-Gumaz")] <- "Benishangul Gumuz"
countryMap@data$ZONE[countryMap@data$NAME_1 %in% c("Southern Nations, Nationalities and Peoples")] <- "SNNP"
countryMap@data$ZONE[countryMap@data$NAME_1 %in% c("Gambela Peoples")] <- "Gambella"
countryMap@data$ZONE[countryMap@data$NAME_1 %in% c("Harari People")] <- "Harari"
countryMap@data$ZONE <- factor(countryMap@data$ZONE)

# Remove Addis Ababa and Dire Dawa
countryMap <- countryMap[!(countryMap@data$ZONE %in% c("Addis Abeba", "Dire Dawa")),]
plot(countryMap)

#  fortify spatial data to use with ggplot and join using join functions from dplyr
#    The join is on id, make sure all ids are character vectors
tf <- fortify(countryMap)
countryMap@data <- rename(countryMap@data, id = ID_1)
countryMap@data$id <- as.character(countryMap@data$id)
tf2 <- left_join(tf, countryMap@data)

# Use ggplot to plot map of Tanzania, specifying the labels and choosing nice colours
#    from the RColorBrewer package

#display.brewer.all()

ZONE_LSMS <- ggplot()+
  geom_polygon(data=tf2, aes(x=long, y=lat, group=group, fill=ZONE), colour="black")+
  geom_point(data=meanYield, aes(x=lon, y=lat, size=(meanYield2)), colour="black")+
  scale_fill_brewer(name = "Zones", palette = "Set1") +
  scale_size_manual(name="Average yield (tons)", values=c(1.5, 2.5, 3.5, 4,5)) +
  coord_equal()+
  labs(x="", y="")+
  theme_classic() +
  theme(legend.key=element_blank(),
        line = element_blank(),
        axis.text = element_blank())
#ZONE_LSMS 

ggsave(plot = ZONE_LSMS, ".\\Cache\\ZONE_LSMS.png", height = 150, width = 200, type = "cairo-png", units="mm")
save.image("Cache/FigTab.Rdata")


