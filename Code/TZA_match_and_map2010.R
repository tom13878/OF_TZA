# -------------------------------------
# summarise the data at the district
# level to see how each party fared
# in each district
# -------------------------------------

# path to folder
dataPath <- "D:/Data/Projects/OF_TZA" 

# packages
library(rgdal)
library(reshape2)
library(ggplot2)
library(RColorBrewer)
library(dplyr)

# read in the data
prez2010_map <- readRDS(file.path(dataPath, "Data/prez2010/prez2010_map.rds"))

# summarise the data to the district/party level
prez2010_dis_map <- group_by(prez2010_map, reg, dis, party) %>%
  summarise(votes=sum(votes)) 
prez2010_dis_map.x <- group_by(prez2010_dis_map, reg, dis) %>%
  summarise(votesTotal=sum(votes))
prez2010_dis_map <- left_join(prez2010_dis_map, prez2010_dis_map.x); rm(prez2010_dis_map.x)
prez2010_dis_map <- mutate(prez2010_dis_map, percent = votes/votesTotal*100)

# make a dataframe showing there percentage of votes
# received by each party in each district

prez2010_dis_map <- select(prez2010_dis_map, reg, dis, party, percent) %>%
  dcast(reg + dis ~ party, fun.aggreagate=)

# -------------------------------------
# join the political data with the map data
# read in the map2pol file
# -------------------------------------

map2pol <- read.csv(file.path(dataPath, "Data/map2pol2010.csv"))
names(map2pol) <- c("REGNAME", "DISTNAME", "reg", "dis")

# get map
TZA <- readOGR(dsn = file.path(dataPath, "Data/maps/Tanzania_District_EA_2002_region"), layer = "Tanzania_District_EA_2002_region")

# join with political data districts.
TZA@data <- left_join(TZA@data, map2pol) # islands are missing but that's fine!
TZA@data <- left_join(TZA@data, prez2010_dis_map)

# -------------------------------------
# mapping
# -------------------------------------

# fortify map for ggplot
tf <- fortify(TZA)
TZA@data$id <- unique(tf$id)
tf <- left_join(tf, TZA@data)

# check available colors
brewer.pal.info

# select variables and assign colors
parties <- as.character(unique(prez2010_map$party))
colors <- c("Blues", "Greens", "Reds", "YlGn", "Oranges", "Purples", "Greys")

# Plotting function, parties have their own scale
plotter <- function(var, color) ggplot(tf) + 
  geom_polygon(data=tf, aes_string(x="long", y="lat", group="group", fill=var), colour="black", size = .1) +
  coord_map("mercator") + ggtitle(var) +
  theme(legend.position="bottom") +
  scale_fill_gradientn(colours=c("#ffffff", brewer.pal(n=9, name=color)),
                       na.value="#ffffff")

# pdf(paste0(dataPath, "parties.pdf"))
# lapply(1:7, function(i) plotter(parties[i], colors[i]))
# dev.off()

# Plotting function, scale always between 0 and 100
plotter2 <- function(var, color) ggplot(tf) + 
  geom_polygon(data=tf, aes_string(x="long", y="lat", group="group", fill=var), colour="black", size = .1) +
  coord_map("mercator") + ggtitle(var) +
  theme(legend.position="bottom") +
  scale_fill_gradientn(colours=c("#ffffff", brewer.pal(n=9, name=color)),
                       na.value="#ffffff", limits=c(0, 100))

pdf(file.path(dataPath, "Graphs/parties2.pdf"))
lapply(1:7, function(i) plotter2(parties[i], colors[i]))
dev.off()
