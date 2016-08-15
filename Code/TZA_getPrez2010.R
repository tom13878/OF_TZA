# -------------------------------------
# read in the presidential election 
# results downloaded from the NEC website
# -------------------------------------

dataPath <- "D:/Data/Projects/OF_TZA/Cache" 

library(dplyr)
library(reshape2)

# read in data
prez2010 <- data.frame()

# note that some excel files were missing when downloaded
# from nec website - but all regions and districts are available
# so these files likely contained nothing

for( i in c(1:12, 16:24)){
  fileName <- paste("data/prez2010/prez2010_", i, ".csv", sep="")
  file <- read.csv(file.path(dataPath, fileName))
  prez2010 <- rbind(prez2010, file)
}

# remove header rows and empty space from 
# file

bad <- prez2010$X %in% "RegionID" | prez2010$X %in% ""
prez2010 <- prez2010[!bad, ]
names(prez2010) <- c("regID", "reg", "disID", "dis", "conID", "con",
                     "candidate", "party", "votes", "spoilt", "perc")

# kill of the islands which we do not need
# and change Dar es salaam region name

islands <- c("KASKAZINI PEMBA", "KASKAZINI UNGUJA", "KUSINI PEMBA",
             "KUSINI UNGUJA")
prez2010 <- prez2010[!prez2010$reg %in% islands, ]

prez2010$reg <- gsub("DAR ES SALAAM", "DAR-ES-SALAAM", prez2010$reg)

# change factor variables to numerics
prez2010$votes <- as.numeric(as.character(prez2010$votes))
prez2010$spoilt <- as.numeric(as.character(prez2010$spoilt))
prez2010$perc <- as.numeric(as.character(prez2010$perc))

# write to file to use later
saveRDS(prez2010, file.path(dataPath, "data/prez2010/prez2010.rds"))
