# -------------------------------------
# organize the presidential election
# data for 2010. Summarise and store
# in a single file
# -------------------------------------

library(dplyr)

dataPath <- "D:/Data/Projects/OF_TZA/" 

# read in data
prez2005 <- data.frame()

for( i in 1:3){
  fileName <- paste("Data/prez2005/prez2005_", i, ".csv", sep="")
  file <- read.csv(file.path(dataPath, fileName))
  prez2005 <- rbind(prez2005, file)
}

# remove header rows and empty space from 
# file

bad <- prez2005$Region %in% "Region" | prez2005$Region %in% ""
prez2005 <- prez2005[!bad, ]
names(prez2005) <- c("regID", "reg", "disID", "dis", "conID", "con",
                     "candidate", "party", "sex", "votes", "spoilt", "perc")

# kill of the islands which we do not need
# and change Dar es salaam region name

islands <- c("KASKAZINI PEMBA", "KASKAZINI UNGUJA", "KUSINI PEMBA",
             "KUSINI UNGUJA", "MJINI MAGHARIBI")
prez2005 <- prez2005[!prez2005$reg %in% islands, ]

prez2005$reg <- gsub("DAR ES SALAAM", "DAR-ES-SALAAM", prez2005$reg)

# change factor variables to numerics
prez2005$votes <- as.numeric(as.character(prez2005$votes))
prez2005$spoilt <- as.numeric(as.character(prez2005$spoilt))
prez2005$perc <- as.numeric(as.character(prez2005$perc))

# summarise the data by constituency
prez2005 <- group_by(prez2005, reg, dis, con) %>% 
  summarise(ccm_prez05=ifelse(party[which.max(votes)] %in% "CCM", 1, 0),
            split_prez05=ifelse(length(party) == 1, ifelse(is.na(perc), 100, perc),
                                abs(perc[party %in% "CCM"] - max(perc[!party %in% "CCM"],
                                                                 na.rm=TRUE))))

rm(list=ls()[!ls() %in% "prez2005"])

write.csv(prez2005, file.path(dataPath, "Data/prez2005/prez2005.csv"), row.names=FALSE)
