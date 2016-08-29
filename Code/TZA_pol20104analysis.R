# -------------------------------------
# prepare the 2010 presidential results
# for analysis later
# -------------------------------------

if(Sys.info()["user"] == "Tomas"){
  dataPath <- "C:/Users/Tomas/Documents/LEI/OF_TZA/"
} else {
  dataPath <- "D:/Data/Projects/OF_TZA" 
}

library(dplyr)
library(reshape2)

prez2010 <- readRDS(file.path(dataPath, "data/prez2010/prez2010.rds"))

# In many places there are Swahili names
# we remove this part of the name

prez2010$dis <- gsub("WILAYA YA ", "", prez2010$dis)
dar <- prez2010$reg %in% "DAR-ES-SALAAM"
prez2010$dis[dar] <- gsub("MANISPAA YA ", "", prez2010$dis[dar])

urban <- c(grep("MANISPAA YA ", prez2010$dis), grep("JIJI LA ", prez2010$dis), grep("MJI WA ", prez2010$dis))
prez2010$dis[urban] <- gsub("MANISPAA YA ", "", prez2010$dis[urban])
prez2010$dis[urban] <- gsub("JIJI LA ", "", prez2010$dis[urban])
prez2010$dis[urban] <- gsub("MJI WA ", "", prez2010$dis[urban])
prez2010$dis[urban] <- paste(prez2010$dis[urban], "URBAN", sep=" ")

# --------------------------------------
# in the LSMS data districts there are some slight
# changes compared to the electoral data districts
# -------------------------------------

# Siha is still part of Hai in the LSMS data
prez2010$dis <- gsub("SIHA", "HAI", prez2010$dis)

# the district called dodoma rural is
# actualy comprised of two districts 
# called BAHI and CHAMWINO
prez2010$dis <- gsub("BAHI", "DODOMA RURAL", prez2010$dis)
prez2010$dis <- gsub("CHAMWINO", "DODOMA RURAL", prez2010$dis)

# in the LSMS data the district called 
# LONGIDO is actually a part of MONDULI

prez2010$dis <- gsub("LONGIDO", "MONDULI", prez2010$dis)

# -------------------------------------
# summarise the data by district
prez2010_dis <- group_by(prez2010, reg, dis, party) %>%
  summarise(votes=sum(votes)) 
prez2010_dis.x <- group_by(prez2010, reg, dis) %>%
  summarise(votesTotal=sum(votes))
prez2010_dis <- left_join(prez2010_dis, prez2010_dis.x); rm(prez2010_dis.x)
prez2010_dis <- mutate(prez2010_dis, percent = votes/votesTotal*100)

# Here a party has won a district if they
# have the overall highest number of votes
# in that district. This does not necessarily 
# mean that they won every constituency in that
# district. 

# All party vote shares
prez2010_dis_All <- select(prez2010_dis, reg, dis, party, percent) %>%
  dcast(reg + dis ~ party)

# just ruling party and vote split
prez2010_dis <- group_by(prez2010_dis, reg, dis) %>%
  summarise(ccm_prez10=ifelse(party[which.max(votes)] %in% "CCM", 1, 0),
            split_prez10=ifelse(length(party) == 1, ifelse(is.na(percent), 100, percent),
                                abs(percent[party %in% "CCM"] - max(percent[!party %in% "CCM"],
                                                                    na.rm=TRUE))))
# join both of these together so that for each district
# we know what vote share each party received AND we 
# know whether the CCM won that district AND we know 
# the vote split between the CCM and the largest or 
# next largest party
prez2010 <- left_join(prez2010_dis, prez2010_dis_All)

rm(prez2010_dis, prez2010_dis_All, urban, dataPath, dar)