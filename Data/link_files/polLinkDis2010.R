# join political data at the district level
# with the 2010 data

library(haven)

# read in HHA 2010

dataPath <- "C:/Users/Tomas/Documents/LEI/data/TZA/"
HHA <- read_dta(file.path(dataPath, "HH_SEC_A.dta"))
HHA <- select(HHA, y2_hhid, region, district)
HHA <- rename(HHA, REG.CODE = region, DIS.CODE = district)
HHA <- HHA[HHA$REG.CODE %in% 1:21,]

# read in file with region and district names
# taken from the back of the LSMS-ISA survey
# and join with HHA

setwd("C:/Users/Tomas/Documents/LEI/pol/data/link_files")
DISREG <- read.csv("DISREG.csv")

HHA <- left_join(HHA, DISREG)

# read in the political information at the
# district level

setwd("C:/Users/Tomas/Documents/LEI/pol/data/prez2010")
prez2010_dis <- read.csv("prez2010_dis.csv")
prez2010_dis <- rename(prez2010_dis, REG.NAME = reg, POLDIS.NAME = dis)

# read in the link file for the districts in
# the political data and in the lsms-isa data
# and join the political data with it's distict
# names

setwd("C:/Users/Tomas/Documents/LEI/pol/data/link_files")
DISREG2POL <- read.csv("DISREG2POL.csv")
prez2010_dis <- left_join(prez2010_dis, DISREG2POL)
prez2010_dis <- prez2010_dis[complete.cases(prez2010_dis), ]
prez2010_dis <- select(prez2010_dis, - POLDIS.NAME)

# join the district results with the household data
polLinkDis2010 <- left_join(HHA, prez2010_dis)

# write the result to a file that can be used to
# join with other important data

saveRDS(polLinkDis2010, "polLinkDis2010.rds")

