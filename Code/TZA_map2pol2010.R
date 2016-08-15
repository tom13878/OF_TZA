# -------------------------------------
# manipulate presidential results to
# be able to map them using the 2002
# census maps
# -------------------------------------

dataPath <- "C:/Users/Tomas/Documents/LEI/pol" 
prez2010 <- readRDS(file.path(dataPath, "data/prez2010/prez2010.rds"))

# the 2002 census map does not match up
# perfectly with the election results.
# often this is because cities are
# included as part of a wider district. 

prez2010$dis <- as.character(prez2010$dis)
prez2010$party <- as.character(prez2010$party)
prez2010$party <- ifelse(prez2010$party %in% "APPT - MAENDELEO", "APPT", prez2010$party)
prez2010$party <- ifelse(prez2010$party %in% "NCCR-MAGEUZI", "NCCR", prez2010$party)

prez2010_map <- prez2010

# arusha
arusha <- c("JIJI LA ARUSHA", "WILAYA YA ARUSHA")
prez2010_map$dis <- ifelse(prez2010_map$dis %in% arusha, "WILAYA YA ARUSHA", prez2010_map$dis)
monduli <- c("WILAYA YA LONGIDO", "WILAYA YA MONDULI")
prez2010_map$dis <- ifelse(prez2010_map$dis %in% monduli, "WILAYA YA MONDULI", prez2010_map$dis)

# dodoma
dodomaRural <- c("WILAYA YA CHAMWINO", "WILAYA YA BAHI")
prez2010_map$dis <- ifelse(prez2010_map$dis %in% dodomaRural, "WILAYA YA DODOMA", prez2010_map$dis)

# kilimajaro
hai <- c("WILAYA YA HAI", "WILAYA YA SIHA")
prez2010_map$dis <- ifelse(prez2010_map$dis %in% hai, "WILAYA YA HAI", prez2010_map$dis)

# MANYARA
babati <- c("MJI WA BABATI", "WILAYA YA BABATI")
prez2010_map$dis <- ifelse(prez2010_map$dis %in% babati, "WILAYA YA BABATI", prez2010_map$dis)

# Pwani
kibaha <- c("MJI WA KIBAHA", "WILAYA YA KIBAHA")
prez2010_map$dis <- ifelse(prez2010_map$dis %in% kibaha, "WILAYA YA KIBAHA", prez2010_map$dis)

# MARA
tarime <- c("WILAYA YA TARIME", "WILAYA YA RORYA")
prez2010_map$dis <- ifelse(prez2010_map$dis %in% tarime, "WILAYA YA TARIME", prez2010_map$dis)

# Mwanza: Mwanza city (second largest in Tanzania) is partly in 
# ilemela and partly in Nyamagana, but for election purposes the
# overall district is just JIJI LA MWANZA -> included in map2pol file

# RUKWA
mpanda <- c("MJI WA MPANDA", "WILAYA YA MPANDA")
prez2010_map$dis <- ifelse(prez2010_map$dis %in% mpanda, "WILAYA YA MPANDA", prez2010_map$dis) 

# TANGA
korogwe <- c("WILAYA YA KOROGWE", "MJI WA KOROGWE")
prez2010_map$dis <- ifelse(prez2010_map$dis %in% korogwe, "WILAYA YA KOROGWE", prez2010_map$dis) 

# in 2007 Mkinga district was split-off from Muheza district
muheza <- c("WILAYA YA MKINGA", "WILAYA YA MUHEZA")
prez2010_map$dis <- ifelse(prez2010_map$dis %in% muheza, "WILAYA YA MUHEZA", prez2010_map$dis) 

rm(arusha, babati, dodomaRural, hai, kibaha, korogwe, monduli, mpanda, muheza, tarime)

# save for use in mapping
saveRDS(prez2010_map, file.path(dataPath, "data/prez2010/prez2010_map.rds"))
