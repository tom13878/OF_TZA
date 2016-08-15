# -------------------------------------
# scrape the 2005 legistlative election
# data from the tanzania electoral 
# commission website and save in a 
# summarised form
# -------------------------------------


wdPath <- "D:/Data/Projects/OF_TZA/"
setwd(wdPath)

# scraper function

get_leg_data <- function(url, CSS){
  
  require(rvest)
  require(dplyr)
  
  leg <- url %>%
    read_html %>%
    html_nodes(CSS) %>%
    html_table(header=T) %>%
    data.frame %>%
    filter(!Region %in% "Region") %>%
    select(-Region, -District, -Const)
  
  return(leg)
  
}

# urls for the legislative data, spread over three pages for 2005

url05_1 <- "https://support.toyotatz.com/nec2/index.php?modules=election_results&sub=&etype=2&year=10&region=&district=&const=&ward=&button2=Submit"
url05_2 <- "https://support.toyotatz.com/nec2/index.php?page=1&modules=election_results&sub=&etype=2&year=10&region=&district=&const=&ward=&button2=Submit"
url05_3 <- "https://support.toyotatz.com/nec2/index.php?page=2&modules=election_results&sub=&etype=2&year=10&region=&district=&const=&ward=&button2=Submit"

# CSS path for table in data

CSS <- "#print > table"

# grab all the data for 2005

leg05_1 <- get_leg_data(url05_1, CSS)
leg05_2 <- get_leg_data(url05_2, CSS)
leg05_3 <- get_leg_data(url05_3, CSS)

leg05 <- rbind(leg05_1, leg05_2, leg05_3)

# rename variables

leg05 <- rename(leg05, reg=RegionID, dis=DistrictID, con=ConstID, candidate=Candidate,
                party=Party, sex=Sex, votes=Valid.Votes, spoilt=Spoilt.Votes, perc=X.)

# make numeric variables
leg05$votes <- gsub(",", "", leg05$votes) %>% as.numeric
leg05$spoilt <- gsub(",", "", leg05$spoilt) %>% as.numeric
leg05$perc <- leg05$perc %>% as.numeric

# make everything upper case
leg05$reg <- toupper(leg05$reg)
leg05$dis <- toupper(leg05$dis)
leg05$con <- toupper(leg05$con)

leg05$reg <- gsub("DAR ES SALAAM", "DAR-ES-SALAAM", leg05$reg)

# remove islands
islands <- c("KASKAZINI PEMBA", "KASKAZINI UNGUJA", "KUSINI PEMBA",
             "KUSINI UNGUJA", "MJINI MAGHARIBI")
leg05 <- leg05[!leg05$reg %in% islands, ]

# summarise data
leg05 <- group_by(leg05, reg, dis, con) %>% 
  summarise(ccm_leg10=ifelse(party[which.max(votes)] %in% "CCM", 1, 0),
            split_leg10=ifelse(length(party) == 1, ifelse(is.na(perc), 100, perc),
                               abs(perc[party %in% "CCM"] - max(perc[!party %in% "CCM"],
                                                                na.rm=TRUE))))

# write to csv
write.csv(leg05, "Cache/leg2005.csv", row.names=FALSE)
