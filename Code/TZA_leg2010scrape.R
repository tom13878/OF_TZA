# -------------------------------------
# scrape the 2010 legistlative election
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

# urls for the legislative data, spread over three pages for 2010
url10_1 <- "https://support.toyotatz.com/nec2/index.php?modules=election_results_2010&sub=&etype=2&region=&button2=Submit"
url10_2 <- "https://support.toyotatz.com/nec2/index.php?page=1&modules=election_results_2010&sub=&etype=2&region=&button2=Submit"
url10_3 <- "https://support.toyotatz.com/nec2/index.php?page=2&modules=election_results_2010&sub=&etype=2&region=&button2=Submit"

# CSS path for tables containing data
CSS <- "#print > table"

# grab all the data for 2010
leg10_1 <- get_leg_data(url10_1, CSS)
leg10_2 <- get_leg_data(url10_2, CSS)
leg10_3 <- get_leg_data(url10_3, CSS)

leg10 <- rbind(leg10_1, leg10_2, leg10_3)

# rename variables
leg10 <- rename(leg10, reg=RegionID, dis=DistrictID, con=ConstID, candidate=Candidates,
                party=Political.Party, votes=Total.Votes, spoilt=Spoiltvotes, perc=Percentage.Votes)

# make numeric variables
leg10$votes <- gsub(",", "", leg10$votes) %>% as.numeric
leg10$spoilt <- gsub(",", "", leg10$spoilt) %>% as.numeric
leg10$perc <- leg10$perc %>% as.numeric

# make everything upper case
leg10$reg <- toupper(leg10$reg)
leg10$dis <- toupper(leg10$dis)
leg10$con <- toupper(leg10$con)

leg10$reg <- gsub("DAR ES SALAAM", "DAR-ES-SALAAM", leg10$reg)

# remove islands
islands <- c("KASKAZINI PEMBA", "KASKAZINI UNGUJA", "KUSINI PEMBA",
             "KUSINI UNGUJA", "MJINI MAGHARIBI")
leg10 <- leg10[!leg10$reg %in% islands, ]

# summarise data
leg10 <- group_by(leg10, reg, dis, con) %>% 
  summarise(ccm_leg10=ifelse(party[which.max(votes)] %in% "CCM", 1, 0),
            split_leg10=ifelse(length(party) == 1, ifelse(is.na(perc), 100, perc),
                               abs(perc[party %in% "CCM"] - max(perc[!party %in% "CCM"],
                                                                na.rm=TRUE))))

# write to csv
write.csv(leg10, "Cache/leg2010.csv", row.names=FALSE)
