# -------------------------------------
# Post Processing for the TZA 2010
# data. Here we make some changes to
# the data to make it possible to
# perform further analysis. This file
# is separate from the raw data processing
# file in order to make transparent any
# changes that were made to the data
# -------------------------------------

if(Sys.info()["user"] == "Tomas"){
  path2Data <- "C:/Users/Tomas/Documents/LEI/pro-gap/TZA/"
} else {
  path2Data <- "D:/Data/Projects/TZAYG/"
}

# source the data
suppressMessages(source(file.path(path2Data, "/Code/TZA_2010.R")))

# -------------------------------------
# For some questions respondents answered
# NA, it is not certain how these responses
# should be treated. Often we assume that
# an NA is equivalent to NO/0
# -------------------------------------

TZA2010$SACCO <- ifelse(TZA2010$SACCO %in% 1, 1, 0) # assume NA -> no SACCO
TZA2010$death <- ifelse(TZA2010$death %in% 1, 1, 0) # assume NA -> no death
TZA2010$one_crop <- ifelse(TZA2010$one_crop %in% 1, 1, 0) # assume NA -> no crops 
TZA2010$inter_crop <- ifelse(TZA2010$inter_crop %in% 1, 1, 0) # assume NA -> no intercropping
TZA2010$hybrd <- ifelse(TZA2010$hybrd %in% 2, 1, 0) # assume NA -> no hybrid seeds
TZA2010$title <- ifelse(TZA2010$title %in% 1, 1, 0) # assume NA -> no title
TZA2010$irrig <- ifelse(TZA2010$irrig %in% 1, 1, 0) # assume NA -> no irrigation
TZA2010$manure <- ifelse(TZA2010$manure %in% 1, 1, 0) # assume NA -> no manure
TZA2010$N <- ifelse(is.na(TZA2010$N), 0, TZA2010$N) # assume NA -> no nitrogen
TZA2010$P <- ifelse(is.na(TZA2010$P), 0, TZA2010$P) # assume NA -> no Phosphorous
TZA2010$pest <- ifelse(TZA2010$pest %in% 1, 1, 0) # assume NA -> no pesticide
TZA2010$trans <- ifelse(TZA2010$trans %in% 1, 1, 0) # assume NA -> no transportation for crop

rm("path2Data")
