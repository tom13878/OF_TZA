# -------------------------------------
# Post Processing for the TZA 2012
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


# LEI Path
# path2Data <- "W:/LEI/Internationaal Beleid  (IB)/Projecten/2285000066 Africa Maize Yield Gap/SurveyData/Code/TZA"

# source the data
suppressMessages(source(file.path(path2Data, "Code/TZA_2012.R")))

# -------------------------------------
# For some questions respondents answered
# NA, it is not certain how these responses
# should be treated. Often we assume that
# an NA is equivalent to NO/0
# -------------------------------------

TZA2012$SACCO <- ifelse(TZA2012$SACCO %in% 1, 1, 0) # assume NA -> no SACCO
TZA2012$death <- ifelse(TZA2012$death %in% 1, 1, 0) # assume NA -> no death
TZA2012$one_crop <- ifelse(TZA2012$one_crop %in% 1, 1, 0) # assume NA -> no crops 
TZA2012$inter_crop <- ifelse(TZA2012$inter_crop %in% 1, 1, 0) # assume NA -> no intercropping
TZA2012$hybrd <- ifelse(TZA2012$hybrd %in% 1, 1, 0) # assume NA -> no hybrid seeds
TZA2012$title <- ifelse(TZA2012$title %in% c(1:10), 1, 0) # assume NA -> no title
TZA2012$irrig <- ifelse(TZA2012$irrig %in% 1, 1, 0) # assume NA -> no irrigation
TZA2012$manure <- ifelse(TZA2012$manure %in% 1, 1, 0) # assume NA -> no manure
TZA2012$N <- ifelse(is.na(TZA2012$N), 0, TZA2012$N) # assume NA -> no nitrogen
TZA2012$P <- ifelse(is.na(TZA2012$P), 0, TZA2012$P) # assume NA -> no Phosphorous
TZA2012$pest <- ifelse(TZA2012$pest %in% 1, 1, 0) # assume NA -> no pesticide
TZA2012$trans <- ifelse(TZA2012$trans %in% 1, 1, 0) # assume NA -> no transportation for crop

rm("path2Data")

