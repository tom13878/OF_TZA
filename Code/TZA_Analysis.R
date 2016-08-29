#######################################
###### ANALYSIS of TZA panel data #####
#######################################
######## Optimal Fertilizer  ##########
#######################################

# file path decision: Michiel or Tom
if(Sys.info()["user"] == "Tomas"){
  filePath <- "C:/Users/Tomas/Documents/LEI/OF_TZA/"
} else {
  filePath <- "D:/Data/Projects/OF_TZA/"
  dataPath <- "N:/Internationaal Beleid  (IB)/Projecten/2285000066 Africa Maize Yield Gap/SurveyData/"
  #surveyPath <- "W:\\LEI\\Internationaal Beleid  (IB)\\Projecten\\2285000066 Africa Maize Yield Gap\\SurveyData\\"
  wdPath <- "D:\\Data\\Projects\\OF_TZA"
  setwd(wdPath)
}

#######################################
############## PACKAGES ETC ###########
#######################################


library(dplyr)
library(ggplot2)
library(stargazer)
library(haven)
library(tidyr)
library(xtable)
library(sandwich)
library(lmtest)
library(AER)
library(boot)
library(purrr)

options(scipen=999)

#######################################
############## READ DATA ##############
#######################################

dbP <- readRDS("Cache/Pooled_TZA.rds")

#######################################
############## CLEANING ###############
#######################################

# Select maize
dbP <- filter(dbP, crop_code == 11 & status == "HEAD"); #rm(TZA2010, TZA2012, TZA2010_2, TZA2012_2) 

# Create rel_harv_area variable
dbP$area_farmer[dbP$area_farmer %in% 0] <- NA
dbP$harv_area[dbP$harv_area %in% 0] <- NA

dbP <- dbP %>% 
  mutate( sh_harv_area = harv_area/area_farmer,
          sh_harv_area = ifelse(sh_harv_area >1, 1, sh_harv_area), # for some farmers the harvested area > plot size. Set to 1
          rel_harv_area = sh_harv_area * area_gps)


# Create id for plots
dbP <- dbP %>% 
  mutate(id=1:dim(.)[1]) 


# Cleaning and analysis depends strongly on which measure is chosen for area, which is the denominator for many variables.
# there are three possible yield variables. 
# that can be created for the last two waves of data. 
# 1. yld1: above uses the full gps areas as denominator
# 2. yld2: uses harvested area as denominator
# 3. yld3: Uses relative harvest area to correct gps area
# To simplify the code we set these values in this part. Subsequent analysis code can then be used for any definition of yield.

# We choose area_gps as this is the variable used by other comparable studies
dbP <- dbP %>% 
  mutate(area = area_gps, 
         yld = crop_qty_harv/area,
         N = N/area)


# Remove Zanzibar
dbP <- filter(dbP, !(ZONE %in% c("ZANZIBAR")))

# cap yield at 16040 kg/ha, the highest potential yield in TZA (not water limited)
dbP <- filter(dbP, yld <=16040)

# Sample includes very small plots <0.005 ha that bias result upwards and very large >35 ha. 
# As we focus on small scale farmers we restrict area size
dbP <- filter(dbP, area_gps >=0.05 & area_gps <=10)

# restrict attention to plots that use N < 1000. There large outliers that are removed.
dbP <- dplyr::filter(dbP, N < 1000)


######################################
##### Get plot specific pricess ######
######################################

# Read price data
prices <- readRDS("Cache/TZA_prices.rds")
db0 <- left_join(dbP, prices)


######################################
######## Modify and add variables ####
######################################

# Select relevant variables and complete cases
db0 <- db0 %>% 
  dplyr::select(hhid, id, ZONE, REGCODE, REGNAME, DISCODE, DISNAME, plotnum,
                yld, N, P, area, area_tot, AEZ, asset, lab, hybrd, manure, pest, soil, legume, irrig, 
                Pc, Pn, Pns, Pnm, 
                sex, age, 
                crop_count, surveyyear, 
                SOC, SOC2, ph, ph2, RootDepth, 
                slope, elevation, nut, nrc, fs,
                rain_year, rain_wq, 
                rural, area_gps, lat, lon) %>%
  do(filter(., complete.cases(.)))

# Following Burke
db0$phdum[db0$ph < 55] <- 1
db0$phdum[db0$ph >= 55 & db0$ph <=70] <- 2 # Neutral and best suited for crops
db0$phdum[db0$ph > 70] <- 3
db0$phdum <- factor(db0$phdum)


db0$phdum2[db0$ph2 < 55] <- 1
db0$phdum2[db0$ph2 >= 55 & db0$ph2 <=70] <- 2
db0$phdum2[db0$ph2 > 70] <- 3
db0$phdum2 <- factor(db0$phdum2)

# # Recode AEZ into 3 zones
# db0$AEZ2 <- db0$AEZ
# db0$AEZ2 <- mapvalues(db0$AEZ2, from = c("Tropic - cool / humid"), to = c("Tropic - cool / subhumid"))
# db0$AEZ2 <- mapvalues(db0$AEZ2, from = c("Tropic - warm / humid"), to = c("Tropic - warm / subhumid"))
# db0$AEZ2 <- mapvalues(db0$AEZ2, from = c("Tropic - cool / semiarid"), to = c("Semi-arid"))
# db0$AEZ2 <- mapvalues(db0$AEZ2, from = c("Tropic - warm / semiarid"), to = c("Semi-arid"))
# db0$AEZ2 <- factor(db0$AEZ2)


# Crop count > 1
db0$crop_count2[db0$crop_count==1] <- 1
db0$crop_count2[db0$crop_count>1] <- 0


# -------------------------------------
# Inflate 2011 prices to 2013 prices: assets
# using inflation rate for 2011 and 2013. These years were selected as the main part of the survey takes place in these years.
# from world bank:
# http://data.worldbank.org/indicator/FP.CPI.TOTL.ZG/countries/TZ?display=graph
# -------------------------------------

inflation <- read.csv(file.path(paste0(dataPath,"Other/Inflation/inflation.csv")))
rate2011 <- inflation$inflation[inflation$code=="TZ" & inflation$year==2011]/100
rate2012 <- inflation$inflation[inflation$code=="TZ" & inflation$year==2012]/100

inflate <- (1 + rate2011)*(1 + rate2012)

db0 <- mutate(db0, asset = ifelse(surveyyear == 2010, asset*inflate, asset))

# additional variables
db0 <- db0 %>% mutate (yesN = ifelse(N>0, 1,0), # Dummy when plot does not use fertilizer, following approach of Battese (1997)
                       noN = ifelse(N<=0, 1,0), # Dummy when plot does not use fertilizer, following approach of Battese (1997)
                       N2 = N*N,
                       P = P/area,
                       P2 = P*P,
                       lab=lab/area,
                       assetph=asset/area_tot,
                       logasset = log(assetph),
                       rain_wq2 = rain_wq*rain_wq,
                       rain_year2 = rain_year*rain_year,
                       surveyyear2 = replace(surveyyear==2010, 1, 0),
                       nut = factor(nut, levels = c(0, 1, 2, 3, 7), labels = c("ocean", "no or slight constraint", "moderate constraint", "severe constraint", "water"))
)

# drop unused levels
db0 <- droplevels(db0)

# add separate dummy for soil instead of factor for CRE
db0 <- cbind(db0, model.matrix( ~ soil - 1, data=db0)) 

# Add CRE variables
db0 <- db0 %>%
        group_by(hhid) %>%
        mutate(lab_bar=mean(lab, na.rm=TRUE),
             N_bar=mean(N, na.rm=TRUE),
             P_bar=mean(P, na.rm=TRUE),
             noN_bar=mean(noN, na.rm=TRUE),
             logasset_bar = mean(logasset, na.rm=TRUE),
             area_bar=mean(area, na.rm=TRUE),
             manure_bar=mean(manure, na.rm=TRUE),
             irrig_bar=mean(irrig, na.rm = TRUE),
             pest_bar=mean(pest, na.rm = TRUE),
             hybrd_bar=mean(hybrd, na.rm = TRUE),
             legume_bar=mean(legume, na.rm=TRUE),
             crop_count_bar=mean(crop_count2, na.rm=TRUE),
             rain_wq_bar=mean(rain_wq, na.rm=TRUE),
             soilSandy_bar=mean(soilSandy, na.rm=TRUE),
             soilLoam_bar=mean(soilLoam, na.rm=TRUE),
             soilClay_bar=mean(soilClay, na.rm=TRUE),
             soilOther_bar=mean(soilOther, na.rm=TRUE))


# Drop unused levels (e.g. Zanzibar in zone), which are giving problems with sfa
db0 <- droplevels(db0)


#######################################
############## ANALYSIS ###############
#######################################


# -------------------------------------
# first step in the estimation deals with
# the time varying endogeneity by using
# the CFA with a number of possible
# variables as instruments. We then keep the
# residuals and use them in the next stage
# -------------------------------------

######################################################
### FIRST STAGE ANALYSIS: NITROGEN DEMAND FUNCTION ###
######################################################

# NB MODEL IS VERY SENSITIVE TO DUMMIES/MULTICOLINEARITY?!
N_dem <- tobit(N ~ 
                      #split_prez10 + 
                      logasset + lab + area + hybrd +
                      manure + pest + legume + soil +
                      SOC2 + phdum2 + 
                      I(Pn/Pc) +
                      #nut + fs +
                      sex + age + crop_count + surveyyear2 +
                      #rural + rain_wq + REGNAME +
                      #N_bar + logasset_bar +  lab_bar + area_bar + 
                      #hybrd_bar + manure_bar + pest_bar +  legume_bar +
                      #soilSandy_bar + soilLoam_bar + soilClay_bar + soilOther_bar +
                      crop_count_bar, data=db0)

# We can directly read the tobit model
# results, but to make them comparable
# to OLS we need the average partial effects
# (APEs). SO we calcualte the scale factor
# and multiply the coefficients by it
sigma <- N_dem$scale
X <- model.matrix(N_dem)
n <- nrow(X)
APE <- (1/n) * sum(pnorm(((X %*% coef(N_dem)))/sigma, 0, 1))

# but for the next stage we need the residuals
# which can then be incorporated into the 
# next stage of the analysis
db0$omega <- residuals(N_dem)

stargazer(N_dem, type = "text", intercept.bottom = FALSE, digits = 2, digits.extra = 2)

######################################################
### SECOND STAGE ANALYSIS: YIELD RESPONSE FUNCTION ###
######################################################

# Yield response function estimation
# NB it appears that there is only limited variation between SOC2 and phdum2=1 and AEZ2 so phdum21 drops out.
# Cannot introduce too many interaction terms with N because of limited N application and variability.
# N is only used in several regions. We run regressions for the whole country and regions where fertilizer is used (see Nuse above).

# Interaction terms for soil constraints, P values and others
OLS0 <- lm(yld ~ N + N2 + 
             logasset + lab + area + 
             hybrd + manure + pest + legume +
             soil + 
             SOC2 + phdum2 + fs +
             elevation + slope +
             rain_wq + 
             crop_count + surveyyear2 +  
             #REGNAME +
             N_bar + logasset_bar +  lab_bar + area_bar + 
             hybrd_bar + manure_bar + pest_bar +  legume_bar +
             soilSandy_bar + soilLoam_bar + soilClay_bar + soilOther_bar +
             rain_wq_bar +
             crop_count_bar +
             omega
           ,data = db0)

# http://jakeruss.com/cheatsheets/stargazer.html

# Robust - white - sandwhich se.
# http://stackoverflow.com/questions/23217681/unprecise-p-values-in-stargazer
# Adjust se en p
robust0  <- coeftest(OLS0, vcov = vcovHC(OLS0, type="HC1")) 
ses0 <- robust0[, 2]
pvals0 <- robust0[, 4]

stargazer(OLS0, type = "text", 
          p = list(pvals0), se = list(ses0),
          intercept.bottom = FALSE, digits = 2, digits.extra = 2)


OLS1 <- lm(yld ~ N + N2 + N:P +
             N:soil + 
             N:SOC2 + N:phdum2 +
             rain_wq + I(rain_wq*rain_wq) +
             logasset + lab + area + 
             hybrd + manure + pest + legume +
             elevation + slope +
             crop_count + surveyyear2 +  
             REGNAME +
             N_bar + logasset_bar +  lab_bar + area_bar + 
             hybrd_bar + manure_bar + pest_bar +  legume_bar +
             soilSandy_bar + soilLoam_bar + soilClay_bar + soilOther_bar +
             rain_wq_bar + crop_count_bar +
             omega
             ,data = db0)


# Adjust se en p
robust1  <- coeftest(OLS1, vcov = vcovHC(OLS1, type="HC1")) 
ses1 <- robust1[, 2]
pvals1 <- robust1[, 4]


OLS2 <- lm(yld ~ N + N2 + N:P +
             N:soil + N:fs +
             N:SOC2 + N:phdum2 +
             hybrd:N +
             rain_wq + I(rain_wq*rain_wq) +
             logasset + lab + area + 
             hybrd + manure + pest + legume +
             elevation + slope +
             crop_count + surveyyear2 +  
             REGNAME +
             N_bar + logasset_bar +  lab_bar + area_bar + 
             hybrd_bar + manure_bar + pest_bar +  legume_bar +
             soilSandy_bar + soilLoam_bar + soilClay_bar + soilOther_bar +
             rain_wq_bar + crop_count_bar +
             omega
           ,data = db0)

# Adjust se en p
robust2  <- coeftest(OLS2, vcov = vcovHC(OLS2, type="HC1")) 
ses2 <- robust2[, 2]
pvals2 <- robust2[, 4]


#db0 <- filter(db0, !(nut %in% c("ocean", "water")))

OLS3 <- lm(yld ~  N:P + 
             N:soil + 
             N:fs + N2:fs +
             N:SOC2 + 
             N:phdum2 + 
             N:hybrd + 
             N:rain_wq + N:I(rain_wq*rain_wq) +
             asset + I(asset*asset)+ lab + I(lab*lab) + area + I(area*area) + 
             hybrd + manure + pest + legume +
             elevation + slope +
             crop_count + surveyyear2 +  
             REGNAME +
             N_bar + logasset_bar +  lab_bar + area_bar + 
             hybrd_bar + manure_bar + pest_bar +  legume_bar +
             soilSandy_bar + soilLoam_bar + soilClay_bar + soilOther_bar +
             rain_wq_bar + crop_count_bar +
             omega
           ,data = db0)

# Adjust se en p
robust3  <- coeftest(OLS3, vcov = vcovHC(OLS3, type="HC1")) 
ses3 <- robust3[, 2]
pvals3 <- robust3[, 4]


stargazer(OLS1, OLS2, OLS3, type = "text", 
          p = list(pvals1, pvals2, pvals3), se = list(ses1, ses2, ses3),
          intercept.bottom = FALSE, digits = 2, digits.extra = 2)


OLS4 <- lm(yld ~  
             N:soil + 
             N:fs + N2:fs +
             N:SOC2 + 
             N:phdum2 + 
             N:hybrd + 
             N:rain_wq + N:I(rain_wq*rain_wq) +
             asset + I(asset*asset)+ lab + I(lab*lab) + area + I(area*area) + 
             hybrd + manure + pest + legume +
             elevation + slope +
             crop_count + surveyyear2 +  
             REGNAME +
             N_bar + logasset_bar +  lab_bar + area_bar + 
             hybrd_bar + manure_bar + pest_bar +  legume_bar +
             soilSandy_bar + soilLoam_bar + soilClay_bar + soilOther_bar +
             rain_wq_bar + crop_count_bar +
             omega
           ,data = db0)

# Adjust se en p
robust4  <- coeftest(OLS4, vcov = vcovHC(OLS4, type="HC1")) 
ses4 <- robust4[, 2]
pvals4 <- robust4[, 4]

stargazer(OLS3, OLS4, type = "text", 
          p = list(pvals3, pvals4), se = list(ses3, ses4),
          intercept.bottom = FALSE, digits = 2, digits.extra = 2)



OLS5 <- lm(yld ~  
             N + P +  asset + lab + area + rain_wq + ph2 + SOC2 + elevation + slope +
             N2 + I(asset*asset)+ I(lab*lab) + I(area*area) + I(rain_wq*rain_wq) + I(ph2*ph2) + I(SOC2*SOC2) + I(elevation*elevation) + I(slope*slope) +
             N:P + N:asset + N:lab + N:area + N:rain_wq + N:ph2 + N:SOC2 + N:elevation + N:slope +
             N:soil + soil +
             N:fs + fs + 
             N:SOC2 + N:phdum2 + 
             N:hybrd + hybrd + 
             N:manure + manure + pest + N:pest + legume + N:legume +
             crop_count + surveyyear2 +  
             REGNAME +
             N_bar + logasset_bar +  lab_bar + area_bar + 
             hybrd_bar + manure_bar + pest_bar +  legume_bar +
             soilSandy_bar + soilLoam_bar + soilClay_bar + soilOther_bar +
             rain_wq_bar + crop_count_bar +
             omega
           ,data = db0)

# Adjust se en p
robust5  <- coeftest(OLS5, vcov = vcovHC(OLS5, type="HC1")) 
ses5 <- robust5[, 2]
pvals5 <- robust5[, 4]

stargazer(OLS5, type = "text", 
          p = list(pvals5), se = list(ses5),
          intercept.bottom = FALSE, digits = 2, digits.extra = 2)


# Regressions per agricultural system

unique(db0$fs)
hp <- filter(db0, fs == "Highland perennial")
af <- filter(db0, fs == "Artisanal fishing")
ap <- filter(db0, fs == "Agro-pastoral")
ir <- filter(db0, fs == "Irrigated")
rt <- filter(db0, fs == "Root and tuber crop")
mm <- filter(db0, fs == "Maize mixed")

db0 %>%
  filter(fs %in% c("Maize mixed")) %>%
  split(.$fs) %>%
  map(~lm(form, data = .)) %>%
  map(summary)

%>%
  map_dbl("r.squared")

#################################
### BOOTSTRAP STANDARD ERRORS ###
#################################

# NB I am having problems bootstrapping factor variables, gives an error
# NB2 Can we and do we need to bootstrap p values? I am finding mixed opinions on internet.
# It seems to be calculated as the proportion of values 
# http://stats.stackexchange.com/questions/20701/computing-p-value-using-bootstrap-with-r
# http://www.burns-stat.com/documents/tutorials/the-statistical-bootstrap-and-other-resampling-methods-2/#permtest
# http://stats.stackexchange.com/questions/83012/how-to-obtain-p-values-of-coefficients-from-bootstrap-regression
# http://www.stat.ucla.edu/~rgould/110as02/bshypothesis.pdf
# Bootstrap function
se_b <- function(formula, data, indices){
  d <- data[indices,]
  fit <- lm(formula, data = d)
  se <- sqrt(diag(vcov(fit)))
  return(coef(fit))
}

# Function to bootstrap
form <- yld ~ N + N2 + N:P +
  N:soil + 
  N:SOC2 + N:phdum2 +
  #rain_wq + I(rain_wq*rain_wq) +
  #logasset + lab + area + 
  hybrd + manure + pest + legume +
  elevation + slope +
  crop_count + surveyyear2 +  
  REGNAME +
  N_bar + logasset_bar +  lab_bar + area_bar + 
  hybrd_bar + manure_bar + pest_bar +  legume_bar +
  soilSandy_bar + soilLoam_bar + soilClay_bar + soilOther_bar +
  rain_wq_bar + crop_count_bar +
  omega 



form <- yld ~N + N2 
OLS2_b <- boot(data = db0, statistic = se_b, R = 1000, formula = form )
summary(OLS2_b)
plot(OLS2_b, index=2) # N variable 
se_b <- sqrt(diag(cov(OLS2_b$t))) # same as se in boot results

# Compare se
OLStest <- lm(form, data = db0)
stargazer(OLStest, OLStest, type = "text", 
          se = list(NULL, se_b),
          intercept.bottom = FALSE, digits = 2, digits.extra = 2)




table(db0$rain_wq, db0$AEZ2)
ggplot(data = db0, aes(x=rain_wq, y = yld)) + geom_point() + facet_wrap(~AEZ2)


### Average Marginal Product
model <- OLS1
b1_tc = coef(model)["AEZ2Tropic-cool / sub-humid:N"]
b1_sa = coef(model)["AEZ2Semi-arid:N"]
b1_tw = coef(model)["AEZ2Tropic-warm / sub-humid:N"]

b2_tc = 2*coef(model)["AEZ2Tropic-cool / sub-humid:N2"]
b2_sa = 2*coef(model)["AEZ2Semi-arid:N2"]
b2_tw = 2*coef(model)["AEZ2Tropic-warm / sub-humid:N2"]

#b3_SOC = coef(model)["N:SOC2"]

#b4_phdum22 = coef(model)["N:phdum22"]
#b4_phdum23 = coef(model)["N:phdum23"]

b5_soilLoam = coef(model)["N:soilLoam"]
b5_soilClay = coef(model)["N:soilClay"]
b5_soilOther = coef(model)["N:soilOther"]

# b6_modcons = coef(model)["N:soil05moderate constraint"]
# b6_sevcons = coef(model)["N:soil05severe constraint"]



# Calculate on transport cost of fertilizer per zone/region
# CHECK definition of transport cost
load(file.path(dataPath, "TZA_HH12_data.RData"))

transportcost <- filter(vo, inputname %in% c("UREA", "DAP") & input ==1 & !is.na(inputcost) & !is.na(trancost) & trancost>0) %>%
  group_by(zone, region) %>%
  summarize(transhare = mean(trancost/(inputcost+trancost)*100),
            N=n())

transportcost2 <- filter(vo, inputname %in% c("UREA", "DAP") & input ==1 & !is.na(inputcost) & !is.na(trancost) & trancost>0 ) %>%
  mutate(transhare = trancost/(inputcost+trancost)*100,
         N=n())


# Merge price data
load(file.path(dataPath, "Prices.RData")) 
Prices <- Prices %>% dplyr:: select(zone, region, surveyyear, price, type) %>%
  spread(type, price)

db0.1 <- left_join(db0, Prices)
