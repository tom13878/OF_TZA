###################################################
### EXPLORATORY ANALYSIS of GHA PANEL DATA data ###
###################################################

library(plyr)
library(dplyr)
library(ggplot2)
library(stargazer)
library(haven)
library(tidyr)
library(xtable)
library(DescTools)
library(sjPlot)
library(xda)
library(car)
library(corrgram)
options(scipen=999)

#######################################
####### EXPLORATORY ANALYSIS ##########
#######################################

dbX <- db0

# summary statistics
stargazer(as.data.frame(dbX), type = "text") # as.data.frame needed because file are tbl format.
numSummary(dbX)
charSummary(dbX)

inputdb <- select(dbX, yld, N, lab, asset, P, area, area_tot, SOC2, ph2, RootDepth, slope, elevation, rain_wq, omega)
corrgram(inputdb, order=TRUE, lower.panel=panel.shade,
         upper.panel=panel.pie, text.panel=panel.txt)

# Number of observation per hhid in sample
plotobs <- db0 %>%
  group_by(hhid) %>%
  summarize(n=n())
table(plotobs$n)
sum(plotobs$n)

# Check individual variables.
# Number of plots per hh per year
plotnumber <- db0 %>%
            group_by(hhid) %>%
            summarize(n=n())

# Fung and herb

# Yield
sjp.frq(db0$yld, 
        type = "dens", 
        normal.curve = TRUE, 
        normal.curve.alpha = .3)
Freq(db0$yld)

# Area
sjp.frq(db0$area, 
        type = "dens", 
        normal.curve = TRUE, 
        normal.curve.alpha = .3)
Freq(db0$area_gps)

# Labour
sjp.frq(db0$lab, 
        type = "dens", 
        normal.curve = TRUE, 
        normal.curve.alpha = .3)

# Fertilizer
sjp.frq(db0$N, 
        type = "dens", 
        normal.curve = TRUE, 
        normal.curve.alpha = .3)
Freq(db0$N)

sjp.frq(db0$P, 
        type = "dens", 
        normal.curve = TRUE, 
        normal.curve.alpha = .3)
Freq(db0$P[db0$P>0])

# Soil variables
ggplot(data = db0, aes(x = soil, y = ph2)) + geom_boxplot() + facet_wrap(~fs)
ggplot(data = db0, aes(x = soil, y = SOC2)) + geom_boxplot() + facet_wrap(~fs)
ggplot(data = db0, aes(x = nut, y = ph2)) + geom_boxplot() + facet_wrap(~fs)
ggplot(data = db0, aes(x = nut, y = SOC2)) + geom_boxplot() + facet_wrap(~fs)


soildb <- select(dbX, nut, soil, ph2, SOC2, yld, N, fs)
corrgram(soildb, order=TRUE, lower.panel=panel.shade,
         upper.panel=panel.pie, text.panel=panel.txt)
          



# Seed
sjp.frq(db0$seed_q, 
        type = "dens", 
        normal.curve = TRUE, 
        normal.curve.alpha = .3)
Freq(db0$seed_q)
sjp.frq(db0$logseed_q, 
        type = "dens", 
        normal.curve = TRUE, 
        normal.curve.alpha = .3)

# Compare extension and extensions2
xtabs(~db0$extension + db0$extension2)


# Infrastructure
Freq(db0$infra_dummy_finance_ph)
infra <- db0 %>% group_by(ZONE) %>% summarize(infra = mean(infra_dummy_finance_ph),
                                              n=n())

# Visual relationship between variables
Plot(dbP[c("yld", "N")], "yld")
bivariate(dbP, "yld", "N")
names(dbP)

# Scatter point and smoothed line
ggplot(data = filter(db0, N>0), aes(x = N, y = yld)) + geom_point() + geom_smooth() + facet_wrap(~fs)
ggplot(data = filter(db0, N>0), aes(x = N, y = P)) + geom_point() + geom_smooth() 
ggplot(data = db0, aes(x = logN, y = logyld)) + geom_point() + geom_smooth()
ggplot(data = filter(db0, surveyyear == 2012), aes(x = seed_q, y = yld)) + geom_point() + geom_smooth()
ggplot(data = filter(db0), aes(x = N, y = rain_wq)) + geom_point() + geom_smooth() + facet_wrap(~surveyyear, scales = "free")


# Boxplots

# Cross tables for dichotomous variables
PercTable(db0$mech, db0$antrac,  margins=c(1,2),  rfrq="110", freq=T) 
PercTable(db0$bank_account_own_pp, db0$infra_dummy_finance_ph,  margins=c(1,2),  rfrq="110", freq=T) 
PercTable(db0$ZONE, db0$inter_crop,  margins=c(1,2),  rfrq="110", freq=T) 
PercTable(db0$herb, db0$pest,  margins=c(1,2),  rfrq="110", freq=T) 

# Analyse fit of model
# Note that, as expected, fitted values or on the frontier (=above).
# https://www.r-bloggers.com/visualising-residuals/?utm_source=feedburner&utm_medium=email&utm_campaign=Feed%3A+RBloggers+%28R+bloggers%29
fit <- sfaCD_CRE_Z
dbX <- db1
dbX$fitted <- fitted(fit)
dbX$residuals <- residuals(fit)

dbX <- dbX %>%
  select(hhid, id, loglab, logN, logyld, fitted, residuals) %>%
  gather(variable, value, -id, - hhid, -fitted, -residuals, -logyld)

ggplot(dbX, aes(x = value, y = logyld)) +
  geom_segment(aes(xend = value, yend = fitted), alpha = .2) +
  geom_point(aes(color = residuals)) +
  scale_color_gradient2(low = "blue", mid = "white", high = "red") +
  guides(color = FALSE) +
  geom_point(aes(y = fitted), shape = 1) +
  theme_bw() +
  facet_wrap(~variable, scales = "free")


