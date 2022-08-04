# 05032022
# this script runs the model summaries for isotope data with annual rainfall, CO2 and temperature anomalies. 

rm(list=ls())

library(tidyverse)
library(readxl)
library(sf)
library(ggplot2)
library(dplyr)
library(lme4)
library(AICcmodavg)
library(MuMIn)

## Set directories
dir = "/Users/IsadT/Desktop/Carbon/data/"
rain <- paste0(dir,'/rainfall')
atmos <- paste0(dir, '/c4grass-isotopes-atmospheric-C-concentrations')
temperature <- paste0(dir, '/c4grass-isotopes-temperature')
analysis <- paste0(dir,'/analysis')

## load data
# dC data with ANNUAL rainfall, annual temperture anomally, annual carbon concentrations
iso_rtc <- read.csv(paste0(dir, '/iso_vars_251_062021.csv'))

# dC data with AVERAGE rainfall, annual temperture anomally, annual carbon concentrations
iso_rtc <- read.csv(paste0(dir, '/iso_vars_334_062021.csv'))

## subsetting the data for the different groups of models -----
iso_c4 <- filter(iso_rtc, pathway=='C4')
iso_c4_50 <- filter(iso_c4, year>=1950)

iso_c3 <- filter(iso_rtc, pathway=='C3')
iso_c3_50 <- filter(iso_c3, year>=1950)


## models summaries ----
# C4 on [CO2] + storm depth, 1892-2009
model_a <- lmer(delta.disc ~ wetlambda + co2_ppm + (1 | sp_code) + (1 | qds),  data = iso_c4, na.action=na.fail)

summary(model_a)
print(anova(model_a, ddf="Satterthwaite"))
summary(anova(model_a, ddf="Satterthwaite"))
corr(model_a)
r.squaredGLMM(model_a)


# C4 on [CO2], 1950-2009
model_b <- lmer(delta.disc ~ co2_ppm + (1 | sp_code) + (1 | qds),  data = iso_c4_50, na.action=na.fail)

summary(model_b)
print(anova(model_b, ddf="Satterthwaite"))
summary(anova(model_b, ddf="Satterthwaite"))
corr(model_b)
r.squaredGLMM(model_b)

# C3 null, 1829-2009
model_c <- lmer(delta.disc ~ 1 + (1 | sp_code) + (1 | qds),  data = iso_c3, na.action=na.fail)
summary(model_c)

# C3 null, 1950-2009
model_d <- lmer(delta.disc ~ 1 + (1 | sp_code) + (1 | qds),  data = iso_c3_50, na.action=na.fail)
summary(model_d)

