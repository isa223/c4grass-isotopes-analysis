# data preparation script
# This script takes the isotope data, rainfall data, temperature data and concentration data and merges it into single csvs for analysis in otherscripts

rm(list=ls())

library(tidyverse)
library(readxl)
library(sf)
library(ggplot2)
library(dplyr)
library(lme4)
library(AICcmodavg)
library(MuMIn)


## Annual isotopes, AVG rain data, annual temperature anomally, annual concentrations 334 samples -----
# AVERAGE RAIN DATA, 341 SAMPLES: isotope and rain data 
isotopes <- read.csv('/c4grass-isotopes-analysis/data/isotopes_avgRain_0531.csv')

# clean isotope and rain data
iso = isotopes %>% 
  dplyr::distinct(year, iso_code,
                  .keep_all=TRUE) 

# getting rid of outliers
iso <- filter(iso, iso_code != 171 & iso_code != 204 & iso_code != 118)

# atmospheric carbon data
carbon <- read.csv('/c4grass-isotopes-analysis/data/merged_csiro_rubino_062021.csv')

# temperature data
temp <- read.csv(paste0(temperature, '/c4grass-isotopes-analysis/data/temperature_data_rain_avg_100321.csv'))
colnames(temp)[colnames(temp)=="iso"] <- 'iso_code'

# merge data together
# isotopes, rain & temperature
iso_rt <- merge(iso, temp, by='iso_code')

# isotopes, rain, temperature & carbon
iso_rtc <- left_join(iso_rt, carbon, by='year')

# write to csv
write_csv(iso_rtc, '/c4grass-isotopes-analysis/data/analysis/data/iso_vars_334_062021.csv')



## Annual isotopes, ANNUAL rain data, annual temperature anomally, annual concentrations 251 samples -----
# isotope and rainfall data. get rid of duplicates in iso_code
iso <- read.csv('/c4grass-isotopes-analysis/data/isotopes_annualRain_0531.csv')
names(iso)

iso = iso %>% 
  dplyr::distinct(year, iso_code,
                  .keep_all=TRUE) 

# temperature data
temp <- read.csv('/c4grass-isotopes-analysis/data/temperature_data_0621.csv')
colnames(temp)[colnames(temp)=="iso"] <- 'iso_code'

# carbon concentrations
carbon <- read.csv('/c4grass-isotopes-analysis/data/merged_csiro_rubino_062021.csv')

# merge data together and finalise cleaning
# merging iso and rain with temperture by iso_code
iso_rt <- merge(iso, temp, by='iso_code')

# merging iso, rain, temp with carbon by year
iso_rtc <- left_join(iso_rt, carbon, by='year')

# get rid of the C3 outliers
iso_rtc <- filter(iso_rtc, iso_code != 171 & iso_code != 204 & iso_code != 118)

# export this to csv 
write.csv(iso_rtc, '/c4grass-isotopes-analysis/data/iso_vars_062021.csv')
