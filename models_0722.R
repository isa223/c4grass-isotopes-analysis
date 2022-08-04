# 062021
# this script runs the models for isotope data with annual rainfall, annual temperature & annual carbon as variables

rm(list=ls())

library(tidyverse)
library(readxl)
library(sf)
library(ggplot2)
library(dplyr)
library(lme4)
library(AICcmodavg)
library(MuMIn)
library(Hmisc)

## setting directories
# data directory
dir <- '/Users/IsadT/Desktop/Carbon/data'
rain <- paste0(dir,'/rainfall')
atmos <- paste0(dir, '/c4grass-isotopes-atmospheric-C-concentrations')
temperature <- paste0(dir, '/c4grass-isotopes-temperature')
analysis <- paste0(dir,'/analysis')

## load isotope, rainfall, concentration, and temperature as separate csvs ------
# isotope and rainfall data. get rid of duplicates in iso_code
iso <- read.csv(paste0(rain,'/isotopes_annualRain_0531.csv'))
names(iso)

iso = iso %>% 
  dplyr::distinct(year, iso_code,
                  .keep_all=TRUE) 

# temperature data
temp <- read.csv(paste0(temperature, '/outputs/temperature_data_0621.csv'))
colnames(temp)[colnames(temp)=="iso"] <- 'iso_code'

# carbon concentrations
carbon <- read.csv(paste0(atmos, '/merged_csiro_rubino_062021.csv'))

# merge data together and finalise cleaning
# merging iso and rain with temperture by iso_code
iso_rt <- merge(iso, temp, by='iso_code')

# merging iso, rain, temp with carbon by year
iso_rtc <- left_join(iso_rt, carbon, by='year')

# get rid of the C3 outliers
iso_rtc <- filter(iso_rtc, iso_code != 171 & iso_code != 204 & iso_code != 118)

# export this to csv 
write.csv(iso_rtc, '/Users/IsadT/Desktop/Carbon/data/analysis/iso_vars_062021.csv')



## load final merged data set, isotope data with all environmental variables----
iso_rtc <- read.csv(paste0(dir, '/iso_vars_251_062021.csv'))

## Subset data for different groups of models
# data is subsetted according to different groups of models. 
# (1) Both pathways,1892-2009; 
# (2) Both pathways, 1950-2009; 
  iso_rtc_50 <- iso_rtc %>% filter(year>=1950)
# (3) C4, 1892-2009;
  c4_data <- iso_rtc %>% filter(pathway=='C4')
# (4) C4, 1950-2009;
  c4_50 <- c4_data %>% filter(year>=1950)
# (5) C3, 1892-2009;
  c3_data <- iso_rtc %>% filter(pathway=='C3')
# (6) C3, 1950-2009;
  c3_50 <- c3_data %>% filter(year>=1950)

## Run the models
# (1) both pathways, (1892-2009)-----
all.parms_both_at<-lmer(delta.disc ~ pathway + wetrain + wetlambda + wetlength + avg_temp_anomaly + co2_ppm +
                  pathway*wetrain + pathway*wetlambda + pathway*wetlength + pathway*avg_temp_anomaly + pathway*co2_ppm +
                  wetrain*wetlambda + wetrain*wetlength + wetrain*avg_temp_anomaly + wetrain*co2_ppm +
                  wetlambda*wetlength + wetlambda*avg_temp_anomaly + wetlambda*co2_ppm +
                  wetlength*avg_temp_anomaly + wetlength*co2_ppm +
                  avg_temp_anomaly*co2_ppm + (1 | sp_code) + (1 | qds), 
                  data = iso_rtc, na.action=na.fail)

results_both_at <- dredge(all.parms_both_at)
write.csv(results_both_at, 'results_both_at_0806.csv')

# (2) both pathways, (1950-2009)-----
all.parms_both_1950 <-lmer(delta.disc ~ pathway + wetrain + wetlambda + wetlength + avg_temp_anomaly + co2_ppm +
                             pathway*wetrain + pathway*wetlambda + pathway*wetlength + pathway*avg_temp_anomaly + pathway*co2_ppm +
                             wetrain*wetlambda + wetrain*wetlength + wetrain*avg_temp_anomaly + wetrain*co2_ppm +
                             wetlambda*wetlength + wetlambda*avg_temp_anomaly + wetlambda*co2_ppm +
                             wetlength*avg_temp_anomaly + wetlength*co2_ppm +
                             avg_temp_anomaly*co2_ppm + (1 | sp_code) + (1 | qds), 
                           data = iso_rtc_50, na.action=na.fail)

results_both_50 <- dredge(all.parms_both_1950)
write.csv(results_both_50, 'results_both_50_0806.csv')


# (3) only c4, (1892-2009)-----
all.parms_c4_at <-lmer(delta.disc ~ wetrain + wetlambda + wetlength + avg_temp_anomaly + co2_ppm +
                             wetrain*wetlambda + wetrain*wetlength + wetrain*avg_temp_anomaly + wetrain*co2_ppm +
                             wetlambda*wetlength + wetlambda*avg_temp_anomaly + wetlambda*co2_ppm +
                             wetlength*avg_temp_anomaly + wetlength*co2_ppm +
                             avg_temp_anomaly*co2_ppm + (1 | sp_code) + (1 | qds), 
                           data = c4_data, na.action=na.fail)
results_c4_at <- dredge(all.parms_c4_at)
write.csv(results_c4_at, 'results_c4_at_0806.csv')

# (4) only c4, (1950-2009) -----
c4_50 <- c4_data %>% filter(year>=1950)
all.parms_c4_50 <-lmer(delta.disc ~ wetrain + wetlambda + wetlength + avg_temp_anomaly + co2_ppm +
                         wetrain*wetlambda + wetrain*wetlength + wetrain*avg_temp_anomaly + wetrain*co2_ppm +
                         wetlambda*wetlength + wetlambda*avg_temp_anomaly + wetlambda*co2_ppm +
                         wetlength*avg_temp_anomaly + wetlength*co2_ppm +
                         avg_temp_anomaly*co2_ppm + (1 | sp_code) + (1 | qds), 
                       data = c4_50, na.action=na.fail)
results_c4_50 <- dredge(all.parms_c4_50)
write.csv(results_c4_at, 'results_c4_50_0806.csv')

# (5) only c3, (1892-2009) ----
c3_data <- iso_rtc %>% filter(pathway=='C3')
all.parms_c3_at <-lmer(delta.disc ~ wetrain + wetlambda + wetlength + avg_temp_anomaly + co2_ppm +
                         wetrain*wetlambda + wetrain*wetlength + wetrain*avg_temp_anomaly + wetrain*co2_ppm +
                         wetlambda*wetlength + wetlambda*avg_temp_anomaly + wetlambda*co2_ppm +
                         wetlength*avg_temp_anomaly + wetlength*co2_ppm +
                         avg_temp_anomaly*co2_ppm + (1 | sp_code) + (1 | qds), 
                       data = c3_data, na.action=na.fail)
results_c3_at <- dredge(all.parms_c3_at)
write.csv(results_c3_at, 'results_c3_at_0806.csv')

# (6) only c3, (1950-2009)-----
c3_50 <- c3_data %>% filter(year >= 1950)
all.parms_c3_50 <-lmer(delta.disc ~ wetrain + wetlambda + wetlength + avg_temp_anomaly + co2_ppm +
                         wetrain*wetlambda + wetrain*wetlength + wetrain*avg_temp_anomaly + wetrain*co2_ppm +
                         wetlambda*wetlength + wetlambda*avg_temp_anomaly + wetlambda*co2_ppm +
                         wetlength*avg_temp_anomaly + wetlength*co2_ppm +
                         avg_temp_anomaly*co2_ppm + (1 | sp_code) + (1 | qds), 
                       data = c3_50, na.action=na.fail)
results_c3_50 <- dredge(all.parms_c3_50)
write.csv(results_c3_50, 'results_c3_50_0806.csv')










# correlation matrix ----
iso_cor <- select(iso_rtc, 'delta.disc', 'wetlambda', 'avg_temp_anomaly', 'co2_ppm', 'wetlength', 'wetrain')
cor_mtx <- cor(iso_cor)
cor_mtx <- round(cor_mtx, 2)
write.csv(cor_mtx, '/Users/IsadT/Desktop/Carbon/data/correlation_matrix.csv')


# p-values, correlation matrix
corr_p <- rcorr(as.matrix(iso_cor))
corr_p
write.csv(corr_p, '/Users/IsadT/Desktop/Carbon/data/correlation_p_matrix.csv')



# scratch code -----

names(iso_rtc)

all.parms<-lmer(delta.disc ~ wetlength + wetrain + pathway +
                  wetlambda + avg_temp_anomaly + co2_ppm + (1 | sp_code) + (1 | qds), data = iso_rtc, na.action=na.fail)

results <- dredge(all.parms)



all.parms<-lmer(delta.disc ~ pathway + wetrain + wetlambda + wetlength + avg_temp_anomaly + co2_ppm +
                  pathway*wetrain + pathway*wetlambda + pathway*wetlength + pathway*avg_temp_anomaly + pathway*co2_ppm +
                  wetrain*wetlambda + wetrain*wetlength + wetrain*avg_temp_anomaly + wetrain*co2_ppm +
                  wetlambda*wetlength + wetlambda*avg_temp_anomaly + wetlambda*co2_ppm +
                  wetlength*avg_temp_anomaly + wetlength*co2_ppm +
                  avg_temp_anomaly*co2_ppm + (1 | sp_code) + (1 | qds), 
                data = iso_rtc, na.action=na.fail)

importance(results)




names(iso_rtc)