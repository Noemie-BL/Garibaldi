# Aims:
# 1. Import GPS with elev, transect, and quadrat data
# 2. Match transect-level & GPS data to quadrat-level data
# 3. Check data
# 4. Save compiled datasets

# Author: Nathalie Chardon
# Date created: 11 Nov 2022
# Date updated: 11 Nov 2022

# # LIBRARIES # #
library(tidyverse)
library(dplyr)
library(lattice)


rm(list=ls()) 


# # WORKING DIRECTORIES # #
raw_dat <- '~/Desktop/Code/Garibaldi/trampling_analyses/raw_data/' #WD for NC
comp_dat <- '~/Desktop/Code/Garibaldi/trampling_analyses/compiled_data/' #WD for NC


# # INPUT FILES # #
setwd(raw_dat)
gps.raw <- read.table('GPS_with-elev.txt', header = T, fill = T) #lat, long, elev, ID for each transect

## *** add transect data
# trans.raw <- read.csv('')

## *** add quad data
quad.raw <- read.csv('Trampling-QUADS_data_EDITED.csv')


# # OUTPUT FILES # #

## *** save this when script is run
# load('quad.R') #gps & transect data matched to quad data (merge_fielddata.R)



####################################################################################################

# # GPS COORDINATES DATA # # 

####################################################################################################

# Manual data processing steps: 
# 1. Exported GPX file from Gaia GPS app on iOS (WGS 84, ESPG: 4326)
# 2. Converted GPX to CSV with https://mygeodata.cloud/converter/gpx-to-csv
# 3. Ordered columns lat, long, name and removed all other columns
# 4. Added elevation data with https://www.gpsvisualizer.com/convert_input?form:add_elevation=auto&convert_format=gpx&form=elevation&units=metric
# DEM used in conversion: USGS NED1, 30 m resolution


# Look at data

head(gps.raw)


# Keep only relevant columns & rename

gps <- gps.raw %>% 
  select(latitude, longitude, altitude, transect = X.m.)

length(unique(gps$transect)) # should have 28 transects + 2 P. albicaulis points + garbage pile cam = 31

plot(latitude ~ longitude, data = gps)


# Check consistency between dataframes

nrow(anti_join(quad.raw, gps, by = 'transect')) #505
nrow(anti_join(gps, quad.raw, by = 'transect')) #25 --> should only be 3


# PROBLEM 1: Transects labeled differently
# SOLUTION: Rename transects manually in CSV file

# PROBLEM 2: Most transects missing

foo <- anti_join(gps, quad.raw, by = 'transect')
unique(foo$transect) #missing transects




####################################################################################################

# # IMPORT TRANSECT & QUAD DATA # # 

####################################################################################################

setwd(raw_dat)

## *** add transect data



# Check consistency between dataframes

nrow(anti_join(trans.raw, quad.raw, by = 'transect')) #
nrow(anti_join(quad.raw, trans.raw, by = 'transect')) #




####################################################################################################

# # MERGE DATAFRAMES # # 

####################################################################################################

# Join GPS to transect data

trans <- left_join(trans.raw, gps, by = 'transect') 


# Join transect to quad data

quad <- left_join(quad.raw, trans, by = 'transect') 




####################################################################################################

# # CHECK DATA # # 

####################################################################################################

# Check data distribution to check for overdispersion & 0-inf

# for loop of histograms for all response variables


# Check for outliers (code modified from Zurr et al. 2009)

# Z <- cbind(quad$var1, ....)
# 
# colnames(Z) <- c('var1', ...)

dotplot(as.matrix(Z), groups = FALSE,
        strip = strip.custom(bg = 'white',
                             par.strip.text = list(cex = 0.8)),
        scales = list(x = list(relation = "free"),
                      y = list(relation = "free"),
                      draw = FALSE),
        col = 1, cex  = 0.5, pch = 16,
        xlab = "Value of the variable",
        ylab = "Order of the data from text file")




####################################################################################################

# # SAVE DATA # # 

####################################################################################################

setwd(comp.data)
save(quad, file = 'quad.R') #gps & transect data matched to quad data (merge_fielddata.R)
