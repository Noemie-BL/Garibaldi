# Aims:
# 1. Import GPS with elev, transect, and quadrat data
# 2. Match transect-level & GPS data to quadrat-level data
# 3. Check data
# 4. Save compiled datasets

# Author: Nathalie Chardon
# Date created: 11 Nov 2022
# Date updated: 15 Nov 2022

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

trans.raw <- read.csv('Trampling-TRANSECTS_data.csv')

quad.raw <- read.csv('Trampling-QUADS_data.csv')


# # OUTPUT FILES # #

load('quad.RData') #gps & transect data matched to quad data (merge_fielddata.R)




####################################################################################################

# # ABBREVIATIONS IN DATAFRAMES # # 

####################################################################################################

# Trail locations: Taylor Meadows (TM), Black Tusk (BT), and Panorama Ridge (PR)

# Transect data: 

# - samplers: Nathalie Chardon (NC), Carly Hilbert (CH), Mackenzie Urquhart-Cronish (MUC), 
# Brianna Ragsdale (BR), Teagan MacLachlan (TL), Vickie Lee (VL), Christian Lauber (CL), 
# Carolyn Chong (CC)

# - d_trail_m: distance from trail edge [m]


# Quad data:

# - species: Vaccinium ovalifolium (vacova), Cassiope mertensiana (casmer), 
# Phyllodoce empetriformis (phyemp), Phyllodoce grandiflora (phygra), Carex sp. (carspp)

# - height_mm: maximum height of plant adjusted for ruler offsets [mm]

# - mxdiam_mm: maximum diameter of plant [mm]

# - flws: number of flowers per plant

# - frts: number of fruits per plant

# - buds: number of buds per plant

# - dist: 0 = off-trail (even) transects, 1 = trail-side (odd) transects




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

nrow(anti_join(gps, quad.raw, by = 'transect')) #25 --> should only be 3 for non-transect GPS data
anti_join(gps, quad.raw, by = 'transect')$transect #check that mismatches are for non-transect data


# PROBLEM 1: Transects labeled differently
# SOLUTION: Rename transects manually in CSV file --> resolved by CH

# PROBLEM 2: Most transects missing 
# SOLUTION: Upload all data to GitHub --> resolved by CH

foo <- anti_join(gps, quad.raw, by = 'transect')
unique(foo$transect) #missing transects


# Final consistency check
anti_join(gps, quad.raw, by = 'transect')$transect




####################################################################################################

# # TRANSECT DATA # # 

####################################################################################################

# Look at data

head(trans.raw)
tail(trans.raw) #empty rows

# Keep only first 28 rows with data

trans.raw <- trans.raw[1:28, ]

# Add matching column name for transect

trans.raw$transect.no <- trans.raw$transect
trans.raw$transect <- trans.raw$start_gps

# Check consistency between dataframes

nrow(anti_join(trans.raw, quad.raw, by = 'transect')) #1


# PROBLEM 3: Transect labeled incorrectly in transect datasheet
# SOLUTION: Re-label transect

anti_join(trans.raw, quad.raw, by = 'transect') #BR-14

trans.raw <- trans.raw %>% 
  mutate(transect = if_else(transect == 'BR-14', 'BT-14', transect))


# Final consistency check

nrow(anti_join(trans.raw, quad.raw, by = 'transect')) #0


# # DATA ENTRY CHECKS WITH RAW DATASHEETS # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

unique(trans.raw$data_entry_notes)

# --> notes don't affect current data analyses




####################################################################################################

# # QUAD DATA # # 

####################################################################################################

# Look at data

head(quad.raw)
tail(quad.raw) #empty rows
dim(quad.raw)

# Keep only rows with data

quad.raw <- quad.raw %>% 
  drop_na(quad)

dim(quad.raw)
tail(quad.raw)

quad.raw %>% filter(transect == 'TM-28', quad == 10) #check that TM-28 quad 10 exists

# Keep only columns with data
colnames(quad.raw)
quad.raw <- quad.raw[, 1:10]


# Check datastructure
str(quad.raw)

# Turn species data to numeric
quad.raw <- quad.raw %>% 
  mutate_at(vars(height_mm, mxdiam_mm, flws, frts, buds), as.numeric)

str(quad.raw)
summary(quad.raw)


# Check consistency between dataframes

nrow(anti_join(quad.raw, trans.raw, by = 'transect')) #2


# PROBLEM 4: Transects entered with extra characters in quad datasheet
# SOLUTION: Re-label transect

anti_join(quad.raw, trans.raw, by = 'transect')$transect #"PR-17 " "TM- 27"

quad.raw <- quad.raw %>% 
  mutate(transect = if_else(transect == 'PR-17 ', 'PR-17', transect))

quad.raw <- quad.raw %>% 
  mutate(transect = if_else(transect == 'TM- 27', 'TM-27', transect))


# Final consistency check

nrow(anti_join(quad.raw, trans.raw, by = 'transect')) #0


# # DATA ENTRY CHECKS WITH RAW DATASHEETS # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

unique(quad.raw$data_entry_notes)

foo <- quad.raw %>% 
  filter(nchar(data_entry_notes) > 1) #filter dataframe to data entry notes

View(foo)

#BT-12-9: value is correct

#BT-13-3: value is correct

#BT-14-9: transect finished at end of day with extra help for plant ID

# BT-15-8: value is correct

# BT-16-1: no data --> delete row

quad.raw <- quad.raw %>% 
  drop_na(height_mm)

# BT-16-1: no data --> delete row (executed with lines above)

# BT-16-2: value is correct

# BT-16-3: value is correct

# PR-17-5: value is correct

# PR-19-2: 'c' among group of car_spp is likely the same --> switch value

quad.raw <- quad.raw %>% 
  mutate(species = if_else(species == 'c', 'carspp', species))

# PR-19-8: no value among group of car_spp is likely the same --> switch value

boo <- quad.raw %>% filter(is.na(species))
View(boo) #same is true for PR-19-9 --> switch value

quad.raw <- quad.raw %>% 
  mutate(species = if_else(is.na(species), 'carspp', species))

# PR-20-9: checked quad photo, both vac_ova and car_spp present --> car_ova == car_spp

quad.raw <- quad.raw %>% 
  mutate(species = if_else(species == 'carova', 'carspp', species))

# PR-22-1: value is correct

# TM-2-6: value is correct

# TM-2-10: value is correct

# TM-4-2: value is correct

# TM-5-9: value is correct

# TM-6-2: value is correct

# TM-7-7: flwr should be 10 --> replace value

quad.raw <- quad.raw %>% 
  mutate(flws = if_else(species == 'casmer' & transect == 'TM-7' & quad == 7,
                           10, flws))

# TM-26-1: value is correct

# TM-28-3: NA value is correct, no data given


# # CHECK DATA VALUES # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

# Correct species names
unique(quad.raw$species)

wrong.list <- c("vacov", "casmer ", "phyemp ", "vacvoa",  "Phy.emp", "Phyemp",
                "cancer", "vacova ")

correct.list <- c("vacova", "casmer", "phyemp", "vacova",  "phyemp", "phyemp",
                "casmer", "vacova")

for (i in 1:nrow(quad.raw)) { #loop through each row
  
  for (j in 1:length(wrong.list)) { #loop through each wrong.list name
    
    if (quad.raw$species[i] == wrong.list[j]) {
      
      quad.raw$species[i] <- correct.list[j]
    }
  }
}

unique(quad.raw$species)

foo <- quad.raw %>% 
  filter(nchar(data_notes) > 1) #filter dataframe to data entry notes

View(foo) #all ok


# Visual data check of distribution for overdispersion & 0-inf
# --> very nice data distribution in height & diam, repro indices likely 0-inflated

spp <- unique(quad.raw$species)
resp <- c('height_mm', 'mxdiam_mm', 'flws', 'frts', 'buds') #set up vectors for loop

par(mfrow = c(5, 5)) #set up plotting space

for (j in 1:length(spp)) {
  
    foo <- quad.raw %>% filter(species == spp[j]) #subset by species
    
    for (h in 1:length(resp)) { #loop through each response var
      
      hist(foo[, resp[h]], breaks = 50, main = paste(spp[j], ':', resp[h]), xlab = '')
    }
}


# Check for outliers (code modified from Zurr et al. 2009)

foo <- quad.raw %>% filter(species == spp[5]) #switch out number here to subset by species
  
Z <- cbind(foo$height_mm, foo$mxdiam_mm, foo$flws, foo$frts, foo$buds)
  
dotplot(as.matrix(Z), groups = F,
          strip = strip.custom(bg = 'white',
                               par.strip.text = list(cex = 0.8)),
          scales = list(x = list(relation = "free"),
                        y = list(relation = "free"),
                        draw = FALSE),
          col = 1, cex  = 0.5, pch = 16,
          xlab = "Value of the variable",
          ylab = "Order of the data from text file")

# carspp: outliers in height & diam

foo <- quad.raw %>% filter(species == 'carspp')

foo %>% filter(height_mm > 150) #several values in this range so ok

foo %>% filter(mxdiam_mm > 300) #several values in this range so ok




####################################################################################################

# # MERGE DATAFRAMES # # 

####################################################################################################

# Join GPS to transect data

trans <- left_join(trans.raw, gps, by = 'transect') 

# Save transect dataframe with all variables
setwd(comp_dat)
save(trans, file = 'trans_ALL.RData')

# Keep only relevant columns

trans <- trans %>% 
  select(c(location, transect, slope, aspect, ruler_offset, latitude, longitude, altitude, transect.no))


# Join transect to quad data

quad.raw <- quad.raw %>% 
  select(!c('data_notes','data_entry_notes')) #remove irrelevant columns

quad <- left_join(quad.raw, trans, by = 'transect') 

head(quad) #look at data
str(quad) #check data structure


# Add disturbance column to quad

quad$x_logical <- quad$transect.no %% 2 == 0 #create even/odd logical

quad <- quad %>% 
  mutate(dist = if_else(x_logical == TRUE, 0, 1)) #0 = off-trail (even) transects, 1 = trail-side (odd) transects




####################################################################################################

# # CORRECT VALUES MEASURED WITH OFFSET RULERS # #

####################################################################################################

# Enter data for no ruler offsets

quad <- quad %>% 
  mutate(ruler_offset = if_else(is.na(ruler_offset), '0', ruler_offset))

# Transect-level offsets

offs <- unique(quad$ruler_offset) #offset types
offs <- offs[c(4, 6, 8)] #transect-level offset types
adj <- c(8, 5, 6) #amount to adjust height by

quad$height_adj <- quad$height_mm #set up empty column

for (i in 1:nrow(quad)) { #loop through each row
  
  for (j in 1:length(offs)) { #loop through each offset type
    
    if (quad$ruler_offset[i] == offs[j]) {
      
      quad$height_adj[i] <- quad$height_mm[i] - adj[j]
    }
  }
}


# Quad-level offsets: specify exact conditions in for loop

offs <- unique(quad$ruler_offset)
offs <- offs[c(1, 2, 5, 9)] #quad-level offset types

# Case 1: 8mm_q1-7
for (i in 1:nrow(quad)) { #loop through each row
  
  if (quad$ruler_offset[i] == offs[1] & quad$quad[i] <= 7) {
    
    quad$height_adj[i] <- quad$height_mm[i] - 8
  }
}

# Case 2: 8mm_q1-4
for (i in 1:nrow(quad)) { #loop through each row
  
  if (quad$ruler_offset[i] == offs[2] & quad$quad[i] <= 4) {
    
    quad$height_adj[i] <- quad$height_mm[i] - 8
  }
}

# Case 3: 5mm_q1-4,7; 8mm_q8-10
for (i in 1:nrow(quad)) { #loop through each row
  
  if (quad$ruler_offset[i] == offs[3] & quad$quad[i] <= 4) { #first offset type for q1-4
    
    quad$height_adj[i] <- quad$height_mm[i] - 5
  }
  
  else if (quad$ruler_offset[i] == offs[3] & quad$quad[i] == 7) { #first offset type for q7
    
    quad$height_adj[i] <- quad$height_mm[i] - 5
  }
  
  else if (quad$ruler_offset[i] == offs[3] & quad$quad[i] >= 8) { #second offset type for q8-10
    
    quad$height_adj[i] <- quad$height_mm[i] - 8
  }
}

# Case 4: 8mm_q9, 5mm_q1-8
for (i in 1:nrow(quad)) { #loop through each row
  
  if (quad$ruler_offset[i] == offs[4] & quad$quad[i] == 9) { #first offset type for q9
    
    quad$height_adj[i] <- quad$height_mm[i] - 8
  }
  
  else if (quad$ruler_offset[i] == offs[3] & quad$quad[i] <= 8) { #second offset type for q1-8
    
    quad$height_adj[i] <- quad$height_mm[i] - 5
  }
}


# Check calculations

plot(height_adj ~ height_mm, data = quad) #check 1:1 line ISN'T perfect (for non-adjusted values)
cor(quad$height_adj, quad$height_mm) #should be < 1

summary(quad$height_mm)
summary(quad$height_adj)



# Force negative height values to zero

quad$height_adj <- ifelse(quad$height_adj <0, 0, quad$height_adj)



####################################################################################################

# # SAVE DATA # # 

####################################################################################################

# Remove unnecessary columns

quad <- quad %>% 
  select(!c('x_logical', 'height_mm', 'ruler_offset')) #remove irrelevant columns


# Rename height column

quad <- rename(quad, height_mm = height_adj)


# Save

setwd(comp_dat)
save(quad, file = 'quad.RData') #gps & transect data matched to quad data (merge_fielddata.R)




####################################################################################################

# # P. ALBICAULIS DATA FOR BC PARKS # # 

####################################################################################################

# Data

setwd(raw_dat)
gps.raw <- read.table('GPS_with-elev.txt', header = T, fill = T) #lat, long, elev, ID for each transect
trans.raw <- read.csv('Trampling-TRANSECTS_data.csv')


# Keep only relevant columns & rename

gps <- gps.raw %>% 
  select(latitude, longitude, altitude, transect = X.m., name)

length(unique(gps$transect)) # should have 28 transects + 2 P. albicaulis points + garbage pile cam = 31


# Add matching column name for transect

trans.raw$transect.no <- trans.raw$transect
trans.raw$transect <- trans.raw$start_gps


# Check consistency between dataframes

trans.raw <- trans.raw[1:28, ] #keep only first 28 rows with data

trans.raw <- trans.raw %>% 
  mutate(transect = if_else(transect == 'BR-14', 'BT-14', transect)) #fix typo

anti_join(gps, trans.raw, by = 'transect')$transect #should only be 3 for non-transect GPS data
nrow(anti_join(trans.raw, gps, by = 'transect')) 


# Join transect to GPS data 

gps.pinalb <- full_join(gps, trans.raw, by = 'transect') 


# Organize dataframe

pinalb <- gps.pinalb %>% 
  filter(transect != 'garbage_pile_cam') %>% #remove garbage pile cam GPS
  select(date, p_albicaulis = p_albicaula, m_from_transect, healthy..1..yes..0...no., cones..2...many..1...few..0...none.,
         transect, location, latitude, longitude, altitude) #keep only relevant columns

pinalb$contact <- 'nathalie.chardon@gmail.com' #add contact info


# Save dataframe
setwd(comp_dat)
write.csv(pinalb, file = 'P_albicaulis_Garibaldi_Aug2022.csv', row.names = F)


# Manual edits: 
# add dates from Gaia
# add '1' for Gaia GPS points
