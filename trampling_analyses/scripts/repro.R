# Aims:
# 1. Define and calculate a standardized reproductive metric

# Authors: Nathalie Chardon, Cassandra Elphinstone, Philippa Stone
# Date created: 15 Feb 2023
# Date updated: 15 Feb 2023 (NC) ###*** edit here if updating script


# # LIBRARIES # # 


rm(list=ls()) 


# # WORKING DIRECTORIES # #
comp_dat <- '~/Desktop/Code/Garibaldi/trampling_analyses/compiled_data/' #WD for NC


# # INPUT FILES # #
setwd(comp_dat)
load('quad.RData') #gps & transect data matched to quad data (merge_fielddata.R)


# # OUTPUT FILES # #




####################################################################################################

# # DATA # # 

####################################################################################################

# Load quadrat-level data (see merge_fielddata.R for variable descriptions)
setwd(comp_dat)
load('quad.RData') #gps & transect data matched to quad data (merge_fielddata.R)

# Set generic dataframe name
dat <- quad 
str(dat)




####################################################################################################

# # VISUALIZING RELATIONSHIPS BETWEEN SIZE AND REPRO OUTPUTS (Philippa) # # 

####################################################################################################

# # ALL SPECIES AGGREGATED

# Plot size vs number of buds/(height * diameter) by disturbed/undisturbed


# Plot size vs number of flowers/(height * diameter) by disturbed/undisturbed


# Plot size vs number of fruits/(height * diameter) by disturbed/undisturbed


# # ALL SPECIES PLOTTED SEPARATELY

# Plot size vs number of buds/(height * diameter) by disturbed/undisturbed


# Plot size vs number of flowers/(height * diameter) by disturbed/undisturbed


# Plot size vs number of fruits/(height * diameter) by disturbed/undisturbed




####################################################################################################

# # EXPLORE DIFFERENT SLOPES OR EQUATION OF LINE DIFFERENCES BY SPECIES (Cassandra) # # 

####################################################################################################





