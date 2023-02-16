# Aims:
# 1. Define and calculate a standardized reproductive metric

# Authors: Nathalie Chardon, Cassandra Elphinstone, Philippa Stone
# Date created: 15 Feb 2023
# Date updated: 15 Feb 2023 (PS) added figures
# Date updated: 15 Feb 2023 (NC) ###*** edit here if updating script


# # LIBRARIES # # 
library(ggplot2)
library(ggtext)
library(scales)

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

# Convert categorical predictor variables to factor
ff <- c('transect', 'species', 'dist')
dat[ff] <- lapply(dat[ff], as.factor)

# adding new columns for plant area, buds/area, flowers/area, and fruits/area

plantArea_cm2 <-  (dat$mxdiam_mm * dat$height_mm)/100 # I changed it into cm2 here because I found it easier to understand, but might be better to switch it to mm2 or m2?
dat <- cbind(dat, plantArea_cm2)
flwsByArea <- dat$flws / dat$plantArea_cm2
frtsByArea <- dat$frts / dat$plantArea_cm2
budsByArea <- dat$buds / dat$plantArea_cm2
dat <- cbind (dat, flwsByArea, frtsByArea, budsByArea)

# full species names
species_names <- c('carspp'= "Carex spp.", 'casmer' = "Cassiope mertensiana", 'phyemp' = "Phyllodoce empetriformis", 'phygla' = "Phyllodoce glanduliflora", 'vacova' = "Vaccinium ovalifolium")

####################################################################################################

# # VISUALIZING RELATIONSHIPS BETWEEN SIZE AND REPRO OUTPUTS (Philippa) # # 

####################################################################################################

# # ALL SPECIES AGGREGATED

# Plot size vs number of buds/(height * diameter) by disturbed/undisturbed

ggplot(dat, aes(x= plantArea_cm2, y= budsByArea, color = dist)) +
  geom_point() +
  stat_smooth(method = "lm", formula = y ~ x, geom = "smooth") + # stat_smooth for non-log transformed y axis
  geom_smooth(method = "glm", formula = y~x, method.args = list(family = gaussian(link = 'log'))) +
  coord_trans(y = "log1p") + #log10(x + 1) y axis 
  scale_y_continuous(labels = label_comma()) + #remove scientific notation
  scale_x_continuous(labels = label_comma()) + #remove scientific notation
  labs(x = "Plant area (cm^2)", y = "Number of buds / cm^2") +
  scale_color_discrete(labels=c('Undisturbed', 'Disturbed'), name = "Disturbance") + #legend labels and title
  theme(axis.title.x = element_markdown(), axis.title.y = element_markdown()) #use markdown theme for axes labels

# Plot size vs number of flowers/(height * diameter) by disturbed/undisturbed

ggplot(dat, aes(x= plantArea_cm2, y= flwsByArea, color = dist)) +
  geom_point() +
  #stat_smooth(method = "lm", formula = y ~ x, geom = "smooth") +
  geom_smooth(method = "glm", formula = y~x, method.args = list(family = gaussian(link = 'log'))) +
  coord_trans(y = "log1p") + #log10(x + 1) y axis 
  scale_y_continuous(labels = label_comma()) + #remove scientific notation
  scale_x_continuous(labels = label_comma()) + #remove scientific notation
  labs(x = "Plant area (cm^2)", y = "Number of flowers / cm^2") +
  scale_color_discrete(labels=c('Undisturbed', 'Disturbed'), name = "Disturbance") + #legend labels and title
  theme(axis.title.x = element_markdown(), axis.title.y = element_markdown()) #use markdown theme for axes labels

# Plot size vs number of fruits/(height * diameter) by disturbed/undisturbed

ggplot(dat, aes(x= plantArea_cm2, y= frtsByArea, color = dist)) +
  geom_point() +
  stat_smooth(method = "lm", formula = y ~ x, geom = "smooth") +
  coord_trans(y = "log1p") + #log10(x + 1) y axis 
  scale_y_continuous(labels = label_comma()) + #remove scientific notation
  scale_x_continuous(labels = label_comma()) + #remove scientific notation
  labs(x = "Plant area (cm^2)", y = "Number of fruits / cm^2") +
  scale_color_discrete(labels=c('Undisturbed', 'Disturbed'), name = "Disturbance") + #legend labels and title
  theme(axis.title.x = element_markdown(), axis.title.y = element_markdown()) #use markdown theme for axes labels


# # ALL SPECIES PLOTTED SEPARATELY

# Plot size vs number of buds/(height * diameter) by disturbed/undisturbed

ggplot(dat, aes(x= plantArea_cm2, y= budsByArea, color = dist)) +
  geom_point() +
  stat_smooth(method = "lm", formula = y ~ x, geom = "smooth") +
  facet_wrap(.~ species, labeller = as_labeller(species_names), scales = "free") + #separate by species and rename with full species names
  coord_trans(y = "log1p") + #log10 y axis 
  scale_y_continuous(labels = label_comma()) + #remove scientific notation
  scale_x_continuous(labels = label_comma()) + #remove scientific notation
  labs(x = "Plant area (cm^2)", y = "Number of buds / cm^2") +
  scale_color_discrete(labels=c('Undisturbed', 'Disturbed'), name = "Disturbance") + #legend labels and title
  theme(axis.title.x = element_markdown(), axis.title.y = element_markdown()) #use markdown theme for axes labels


# Plot size vs number of flowers/(height * diameter) by disturbed/undisturbed

ggplot(dat, aes(x= plantArea_cm2, y= flwsByArea, color = dist)) +
  geom_point() +
  geom_smooth(method = "glm", formula = y~x,   method.args = list(family = gaussian(link = 'log'))) +
  #stat_smooth(method = "lm", formula = y ~ x, geom = "smooth") +
  facet_wrap(.~ species, labeller = as_labeller(species_names), scales = "free") + #separate by species and rename with full species names
  coord_trans(y = "log1p") + #log10 y axis 
  scale_y_continuous(labels = label_comma()) + #remove scientific notation
  scale_x_continuous(labels = label_comma()) + #remove scientific notation
  labs(x = "Plant area (cm^2)", y = "Number of flowers / cm^2") +
  scale_color_discrete(labels=c('Undisturbed', 'Disturbed'), name = "Disturbance") + #legend labels and title
  theme(axis.title.x = element_markdown(), axis.title.y = element_markdown()) #use markdown theme for axes labels


# Plot size vs number of fruits/(height * diameter) by disturbed/undisturbed

ggplot(dat, aes(x= plantArea_cm2, y= frtsByArea, color = dist)) +
  geom_point() +
 # geom_smooth(method = "lm", formula = y~x,   method.args = list(family = gaussian(link = 'log'))) +
  stat_smooth(method = "lm", formula = y ~ x, geom = "smooth") +
  facet_wrap(.~ species, labeller = as_labeller(species_names), scales = "free") + #separate by species and rename with full species names
  coord_trans(y = "log1p") + #log10 y axis 
  scale_y_continuous(labels = label_comma()) + #remove scientific notation
  scale_x_continuous(labels = label_comma()) + #remove scientific notation
  labs(x = "Plant area (cm^2)", y = "Number of fruits / cm^2") +
  scale_color_discrete(labels=c('Undisturbed', 'Disturbed'), name = "Disturbance") + #legend labels and title
  theme(axis.title.x = element_markdown(), axis.title.y = element_markdown()) #use markdown theme for axes labels



####################################################################################################

# # EXPLORE DIFFERENT SLOPES OR EQUATION OF LINE DIFFERENCES BY SPECIES (Cassandra) # # 

####################################################################################################





