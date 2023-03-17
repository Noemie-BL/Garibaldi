# Aims:
# 1. Define and calculate a standardized reproductive metric

# Authors: Nathalie Chardon, Cassandra Elphinstone, Philippa Stone
# Date created: 15 Feb 2023
# Date updated: 10 March 2023 (PS)
# Date updated: 15 Feb 2023 (NC) ###*** edit here if updating script


# # LIBRARIES # # 
library(lmerTest)
library(ggplot2)
library(ggtext)
library(scales)
library(tidyverse)

rm(list=ls()) 


# # WORKING DIRECTORIES # #
#comp_dat <- '~/Desktop/Code/Garibaldi/trampling_analyses/compiled_data/' #WD for NC
comp_dat <- 'C:/Users/Owner/Documents/GitHub/Garibaldi/trampling_analyses/compiled_data/' #WD for CE

# # INPUT FILES # #
setwd(comp_dat)
load('quad.RData') #gps & transect data matched to quad data (merge_fielddata.R)


# # OUTPUT FILES # #



# # PLOT THEME # #
mytheme <-   theme_classic() +
  theme(axis.text.x = element_text(colour = 'black', size = 25), #x axis text size
        axis.text.y = element_text(colour = 'black', size = 25), #y axis text size
        axis.title.x = element_text(size = 28), #x axis label size
        axis.title.y = element_text(size = 28), #x axis label size
        plot.title = element_text(size = 30, #title size
                                  hjust = 0.5), #align title to center
        legend.title = element_text(size = 24), legend.text = element_text(size = 22)) 


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

# Remove Carex spp.
dat <- subset(dat, dat$species!= "carspp")

# adding new columns for plant area, buds/area, flowers/area,fruits/area, total repro struct, and total repro struct/area

flwsByArea <- dat$flws / dat$plantArea_cm2
frtsByArea <- dat$frts / dat$plantArea_cm2
budsByArea <- dat$buds / dat$plantArea_cm2
dat <- cbind (dat, flwsByArea, frtsByArea, budsByArea)

totalReproStruct <- dat$flws + dat$frts + dat$buds
dat <- cbind(dat, totalReproStruct)

totalReproStructByArea <- dat$totalReproStruct / dat$plantArea_cm2
dat <- cbind(dat, totalReproStructByArea)

# Convert Inf and -Inf values to NA
dat <- dat %>% 
  mutate_if(is.numeric, ~ replace(., is.infinite(.), NA))

summary(dat) #check that no infinite values exist

# full species names
species_names <- c('carspp'= "Carex spp.", 'casmer' = "Cassiope mertensiana", 'phyemp' = "Phyllodoce empetriformis", 'phygla' = "Phyllodoce glanduliflora", 'vacova' = "Vaccinium ovalifolium")

####################################################################################################

# # VISUALIZING RELATIONSHIPS BETWEEN SIZE AND REPRO OUTPUTS (Philippa) # # 

####################################################################################################

# # ALL SPECIES AGGREGATED

# Plot size vs number of buds/(height * diameter) by disturbed/undisturbed

ggplot(dat, aes(x= plantArea_cm2, y= budsByArea, color = dist)) +
  geom_point() +
  stat_smooth(method = "lm", formula = y ~ x, geom = "smooth") + # trend line
  coord_trans(y = "log1p") + #log10(x + 1) y axis 
  scale_y_continuous(labels = label_comma()) + #remove scientific notation
  scale_x_continuous(labels = label_comma()) + #remove scientific notation
  labs(x = "Plant area (cm^2)", y = "Number of buds / cm^2") +
  scale_color_discrete(labels=c('Undisturbed', 'Disturbed'), name = "Disturbance") + #legend labels and title
  theme(axis.title.x = element_markdown(), axis.title.y = element_markdown()) #use markdown theme for axes labels

# Plot size vs number of flowers/(height * diameter) by disturbed/undisturbed

ggplot(dat, aes(x= plantArea_cm2, y= flwsByArea, color = dist)) +
  geom_point() +
  stat_smooth(method = "lm", formula = y ~ x, geom = "smooth") +
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

# Plot size vs number of buds, flowers, and fruits/(height * diameter) by disturbed/undisturbed

ggplot(dat, aes(x= plantArea_cm2, y= totalReproStructByArea, color = dist)) +
  geom_point() +
  stat_smooth(method = "lm", formula = y ~ x, geom = "smooth") +
  coord_trans(y = "log1p") + #log10(x + 1) y axis 
  scale_y_continuous(labels = label_comma()) + #remove scientific notation
  scale_x_continuous(labels = label_comma()) + #remove scientific notation
  labs(x = "Plant area (cm^2)", y = "Number of reproductive structures / cm^2") +
  scale_color_discrete(labels=c('Undisturbed', 'Disturbed'), name = "Disturbance") + #legend labels and title
  theme(axis.title.x = element_markdown(), axis.title.y = element_markdown()) #use markdown theme for axes labels

# Plot size vs number of buds, flowers, and fruits by disturbed/undisturbed

ggplot(dat, aes(x= plantArea_cm2, y= totalReproStruct, color = dist)) +
  geom_point() +
  stat_smooth(method = "lm", formula = y ~ x, geom = "smooth") +
  scale_y_continuous(labels = label_comma()) + #remove scientific notation
  scale_x_continuous(labels = label_comma()) + #remove scientific notation
  labs(x = "Plant area (cm^2)", y = "Number of reproductive structures") +
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
  stat_smooth(method = "lm", formula = y ~ x, geom = "smooth") +
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
  stat_smooth(method = "lm", formula = y ~ x, geom = "smooth") +
  facet_wrap(.~ species, labeller = as_labeller(species_names), scales = "free") + #separate by species and rename with full species names
  coord_trans(y = "log1p") + #log10 y axis 
  scale_y_continuous(labels = label_comma()) + #remove scientific notation
  scale_x_continuous(labels = label_comma()) + #remove scientific notation
  labs(x = "Plant area (cm^2)", y = "Number of fruits / cm^2") +
  scale_color_discrete(labels=c('Undisturbed', 'Disturbed'), name = "Disturbance") + #legend labels and title
  theme(axis.title.x = element_markdown(), axis.title.y = element_markdown()) #use markdown theme for axes labels


# Plot size vs number of buds, flowers, and fruits/(height * diameter) by disturbed/undisturbed

ggplot(dat, aes(x= plantArea_cm2, y= totalReproStructByArea, color = dist)) +
  geom_point() +
  stat_smooth(method = "lm", formula = y ~ x, geom = "smooth") +
  facet_wrap(.~ species, labeller = as_labeller(species_names), scales = "free") + #separate by species and rename with full species names
  coord_trans(y = "log1p") + #log10 y axis 
  scale_y_continuous(labels = label_comma()) + #remove scientific notation
  scale_x_continuous(labels = label_comma()) + #remove scientific notation
  labs(x = "Plant area (cm^2)", y = "Number of reproductive structures / cm^2") +
  scale_color_discrete(labels=c('Undisturbed', 'Disturbed'), name = "Disturbance") + #legend labels and title
  theme(axis.title.x = element_markdown(), axis.title.y = element_markdown()) #use markdown theme for axes labels


# Plot size vs number of buds, flowers, and fruits by disturbed/undisturbed

ggplot(dat, aes(x= plantArea_cm2, y= totalReproStruct, color = dist)) +
  geom_point() +
  stat_smooth(method = "lm", formula = y ~ x, geom = "smooth") +
  facet_wrap(.~ species, labeller = as_labeller(species_names), scales = "free") + #separate by species and rename with full species names
  scale_y_continuous(labels = label_comma()) + #remove scientific notation
  scale_x_continuous(labels = label_comma()) + #remove scientific notation
  labs(x = "Plant area (cm^2)", y = "Number of reproductive structures") +
  scale_color_discrete(labels=c('Undisturbed', 'Disturbed'), name = "Disturbance") + #legend labels and title
  theme(axis.title.x = element_markdown(), axis.title.y = element_markdown()) #use markdown theme for axes labels


####################################################################################################

# # EXPLORE DIFFERENT SLOPES OR EQUATION OF LINE DIFFERENCES BY SPECIES (Cassandra) # # 

####################################################################################################

#fitting model for reproductinve output
mod <- lmer(repro ~ dist * plantArea_cm2 + (1|trans.pair) + (1|species), data = dat, REML = T)


