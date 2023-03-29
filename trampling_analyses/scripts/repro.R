# Aims:
# 1. Define and calculate a standardized reproductive metric

# Authors: Nathalie Chardon, Cassandra Elphinstone, Philippa Stone
# Date created: 15 Feb 2023
# Date updated: 28 March 2023 (NC)


# # LIBRARIES # # 
library(ggplot2)
library(ggtext)
library(scales)
library(tidyverse)
library(lmerTest)


rm(list=ls()) 


# # INPUT FILES # #
load('trampling_analyses/compiled_data/quad.RData') #gps & transect data matched to quad data (merge_fielddata.R)


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

theme_set(mytheme)




####################################################################################################

# # DATA # # 

####################################################################################################

# Load quadrat-level data (see merge_fielddata.R for variable descriptions)
load('trampling_analyses/compiled_data/quad.RData') #gps & transect data matched to quad data (merge_fielddata.R)


# Remove Carex spp.
quad <- quad %>% 
  filter(species != "carspp")

# adding new columns for plant area, buds/area, flowers/area,fruits/area, total repro struct, and total repro struct/area

flwsByArea <- quad$flws / quad$plantArea_cm2
frtsByArea <- quad$frts / quad$plantArea_cm2
budsByArea <- quad$buds / quad$plantArea_cm2
quad <- cbind (quad, flwsByArea, frtsByArea, budsByArea)

totalReproStruct <- quad$flws + quad$frts + quad$buds
quad <- cbind(quad, totalReproStruct)

totalReproStructByArea <- quad$totalReproStruct / quad$plantArea_cm2
quad <- cbind(quad, totalReproStructByArea)

# Convert Inf and -Inf values to NA
quad <- quad %>% 
  mutate_if(is.numeric, ~ replace(., is.infinite(.), NA))

summary(quad) #check that no infinite values exist

# full species names
species_names <- c('carspp'= "Carex spp.", 'casmer' = "Cassiope mertensiana", 'phyemp' = "Phyllodoce empetriformis", 'phygla' = "Phyllodoce glanduliflora", 'vacova' = "Vaccinium ovalifolium")




####################################################################################################

# # VISUALIZING RELATIONSHIPS BETWEEN SIZE AND REPRO OUTPUTS (Philippa) # # 

####################################################################################################

# # ALL SPECIES AGGREGATED

dat <- quad #generic dataframe

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

totalReproPlotArea <- ggplot(dat, aes(x= plantArea_cm2, y= totalReproStructByArea, color = dist)) +
  geom_point() +
  stat_smooth(method = "lm", formula = y ~ x, geom = "smooth") +
  coord_trans(y = "log1p") + #log10(x + 1) y axis 
  scale_y_continuous(labels = label_comma()) + #remove scientific notation
  scale_x_continuous(labels = label_comma()) + #remove scientific notation
  labs(x = "Plant area (cm^2)", y = "Number of reproductive structures / cm^2") +
  scale_color_discrete(labels=c('Undisturbed', 'Disturbed'), name = "Disturbance") + #legend labels and title
  theme(axis.title.x = element_markdown(), axis.title.y = element_markdown()) #use markdown theme for axes labels

ggsave(filename = "trampling_analyses/figures/publication/totalReproPlotArea", plot = totalReproPlotArea, device = "pdf", dpi = 600, width = 14, height = 8, units = "in")

# Plot size vs number of buds, flowers, and fruits by disturbed/undisturbed

totalReproPlot <- ggplot(dat, aes(x= plantArea_cm2, y= totalReproStruct, color = dist)) +
  geom_point() +
  stat_smooth(method = "lm", formula = y ~ x, geom = "smooth") +
  scale_y_continuous(labels = label_comma()) + #remove scientific notation
  scale_x_continuous(labels = label_comma()) + #remove scientific notation
  labs(x = "Plant area (cm^2)", y = "Number of reproductive structures") +
  scale_color_discrete(labels=c('Undisturbed', 'Disturbed'), name = "Disturbance") + #legend labels and title
  theme(axis.title.x = element_markdown(), axis.title.y = element_markdown()) #use markdown theme for axes labels

ggsave(filename = "trampling_analyses/figures/publication/totalReproPlot", plot = totalReproPlot, device = "pdf", dpi = 600, width = 14, height = 8, units = "in")


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

totalReproPlotAreaBySpecies <- ggplot(dat, aes(x= plantArea_cm2, y= totalReproStructByArea, color = dist)) +
  geom_point() +
  stat_smooth(method = "lm", formula = y ~ x, geom = "smooth") +
  facet_wrap(.~ species, labeller = as_labeller(species_names), scales = "free") + #separate by species and rename with full species names
  coord_trans(y = "log1p") + #log10 y axis 
  scale_y_continuous(labels = label_comma()) + #remove scientific notation
  scale_x_continuous(labels = label_comma()) + #remove scientific notation
  labs(x = "Plant area (cm^2)", y = "Number of reproductive structures / cm^2") +
  scale_color_discrete(labels=c('Undisturbed', 'Disturbed'), name = "Disturbance") + #legend labels and title
  theme(axis.title.x = element_markdown(), axis.title.y = element_markdown()) #use markdown theme for axes labels

ggsave(filename = "../figures/publication/totalReproPlotAreaBySpecies", plot = totalReproPlotAreaBySpecies, device = "pdf", dpi = 600, width = 14, height = 8, units = "in")

# Plot size vs number of buds, flowers, and fruits by disturbed/undisturbed

totalReproPlotBySpecies <- ggplot(dat, aes(x= plantArea_cm2, y= totalReproStruct, color = dist)) +
  geom_point() +
  stat_smooth(method = "lm", formula = y ~ x, geom = "smooth") +
  facet_wrap(.~ species, labeller = as_labeller(species_names), scales = "free") + #separate by species and rename with full species names
  scale_y_continuous(labels = label_comma()) + #remove scientific notation
  scale_x_continuous(labels = label_comma()) + #remove scientific notation
  labs(x = "Plant area (cm^2)", y = "Number of reproductive structures") +
  scale_color_discrete(labels=c('Undisturbed', 'Disturbed'), name = "Disturbance") + #legend labels and title
  theme(axis.title.x = element_markdown(), axis.title.y = element_markdown()) #use markdown theme for axes labels

ggsave(filename = "trampling_analyses/figures/publication/totalReproPlotBySpecies", plot = totalReproPlotBySpecies, device = "pdf", dpi = 600, width = 14, height = 8, units = "in")




####################################################################################################

# # TEST FOR RELATIONSHIP BETWEEN PLANT AREA AND REPRO STRUCTURES BY SPECIES (Nathalie) # # 
# package: lmerTest
# Fixed effects: plantArea_cm2 + dist
# Random effects: (1|trans.pair)

####################################################################################################

# # Data
str(quad) #check that categorical explanatory variables are factors, others numeric

# Skew function (from: https://towardsdatascience.com/evaluating-bayesian-mixed-models-in-r-python-27d344a03016_)
skew <- function(y){ # Fisher-Pearson Skew function based on NIST definition
  n <- length(y)
  dif <- y - mean(y)
  skew_stat <- (sqrt(n-1)/(n-2))*n *(sum(dif^3)/(sum(dif^2)^1.5))
  return(skew_stat)
}

## PHYEMP ##

# Filter by species 
dat <- quad %>%
  filter(species == 'phyemp')

hist(dat$totalReproStructByArea, breaks = 100)

# # Fit Model 
mod <- lmer(totalReproStructByArea ~ plantArea_cm2 + dist + (1|trans.pair), data = dat)

summary(mod)


## CASMER ##

# Filter by species 
dat <- quad %>%
  filter(species == 'casmer')

hist(dat$totalReproStructByArea, breaks = 100)

# # Fit Model 
mod <- lmer(totalReproStructByArea ~ plantArea_cm2 + dist + (1|trans.pair), data = dat)

summary(mod)


## VACOVA ##

# Filter by species 
dat <- quad %>%
  filter(species == 'vacova')

hist(dat$totalReproStructByArea, breaks = 100)

# # Fit Model 
mod <- lmer(totalReproStructByArea ~ plantArea_cm2 + dist + (1|trans.pair), data = dat)

summary(mod)




