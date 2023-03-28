# Aims:
# 1. Define and calculate a standardized reproductive metric

# Authors: Nathalie Chardon, Cassandra Elphinstone, Philippa Stone
# Date created: 15 Feb 2023
# Date updated: 28 March 2023 (NC)


# # LIBRARIES # # 
library(lmerTest)
library(ggplot2)
library(ggtext)
library(scales)
library(tidyverse)
library(brms)
library(lmerTest) #lmer from lme4 with added p-values


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

# # TEST FOR RELATIONSHIP BETWEEN PLANT AREA AND REPRO STRUCTURES (Nathalie) # # 
# package: brms
# Family: Gamma
# Fixed effects: plantArea_cm2 + dist
# Random effects: (1|trans.pair) + (plantArea_cm2 + dist|species)

####################################################################################################

# # Data
str(dat) #check that categorical explanatory variables are factors, others numeric

hist(dat$totalReproStructByArea, breaks = 100) #response variable


# Skew function (from: https://towardsdatascience.com/evaluating-bayesian-mixed-models-in-r-python-27d344a03016_)
skew <- function(y){ # Fisher-Pearson Skew function based on NIST definition
  n <- length(y)
  dif <- y - mean(y)
  skew_stat <- (sqrt(n-1)/(n-2))*n *(sum(dif^3)/(sum(dif^2)^1.5))
  return(skew_stat)
}

# Set 0 --> 0.0001 for Gamma distribution models
dat <- dat %>%
  mutate(totalReproStructByArea = if_else(totalReproStructByArea == 0, 0.0001, totalReproStructByArea))

# # Fit Model 
repro_size <- brms::brm(totalReproStructByArea ~ plantArea_cm2 + dist + 
                          (1|trans.pair) + (1|species),
                        
                       data = dat, family = gaussian(), 
                       
                       # fitting information
                       chains = 3, iter = 3000, warmup = 1000, cores = 4, #for computers with 4 cores
                       file = 'trampling_analyses/outputs/repro_size.rds', file_refit = 'on_change')

mod <- repro_size #generic model name


# # Model output

# Check posterior
summary(mod)
plot(mod)
plot(conditional_effects(mod))[[1]] + 
  geom_point(aes(plantArea_cm2, totalReproStructByArea), data = dat, inherit.aes = FALSE)

# Check prior
prior_summary(mod) 
ps <- powerscale_sensitivity(mod)
unique(ps$sensitivity$diagnosis)

# Model fit
pp_check(mod, ndraws = 100)
ppc_stat(y = dat$rel_rec, 
         yrep = posterior_predict(mod, ndraws = 1000),
         stat = "skew")
model_loo <- loo(mod, save_psis = TRUE, cores=4) 
w <- weights(model_loo$psis_object)
ppc_loo_pit_overlay(dat$totalReproStructByArea, 
                    posterior_predict(mod), 
                    lw = w)
