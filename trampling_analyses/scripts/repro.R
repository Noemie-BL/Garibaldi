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
library(egg) #for labelling plot facets


rm(list=ls()) 

# # WORKING DIRECTORIES # #
#comp_dat <- '~/Desktop/Code/Garibaldi/trampling_analyses/compiled_data/' #WD for NC
#comp_dat <- 'C:/Users/Owner/Documents/GitHub/Garibaldi/trampling_analyses/compiled_data/' #WD for CE
comp_dat <- '~/Documents/UBC/Projects/Garibaldi/Garibaldi/trampling_analyses/compiled_data/' #WD for PS

setwd(comp_dat)

# # INPUT FILES # #
load('trampling_analyses/compiled_data/quad.RData') #gps & transect data matched to quad data (merge_fielddata.R)


# # OUTPUT FILES # #
load('trampling_analyses/compiled_data/quad.RData') ##updated with reproductive metric & plant area (repro.R)



####################################################################################################

# # CALCULATE REPRODUCTIVE METRIC # # 

####################################################################################################

# Data
load('trampling_analyses/compiled_data/quad.RData') #gps & transect data matched to quad data (merge_fielddata.R)

dat <- quad #generic dataframe name
str(dat)

# Remove P. grandiflora because only sampled at 1 site
dat <- dat %>% 
  filter(!species == 'phygla')

# Convert categorical predictor variables to factor
ff <- c('transect', 'species', 'dist', 'trans.pair')
dat[ff] <- lapply(dat[ff], as.factor)

str(dat) #check data structure


# Calculate plant area for reproductive output
dat$plantArea_cm2 <- (dat$mxdiam_mm * dat$height_mm)/100

# Calculate reproductive metric combining buds, flws, frts and dividing by plant area
dat$repro <- NA #initialize column
for (i in 1:nrow(dat)) { #loop through each row of DF
  
  dat$repro[i] <- sum(c(dat$buds[i], dat$flws[i], dat$frts[i]), na.rm = T) / dat$plantArea_cm2[i]
}
summary(dat$repro)
hist(dat$repro, breaks = 100)

# dat <- dat %>% 
#   mutate_if(is.numeric, ~ replace(., is.infinite(.), NA)) #Inf to NAs


# Calculate relative reproduction
mm <- dat %>% #create DF with max value per species
  group_by(species) %>% 
  summarise(max = max(repro, na.rm = T)) #NA listed in plots not seeded

dat <- left_join(dat, mm, by = 'species') #combine DFs
dat$rel_repro <- dat$repro/dat$max
summary(dat$rel_repro)
hist(dat$rel_repro, breaks = 50)

dat <- dat %>% 
  select(-c(max)) #remove max column


# Save updated DF
quad <- dat
save(quad, file = 'trampling_analyses/compiled_data/quad.RData')




####################################################################################################

# # DATA FOR FIGS # # 

####################################################################################################

# # PLOT THEME # #
mytheme <- theme_classic() +
  theme(axis.text.x = element_text(colour = 'black', size = 25), #x axis text size
        axis.text.y = element_text(colour = 'black', size = 25), #y axis text size
        axis.title.x = element_text(size = 28), #x axis label size
        axis.title.y = element_text(size = 28), #x axis label size
        plot.title = element_text(size = 30, #title size
                                  hjust = 0.5), #align title to center
        legend.title = element_text(size = 24), legend.text = element_text(size = 22),
        panel.grid.major = element_blank(), #remove major gridlines
        panel.grid.minor = element_blank(), #remove minor gridlines
        panel.background = element_blank(), #remove panel background
        axis.line = element_line(colour = "black"), #set axis line color
        legend.key.size = unit(2, "line"), #increase size of legend key
        legend.spacing.y = unit(0.5, "cm"), #increase vertical spacing between legend items
        strip.background = element_rect(fill = "white"), #set background color for facet labels
        strip.text.x = element_text(face = "italic", size = 20)) #set size for x-axis facet labels

theme_set(mytheme)


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

totalReproPlotArea <- ggplot(dat, aes(x = plantArea_cm2, y = totalReproStructByArea, color = dist)) +
  geom_point() +
  stat_smooth(aes(fill = dist), method = "lm", formula = y ~ x, geom = "ribbon", alpha = 0.3, size = 0) +
  stat_smooth(aes(color = dist), method = "lm", formula = y ~ x, geom = "line", size=1) +
  scale_color_manual(values = c("#999999", "#E69F00"), labels = c("Undisturbed", "Disturbed"), name = "Disturbance") +
  scale_fill_manual(values = c("#999999", "#E69F00"), labels = c("Undisturbed", "Disturbed"), name = "Disturbance") +
  coord_trans(y = "log1p") +
 # scale_y_continuous(labels = label_comma()) +
 # scale_x_continuous(labels = label_comma()) +
  labs(x = expression(paste("Plant size [", cm^2, "]")),
       y = expression(paste("Density of reproductive structures [counts/", cm^2, "]"))) +
  mytheme

ggsave(filename = "../figures/publication/totalReproPlotArea", plot = totalReproPlotArea, device = "pdf", dpi = 600, width = 20, height = 10, units = "in")

# Plot size vs number of buds, flowers, and fruits by disturbed/undisturbed

totalReproPlot <- ggplot(dat, aes(x = plantArea_cm2, y = totalReproStruct, color = dist)) +
  geom_point() +
  stat_smooth(aes(fill = dist), method = "lm", formula = y ~ x, geom = "ribbon", alpha = 0.3, size = 0) +
  stat_smooth(aes(color = dist), method = "lm", formula = y ~ x, geom = "line", size=1) +
  scale_color_manual(values = c("#999999", "#E69F00"), labels = c("Undisturbed", "Disturbed"), name = "Disturbance") +
  scale_fill_manual(values = c("#999999", "#E69F00"), labels = c("Undisturbed", "Disturbed"), name = "Disturbance") +
  # scale_y_continuous(labels = label_comma()) +
  # scale_x_continuous(labels = label_comma()) +
  labs(x = expression(paste("Plant size [", cm^2, "]")),
       y = expression(paste("Number of reproductive structures"))) +
  mytheme

ggsave(filename = "../figures/publication/totalReproPlot", plot = totalReproPlot, device = "pdf", dpi = 600, width = 20, height = 10, units = "in")


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
  stat_smooth(aes(fill = dist), method = "lm", formula = y ~ x, geom = "ribbon", alpha = 0.3, size = 0) +
  stat_smooth(aes(color = dist), method = "lm", formula = y ~ x, geom = "line", size=1) +
  scale_color_manual(values = c("#999999", "#E69F00"), labels = c("Undisturbed", "Disturbed"), name = "Disturbance") +
  scale_fill_manual(values = c("#999999", "#E69F00"), labels = c("Undisturbed", "Disturbed"), name = "Disturbance") +
  facet_wrap(.~ species, labeller = as_labeller(species_names), scales = "free", ncol = 3) + #separate by species and rename with full species names
  coord_trans(y = "log1p") + #log10 y axis 
  labs(x = expression(paste("Plant size [", cm^2, "]")),
       y = expression(paste("Density of reproductive structures [counts/", cm^2, "]"))) +
  mytheme

totalReproPlotAreaBySpecies <- tag_facet(totalReproPlotAreaBySpecies)

ggsave(filename = "../figures/publication/totalReproPlotAreaBySpecies", plot = totalReproPlotAreaBySpecies, device = "pdf", dpi = 600, width = 20, height = 10, units = "in")

# Plot size vs number of buds, flowers, and fruits by disturbed/undisturbed

totalReproPlotBySpecies <- ggplot(dat, aes(x= plantArea_cm2, y= totalReproStruct, color = dist)) +
  geom_point() +
  stat_smooth(aes(fill = dist), method = "lm", formula = y ~ x, geom = "ribbon", alpha = 0.3, size = 0) +
  stat_smooth(aes(color = dist), method = "lm", formula = y ~ x, geom = "line", size=1) +
  scale_color_manual(values = c("#999999", "#E69F00"), labels = c("Undisturbed", "Disturbed"), name = "Disturbance") +
  scale_fill_manual(values = c("#999999", "#E69F00"), labels = c("Undisturbed", "Disturbed"), name = "Disturbance") +
  facet_wrap(.~ species, labeller = as_labeller(species_names), scales = "free", ncol = 3) + #separate by species and rename with full species names
  labs(x = expression(paste("Plant size [", cm^2, "]")),
       y = expression(paste("Number of reproductive structures"))) +
  mytheme

totalReproPlotBySpecies <- tag_facet(totalReproPlotBySpecies)

ggsave(filename = "../figures/publication/totalReproPlotBySpecies", plot = totalReproPlotBySpecies, device = "pdf", dpi = 600, width = 20, height = 10, units = "in")




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




