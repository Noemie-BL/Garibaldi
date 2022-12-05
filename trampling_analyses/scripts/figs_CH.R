# Aims:
# 1. Visualize data

# Author: Nathalie Chardon
# Date created: 11 Nov 2022
# Date updated: 16 Nov 2022

# # LIBRARIES # #
library(ggplot2)
library(ggsignif)

rm(list=ls()) 


# # WORKING DIRECTORIES # #
# comp_dat <- '~/Desktop/Code/Garibaldi/trampling_analyses/compiled_data/' #WD for NC
# figs <- '~/Desktop/Code/Garibaldi/trampling_analyses/figures/BC_PARF/' #WD for NC
comp_dat <- "C:/Users/Carly/Documents/Code/Garibaldi/trampling_analyses/compiled_data" # WD Carly
figs <- "C:/Users/Carly/Documents/Code/Garibaldi/trampling_analyses/figures/BC_PARF" #WD Carly


# # INPUT FILES # #
setwd(comp_dat)
load('quad.RData') #gps & transect data matched to quad data (merge_fielddata.R)


# # OUTPUT FILES # #




####################################################################################################

### *** add new modified code with other variables
# # DATA PLOTS # # 

####################################################################################################

# # DATA # #

setwd(figs)
dat <- quad #generic dataframe name

# Convert categorical predictor variables to factor
ff <- c('transect', 'species', 'dist')
dat[ff] <- lapply(dat[ff], as.factor)


# # PLOT THEME # #

setwd(figs) #location to save figures

mytheme <-   theme_classic() +
  theme(axis.text.x = element_text(colour = 'black', size = 10), #x axis text size
        axis.text.y = element_text(colour = 'black', size = 10), #y axis text size
        axis.title.x = element_text(size = 18), #x axis label size
        axis.title.y = element_text(size = 18), #y axis label size
        plot.title = element_text(size = 30, #title size
                                  hjust = 0.5), #align title to center
        legend.title = element_text(size = 24), #Legend title text size
        legend.text = element_text(size = 22)) #Actual legend text


# # PLOTS

# height ~ elev by disturbance & species: only Carex spp shows an interesting interaction with elev, so leave out elev for now

x <- dat$altitude
y <- dat$height_mm

ggplot(dat, aes(x, y, color = dist)) +
  geom_point(size = 3, alpha = 0.5) +
  ylab('Plant Height [mm]') + xlab('Elevation [m]') + labs(title = '') + 
  stat_smooth(method = "lm", formula = y ~ x, geom = "smooth") +
  facet_wrap(~species, scales = "free_y") + #separate panels into species
  theme_classic()


### *** ex of other plots to try: boxplots of disturbance vs. no dist regardless of elevation

# height boxplot of disturbance vs no distubance by species

#full species names
species_names <- c('carspp'= "Carex spp.", 'casmer' = "Cassiope mertensiana", 'phyemp' = "Phyllodoce empetriformis", 'phygla' = "Phyllodoce glanduliflora", 'vacova' = "Vaccinium ovalifolium")

gplot <- ggplot(dat, aes(x=dist, y=height_mm, fill=dist)) +
  geom_boxplot(fill=c('darkolivegreen', 'steelblue2', 'darkolivegreen', 'steelblue2', 'darkolivegreen', 'steelblue2', 'darkolivegreen', 'steelblue2', 'darkolivegreen', 'steelblue2')) +
  facet_grid(.~ species, labeller = as_labeller(species_names)) + #separate by species and rename with full species names
  # geom_signif(comparisons = list(c("0", "1")), map_signif_level=TRUE) + #significance stars --> cannot run Wilcox test with nested sampling design
  scale_x_discrete(labels=c("0" = "Off Trail", "1" = "On Trail")) + #rename dist variable from 0 and 1
  theme(axis.text.x = element_text(angle = 60, hjust=1)) + #angle text
  ylab("Height (mm)") +
  theme(legend.position = "none") + #no legend 
  mytheme +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))
gplot

ggsave(gplot, filename = 'height_dist.pdf')


# max diameter boxplot of disturbance vs no distubance by species

species_names <- c('carspp'= "Carex spp.", 'casmer' = "Cassiope mertensiana", 'phyemp' = "Phyllodoce empetriformis", 'phygla' = "Phyllodoce glanduliflora", 'vacova' = "Vaccinium ovalifolium")

widthplot <- ggplot(dat, aes(x=dist, y=mxdiam_mm, fill=dist)) +
  geom_boxplot(fill=c('darkolivegreen', 'steelblue2', 'darkolivegreen', 'steelblue2', 'darkolivegreen', 'steelblue2', 'darkolivegreen', 'steelblue2', 'darkolivegreen', 'steelblue2')) + # colour palette
  facet_grid(.~ species, labeller = as_labeller(species_names)) + #separate by species and rename with full species names
  # geom_signif(comparisons = list(c("0", "1")), map_signif_level=TRUE) + #significance stars
  scale_x_discrete(labels=c("0" = "Off Trail", "1" = "On Trail")) + #rename dist variable from 0 and 1
  theme(axis.text.x = element_text(angle = 60, hjust=1)) + #angle text
  ylab("Maximum diameter (mm)") +
  theme(legend.position = "none") #no legend
  mytheme
widthplot

ggsave(widthplot, filename= 'width_dist.pdf')


# bud number boxplot of disturbance vs no distubance by species

budplot <- ggplot(dat, aes(x=dist, y=buds, fill=dist)) +
  geom_boxplot() +
  facet_grid(.~ species, labeller = as_labeller(species_names)) + #separate by species and rename with full species names
  # geom_signif(comparisons = list(c("0", "1")), map_signif_level=TRUE) + #significance stars
  scale_x_discrete(labels=c("0" = "Off Trail", "1" = "On Trail")) + #rename dist variable from 0 and 1
  theme(axis.text.x = element_text(angle = 60, hjust=1)) + #angle text
  ylab("Buds") +
  theme(legend.position = "none") #no legend
budplot

ggsave(budplot, filename = 'bud_dist.pdf')

# flower number boxplot of disturbance vs no distubance by species

flowersplot <- ggplot(dat, aes(x=dist, y=flws, fill=dist)) +
  geom_boxplot() +
  facet_grid(.~ species, labeller = as_labeller(species_names)) + #separate by species and rename with full species names
  # geom_signif(comparisons = list(c("0", "1")), map_signif_level=TRUE) + #significance stars
  scale_x_discrete(labels=c("0" = "Off Trail", "1" = "On Trail")) + #rename dist variable from 0 and 1
  theme(axis.text.x = element_text(angle = 60, hjust=1)) + #angle text
  ylab("Flowers") +
  theme(legend.position = "none") #no legend
flowersplot

ggsave(flowersplot, filename = 'flowers_dist.pdf')

# fruit number boxplot of disturbance vs no distubance by species

ggplot(dat, aes(x=dist, y=frts, fill=dist)) +
  geom_boxplot() +
  facet_grid(.~ species, labeller = as_labeller(species_names)) + #separate by species and rename with full species names
  # geom_signif(comparisons = list(c("0", "1")), map_signif_level=TRUE) + #significance stars
  scale_x_discrete(labels=c("0" = "Off Trail", "1" = "On Trail")) + #rename dist variable from 0 and 1
  theme(axis.text.x = element_text(angle = 60, hjust=1)) + #angle text
  ylab("Fruits") +
  theme(legend.position = "none") #no legend


# bud flower and fruit number boxplot of disturbance vs no distubance by species

dat$BudsFlwsFrts <- rowSums(dat[ , c(5,6,7)])

ggplot(dat, aes(x=dist, y=BudsFlwsFrts, fill=dist)) +
  geom_boxplot() +
  facet_grid(.~ species, labeller = as_labeller(species_names)) + #separate by species and rename with full species names
  # geom_signif(comparisons = list(c("0", "1")), map_signif_level=TRUE) + #significance stars
  scale_x_discrete(labels=c("0" = "Off Trail", "1" = "On Trail")) + #rename dist variable from 0 and 1
  theme(axis.text.x = element_text(angle = 60, hjust=1)) + #angle text
  ylab("Reproductive structures") +
  theme(legend.position = "none") #no legend

# # Below is my best attempt for my weird graph # #

dat$Buds
ggplot(dat, aes(BudsPerSpecies)) +
ggplot(dat, aes(FlwsPerSpecies)) +
ggplot(dat, aes (FrtsPerSpecies)) +
  geom_bar() +
  facet_grid(.~ species, labeller = as_labeller(species_names)) + 
  theme_minimal() +
  labs(y = 'test label', title = 'test title')