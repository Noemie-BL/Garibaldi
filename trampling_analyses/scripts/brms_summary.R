# Aims: Interpreting brms summary table

# Author: Nathalie Chardon
# Date created: 31 July 2023
# Date updated: 31 July 2023 (NC)

# # LIBRARIES # #
library(tidyverse)
library(ggplot2)

rm(list=ls()) 


# # INPUT FILES # #
load('trampling_analyses/compiled_data/quad.RData') ##updated with reproductive metric & plant area (repro.R)




####################################################################################################

# # DIFFERENCES IN RAW DATA # #

####################################################################################################

# Summary output does not match conditional effects plot
dat <- quad %>%
  filter(species == 'carspp')
plot(height_mm ~ dist, data = dat)

# Models where plots match summary output
plot(perc.cov ~ dist, data = quad)

dat <- quad %>%
  filter(species == 'phyemp')
plot(mxdiam_mm ~ dist, data = dat)

dat <- quad %>%
  filter(species == 'casmer')
plot(rel_repro ~ dist, data = dat)

dat <- quad %>%
  filter(species == 'vacova')
plot(height_mm ~ dist, data = dat)
plot(mxdiam_mm ~ dist, data = dat)


# Carex height with elevation
quad %>%
  filter(species == 'carspp') %>% 
  ggplot(aes(altitude, height_mm, color = dist)) +
  geom_point()


