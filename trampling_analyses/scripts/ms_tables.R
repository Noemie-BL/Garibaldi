# Aims: Manuscript tables
# 1. Bayesian results
# 2. BRMS details (created manually)


# Author: Nathalie Chardon
# Date created: 14 June 2023
# Date updated: 14 June 2023 (NC)

# # LIBRARIES # #
library(tidyverse)


rm(list=ls()) 


# # INPUT FILES # #
#[individual BRMS model files, see below]


# # OUTPUT FILES # #
load('trampling_analyses/outputs/ms_results/brm_table.RData') #model results for species traits (ms_tables.R)
brm.tab <- read.csv('trampling_analyses/outputs/ms_results/brm_table.csv') #model results for species traits (ms_tables.R)




####################################################################################################

# # Table [RESULTS]: Estimated parameters of hierarchical models

####################################################################################################

## PHYEMP ## --------------------------------------------------------------------------------------
sp <- 'phyemp'

# PHYEMP Height
mod <- readRDS('trampling_analyses/outputs/ms_results/phyemp_height_nb.rds')

ss <- summary(mod)
ii <- paste(round(ss$fixed[1, 1], 2), '(', round(ss$fixed[1, 3], 2), ',', round(ss$fixed[1, 4], 2), ')')
dd <- paste(round(ss$fixed[2, 1], 2), '(', round(ss$fixed[2, 3], 2), ',', round(ss$fixed[2, 4], 2), ')')
ee <- paste(round(ss$fixed[3, 1], 2), '(', round(ss$fixed[3, 3], 2), ',', round(ss$fixed[3, 4], 2), ')')

brm.res <- data.frame(Species = sp, Trait = ss$formula[4], N = ss$nobs, 
                      Iterations = ss$iter, Intercept = ii, Disturbance = dd, 
                      Elevation = ee)

brm.tab <- brm.res
brm.tab


# PHYEMP Diameter
mod <- readRDS('trampling_analyses/outputs/ms_results/phyemp_diam_nb.rds')

ss <- summary(mod)
ii <- paste(round(ss$fixed[1, 1], 2), '(', round(ss$fixed[1, 3], 2), ',', round(ss$fixed[1, 4], 2), ')')
dd <- paste(round(ss$fixed[2, 1], 2), '(', round(ss$fixed[2, 3], 2), ',', round(ss$fixed[2, 4], 2), ')')
ee <- paste(round(ss$fixed[3, 1], 2), '(', round(ss$fixed[3, 3], 2), ',', round(ss$fixed[3, 4], 2), ')')

brm.res <- data.frame(Species = sp, Trait = ss$formula[4], N = ss$nobs, 
                      Iterations = ss$iter, Intercept = ii, Disturbance = dd, 
                      Elevation = ee)
brm.res

brm.tab <- bind_rows(brm.tab, brm.res)
brm.tab


# PHYEMP Reproduction
mod <- readRDS('trampling_analyses/outputs/ms_results/phyemp_repro_beta.rds')

ss <- summary(mod)
ii <- paste(round(ss$fixed[1, 1], 2), '(', round(ss$fixed[1, 3], 2), ',', round(ss$fixed[1, 4], 2), ')')
dd <- paste(round(ss$fixed[2, 1], 2), '(', round(ss$fixed[2, 3], 2), ',', round(ss$fixed[2, 4], 2), ')')
ee <- paste(round(ss$fixed[3, 1], 2), '(', round(ss$fixed[3, 3], 2), ',', round(ss$fixed[3, 4], 2), ')')

brm.res <- data.frame(Species = sp, Trait = ss$formula[4], N = ss$nobs, 
                      Iterations = ss$iter, Intercept = ii, Disturbance = dd, 
                      Elevation = ee)
brm.res

brm.tab <- bind_rows(brm.tab, brm.res)
brm.tab


## CASMER ## --------------------------------------------------------------------------------------
sp <- 'casmer'

# CASMER Height
mod <- readRDS('trampling_analyses/outputs/ms_results/height_nb_casmer.rds')

ss <- summary(mod)
ii <- paste(round(ss$fixed[1, 1], 2), '(', round(ss$fixed[1, 3], 2), ',', round(ss$fixed[1, 4], 2), ')')
dd <- paste(round(ss$fixed[2, 1], 2), '(', round(ss$fixed[2, 3], 2), ',', round(ss$fixed[2, 4], 2), ')')
ee <- paste(round(ss$fixed[3, 1], 2), '(', round(ss$fixed[3, 3], 2), ',', round(ss$fixed[3, 4], 2), ')')

brm.res <- data.frame(Species = sp, Trait = ss$formula[4], N = ss$nobs, 
                      Iterations = ss$iter, Intercept = ii, Disturbance = dd, 
                      Elevation = ee)
brm.res

brm.tab <- bind_rows(brm.tab, brm.res)
brm.tab


# CASMER Diameter
mod <- readRDS('trampling_analyses/outputs/ms_results/diam_nb_casmer.rds')

ss <- summary(mod)
ii <- paste(round(ss$fixed[1, 1], 2), '(', round(ss$fixed[1, 3], 2), ',', round(ss$fixed[1, 4], 2), ')')
dd <- paste(round(ss$fixed[2, 1], 2), '(', round(ss$fixed[2, 3], 2), ',', round(ss$fixed[2, 4], 2), ')')
ee <- paste(round(ss$fixed[3, 1], 2), '(', round(ss$fixed[3, 3], 2), ',', round(ss$fixed[3, 4], 2), ')')

brm.res <- data.frame(Species = sp, Trait = ss$formula[4], N = ss$nobs, 
                      Iterations = ss$iter, Intercept = ii, Disturbance = dd, 
                      Elevation = ee)
brm.res

brm.tab <- bind_rows(brm.tab, brm.res)
brm.tab


# CASMER Reproduction
mod <- readRDS('trampling_analyses/outputs/ms_results/repro_beta_casmer.rds')

ss <- summary(mod)
ii <- paste(round(ss$fixed[1, 1], 2), '(', round(ss$fixed[1, 3], 2), ',', round(ss$fixed[1, 4], 2), ')')
dd <- paste(round(ss$fixed[2, 1], 2), '(', round(ss$fixed[2, 3], 2), ',', round(ss$fixed[2, 4], 2), ')')
ee <- paste(round(ss$fixed[3, 1], 2), '(', round(ss$fixed[3, 3], 2), ',', round(ss$fixed[3, 4], 2), ')')

brm.res <- data.frame(Species = sp, Trait = ss$formula[4], N = ss$nobs, 
                      Iterations = ss$iter, Intercept = ii, Disturbance = dd, 
                      Elevation = ee)
brm.res

brm.tab <- bind_rows(brm.tab, brm.res)
brm.tab


## VACOVA ## --------------------------------------------------------------------------------------
sp <- 'vacova'

# VACOVA Height
mod <- readRDS('trampling_analyses/outputs/ms_results/height_nb_vacova.rds')

ss <- summary(mod)
ii <- paste(round(ss$fixed[1, 1], 2), '(', round(ss$fixed[1, 3], 2), ',', round(ss$fixed[1, 4], 2), ')')
dd <- paste(round(ss$fixed[2, 1], 2), '(', round(ss$fixed[2, 3], 2), ',', round(ss$fixed[2, 4], 2), ')')
ee <- paste(round(ss$fixed[3, 1], 2), '(', round(ss$fixed[3, 3], 2), ',', round(ss$fixed[3, 4], 2), ')')

brm.res <- data.frame(Species = sp, Trait = ss$formula[4], N = ss$nobs, 
                      Iterations = ss$iter, Intercept = ii, Disturbance = dd, 
                      Elevation = ee)
brm.res

brm.tab <- bind_rows(brm.tab, brm.res)
brm.tab


# VACOVA Diameter
mod <- readRDS('trampling_analyses/outputs/ms_results/diam_nb_vacova.rds')

ss <- summary(mod)
ii <- paste(round(ss$fixed[1, 1], 2), '(', round(ss$fixed[1, 3], 2), ',', round(ss$fixed[1, 4], 2), ')')
dd <- paste(round(ss$fixed[2, 1], 2), '(', round(ss$fixed[2, 3], 2), ',', round(ss$fixed[2, 4], 2), ')')
ee <- paste(round(ss$fixed[3, 1], 2), '(', round(ss$fixed[3, 3], 2), ',', round(ss$fixed[3, 4], 2), ')')

brm.res <- data.frame(Species = sp, Trait = ss$formula[4], N = ss$nobs, 
                      Iterations = ss$iter, Intercept = ii, Disturbance = dd, 
                      Elevation = ee)
brm.res

brm.tab <- bind_rows(brm.tab, brm.res)
brm.tab


# VACOVA Reproduction
mod <- readRDS('trampling_analyses/outputs/ms_results/repro_beta_vacova.rds')

ss <- summary(mod)
ii <- paste(round(ss$fixed[1, 1], 2), '(', round(ss$fixed[1, 3], 2), ',', round(ss$fixed[1, 4], 2), ')')
dd <- paste(round(ss$fixed[2, 1], 2), '(', round(ss$fixed[2, 3], 2), ',', round(ss$fixed[2, 4], 2), ')')
ee <- paste(round(ss$fixed[3, 1], 2), '(', round(ss$fixed[3, 3], 2), ',', round(ss$fixed[3, 4], 2), ')')

brm.res <- data.frame(Species = sp, Trait = ss$formula[4], N = ss$nobs, 
                      Iterations = ss$iter, Intercept = ii, Disturbance = dd, 
                      Elevation = ee)
brm.res

brm.tab <- bind_rows(brm.tab, brm.res)
brm.tab


## CARSPP ## --------------------------------------------------------------------------------------
sp <- 'carspp'

# CARSPP Height
mod <- readRDS('trampling_analyses/outputs/ms_results/height_nb_carspp.rds')

ss <- summary(mod)
ii <- paste(round(ss$fixed[1, 1], 2), '(', round(ss$fixed[1, 3], 2), ',', round(ss$fixed[1, 4], 2), ')')
dd <- paste(round(ss$fixed[2, 1], 2), '(', round(ss$fixed[2, 3], 2), ',', round(ss$fixed[2, 4], 2), ')')
ee <- paste(round(ss$fixed[3, 1], 2), '(', round(ss$fixed[3, 3], 2), ',', round(ss$fixed[3, 4], 2), ')')

brm.res <- data.frame(Species = sp, Trait = ss$formula[4], N = ss$nobs, 
                      Iterations = ss$iter, Intercept = ii, Disturbance = dd, 
                      Elevation = ee)
brm.res

brm.tab <- bind_rows(brm.tab, brm.res)
brm.tab


# CARSPP Diameter
mod <- readRDS('trampling_analyses/outputs/ms_results/diam_nb_carspp.rds')

ss <- summary(mod)
ii <- paste(round(ss$fixed[1, 1], 2), '(', round(ss$fixed[1, 3], 2), ',', round(ss$fixed[1, 4], 2), ')')
dd <- paste(round(ss$fixed[2, 1], 2), '(', round(ss$fixed[2, 3], 2), ',', round(ss$fixed[2, 4], 2), ')')
ee <- paste(round(ss$fixed[3, 1], 2), '(', round(ss$fixed[3, 3], 2), ',', round(ss$fixed[3, 4], 2), ')')

brm.res <- data.frame(Species = sp, Trait = ss$formula[4], N = ss$nobs, 
                      Iterations = ss$iter, Intercept = ii, Disturbance = dd, 
                      Elevation = ee)
brm.res

brm.tab <- bind_rows(brm.tab, brm.res)
brm.tab


## PLANT PERCENT COVER ## --------------------------------------------------------------------------
sp <- '[All Plants]'

# PLANT PERCENT COVER
mod <- readRDS('trampling_analyses/outputs/ms_results/perc-cov_beta.rds')

ss <- summary(mod)
ii <- paste(round(ss$fixed[1, 1], 2), '(', round(ss$fixed[1, 3], 2), ',', round(ss$fixed[1, 4], 2), ')')
dd <- paste(round(ss$fixed[2, 1], 2), '(', round(ss$fixed[2, 3], 2), ',', round(ss$fixed[2, 4], 2), ')')
ee <- paste(round(ss$fixed[3, 1], 2), '(', round(ss$fixed[3, 3], 2), ',', round(ss$fixed[3, 4], 2), ')')

brm.res <- data.frame(Species = sp, resp = 'Percent Cover', N = ss$nobs, 
                      Iterations = ss$iter, Intercept = ii, Disturbance = dd, 
                      Elevation = ee)
brm.res

brm.tab <- bind_rows(brm.tab, brm.res)
brm.tab


## CLEAN UP & SAVE RESULTS TABLE ## ----------------------------------------------------------------
brm.tab <- brm.tab %>% 
  rename(Trait = resp) #rename column

brm.tab <- brm.tab %>% 
  mutate(Species = if_else(Species == 'phyemp', 'P. empetriformis', Species)) %>% #replace species codes with species names
  mutate(Species = if_else(Species == 'casmer', 'C. mertensiana', Species)) %>% 
  mutate(Species = if_else(Species == 'vacova', 'V. ovalifolium', Species)) %>% 
  mutate(Species = if_else(Species == 'carspp', 'Carex spp.', Species)) %>% 
  mutate(Trait = if_else(Trait == 'heightmm', 'Height', Trait)) %>% #rename trait variables
  mutate(Trait = if_else(Trait == 'mxdiammm', 'Diameter', Trait)) %>%
  mutate(Trait = if_else(Trait == 'relrepro', 'Reproduction', Trait))
brm.tab


save(brm.tab, file = 'trampling_analyses/outputs/ms_results/brm_table.RData')
write.csv(brm.tab, file = 'trampling_analyses/outputs/ms_results/brm_table.csv', row.names = F)




####################################################################################################

# # Table S[BRMS]: Model fitting details

####################################################################################################

# Manually created when running models in bayesian.R; iterations taken from Table [RESULTS] above


