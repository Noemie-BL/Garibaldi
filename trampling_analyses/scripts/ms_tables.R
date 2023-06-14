# Aims:
# 1. Manuscript tables

## IN PROGRESS: make new script or modify bayesian.R?

# Author: Nathalie Chardon
# Date created: 14 June 2023
# Date updated: 14 June 2023 (NC)

# # LIBRARIES # #


rm(list=ls()) 


# # INPUT FILES # #
load('trampling_analyses/compiled_data/quad.RData') ##updated with reproductive metric & plant area (repro.R)
#[individual BRMS model files, see below]

# # OUTPUT FILES # #




####################################################################################################

# # Table [RESULTS]: Estimated parameters of hierarchical models

####################################################################################################

# PHYEMP Height
mod <- readRDS('trampling_analyses/outputs/ms_results/phyemp_height_nb.rds')

# PHYEMP Diameter
mod <- readRDS('trampling_analyses/outputs/ms_results/phyemp_diam_nb.rds')

# PHYEMP Reproduction
mod <- readRDS('trampling_analyses/outputs/ms_results/phyemp_repro_beta.rds')


# CASMER Height
mod <- readRDS('trampling_analyses/outputs/ms_results/height_nb_casmer.rds')

# CASMER Diameter
mod <- readRDS('trampling_analyses/outputs/ms_results/diam_nb_casmer.rds')

# CASMER Reproduction
mod <- readRDS('trampling_analyses/outputs/ms_results/repro_beta_casmer.rds')


# VACOVA Height
mod <- readRDS('trampling_analyses/outputs/ms_results/height_nb_vacova.rds')

# VACOVA Diameter
mod <- readRDS('trampling_analyses/outputs/April2023/diam_nb_vacova.rds')

# VACOVA Reproduction
mod <- readRDS('trampling_analyses/outputs/ms_results/repro_beta_vacova.rds')


# CARSPP Height
mod <- readRDS('trampling_analyses/outputs/ms_results/height_nb_carspp.rds')

# CARSPP Diameter
mod <- readRDS('trampling_analyses/outputs/ms_results/diam_nb_carspp.rds')


# PLANT PERCENT COVER
mod <- readRDS('trampling_analyses/outputs/ms_results/perc-cov_beta.rds')




####################################################################################################

# # Table S[BRMS]: Model fitting details

####################################################################################################

# Manually created when running models