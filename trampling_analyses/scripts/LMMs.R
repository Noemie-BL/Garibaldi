### *** NEED TO RUN MERGE_FIELDDATA.R BEFORE RUNNING ANYTHING HERE

# Aims:
# 1. Fit LMMs to test effects of trampling and elevation on plant responses
# 2. Check model residuals & goodness of fit

# Author: Nathalie Chardon
# Date created: 11 Nov 2022
# Date updated: 11 Nov 2022

# # LIBRARIES # #
library(lme4)


rm(list=ls()) 


# # WORKING DIRECTORIES # #
comp_dat <- '~/Desktop/Code/Garibaldi/trampling_analyses/compiled_data/' #WD for NC


# # INPUT FILES # #
setwd(comp_dat)
load('quad.RData') #gps & transect data matched to quad data (merge_fielddata.R)


# # OUTPUT FILES # #




####################################################################################################

# # COMPILE MODEL # # 

####################################################################################################

# Check out this resource for a visual explanation of hierarchical models: 
# http://mfviz.com/hierarchical-models/

# Data
dat <- quad #generic dataframe name


# Convert categorical predictor variables to factor
### *** do this

# Check for correlation in predictor variables
# - not needed for this analyses because only 1 continuous variable - 

# Calculate variance inflation factor (VIF)
# - not needed for this analyses because only 1 continuous variable - 


# Fit model
### *** check variable names

mod <- lmer(height ~ elevation * trail + species + (1|transect), data = dat)


# Goodness of fit
### *** do this (e.g., extract from model object or get_gof from package modelsummary)



####################################################################################################

# # CHECK MODEL ASSUMPTIONS # # 

####################################################################################################

# Homogeneity of variance: variance in residuals is constant

rr <- resid(mod) #residuals
ff <- fitted(mod) #fitted values

plot(rr ~ ff, xlab = 'Predicted Values', ylab = 'Residuals')


# Assumption of normality: residuals are normally distributed

par(mfrow = c(1,2))

hist(rr, breaks = 100)

qqnorm(rr, main="normal qq-plot, residuals")
qqline(rr)


# Look up additional assumptions to check with hierarchical models


