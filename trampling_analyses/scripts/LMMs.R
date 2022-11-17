### *** NEED TO RUN MERGE_FIELDDATA.R BEFORE RUNNING ANYTHING HERE

# Aims:
# 1. Fit LMMs to test effects of trampling and elevation on plant responses
# 2. Check model residuals & goodness of fit

# Author: Nathalie Chardon
# Date created: 11 Nov 2022
# Date updated: 16 Nov 2022

###*** install these packages
# # LIBRARIES # # 
library(tidyverse)
library(lme4)
library(lmerTest)


rm(list=ls()) 

###*** create new object 'comp_dat' for your working directory
# # WORKING DIRECTORIES # #
comp_dat <- '~/Desktop/Code/Garibaldi/trampling_analyses/compiled_data/' #WD for NC


# # INPUT FILES # #
setwd(comp_dat)
load('quad.RData') #gps & transect data matched to quad data (merge_fielddata.R)


# # OUTPUT FILES # #




####################################################################################################

###*** run this section
# # DATA # # 

####################################################################################################

# Data
dat <- quad #generic dataframe name
str(dat)


# Convert categorical predictor variables to factor
ff <- c('transect', 'species', 'dist')
dat[ff] <- lapply(dat[ff], as.factor)


# Check for correlation in predictor variables
# - not needed for this analyses because only 1 continuous variable - 


# Calculate variance inflation factor (VIF)
# - not needed for this analyses because only 1 continuous variable - 




####################################################################################################

###*** modify below to test other variable you are interested in (e.g., plant diameter)
# # COMPILE MODEL # # 

####################################################################################################

# Check out this resource for a visual explanation of hierarchical models: 
# http://mfviz.com/hierarchical-models/

# Question: Does disturbance differentially affect plant height at different elevations?

# Fit model
mod <- lmer(height_mm ~ dist * altitude + (1|transect) + (1|species), data = dat)

# Model summary
summary(mod)

# Goodness of fit
ff <- fitted(mod) #predicted values
oo <- dat$height_mm #observed values
cor(ff, oo) #correlation between predicted and observed values




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


# TO DO: Look up additional assumptions to check with hierarchical models


