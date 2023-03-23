# Aims:
# 1. Analyses in Bayesian framework to model disturbance effects on plant height, diameter, and repro
# 2. Visualize model fit
# 3. Plot results

# Author: Nathalie Chardon
# Date created: 13 Mar 2023
# Date updated: 13 Mar 2023 (NC)

# # LIBRARIES # #
library(rjags)
library(R2jags)
library(ggplot2)
library(brms)
library(tidybayes)
library(emmeans)
library(priorsense) # remotes::install_github("n-kall/priorsense")
library(bayesplot)
library(tidyverse)
library(loo)
library(distributional)

rm(list=ls()) 

# # INPUT FILES # #
load('trampling_analyses/compiled_data/quad.RData') #updated with reproductive metric & plant area (LMMs.R)


# # OUTPUT FILES # #
# [jags files, see below]




####################################################################################################

# # SET UP DATA # # 

####################################################################################################

# Coding Club tutorial: https://ourcodingclub.github.io/tutorials/brms/
# Towards Data Science: https://towardsdatascience.com/evaluating-bayesian-mixed-models-in-r-python-27d344a03016

# Data
load('trampling_analyses/compiled_data/quad.RData') #updated with reproductive metric & plant area (LMMs.R)

str(quad) #check that categorical explanatory variables are factors, others numeric


# Skew function (from: https://towardsdatascience.com/evaluating-bayesian-mixed-models-in-r-python-27d344a03016_)
skew <- function(y){ # Fisher-Pearson Skew function based on NIST definition
  n <- length(y)
  dif <- y - mean(y)
  skew_stat <- (sqrt(n-1)/(n-2))*n *(sum(dif^3)/(sum(dif^2)^1.5))
  return(skew_stat)
}




####################################################################################################

# # HEIGHT # #
# Family: negative binomial
# Fixed effects: dist * altitude
# Random effects: (1|trans.pair) + (1|species)

####################################################################################################

# # Data 
dat <- quad %>% #rename DF
  filter_at(vars(height_mm, dist, altitude), all_vars(!is.na(.))) #remove NAs in variables used in model


# # Model 
height_nb <- brms::brm(height_mm ~ dist * altitude + (1|trans.pair) + (1|species),
                       data = dat, family = negbinomial(link = "log", link_shape = "log"), 
                       # fitting information
                       chains = 3, iter = 3000, warmup = 1000, cores = 4, #for computers with 4 cores
                       file = 'trampling_analyses/outputs/height_nb.rds', file_refit = 'on_change')
mod <- height_nb #generic model name

## PROBLEM: transitions after warmup exceeded max treedepth
## SOLUTION: increase max_treedepth > 10 BUT this is not recommended and this warning is only an 
## efficiency, not modeling problem (https://mc-stan.org/misc/warnings.html#maximum-treedepth-exceeded)

# # Posterior Distribution 

# Summary of posterior distribution for fixed and random effects: 
# Estimate: mean of posterior distribution
# Est. Error: error associated with mean (standard error)
# CI intervals: if CI intervals encompass 0 => can't be sure effect isn't 0
summary(mod) 

plot(conditional_effects(mod), ask = FALSE) #fitted parameters and their CI


# # Prior distribution

# What about priors? brms will try to pick reasonable defaults, which you can see:
prior_summary(mod) #can define priors in brm(priors = ...)


# Do priors overwhelm likelihood?  
powerscale_sensitivity(mod) #look at 'diagnosis' column to see if prior isn't appropriate


# # Model Fit

# sample size (Bulk_ESS & Tail_ESS) should > 1000 & rhat < 1.1
summary(mod) 

## PROBLEM: low ESS for dist and dist*altitude
## SOLUTION: increase sampling frequency, increase chain length (https://beast.community/ess_tutorial)

plot(mod) #model convergence (L: does distribution mean match estimate? R: did all values get explored?) 

# Posterior predictive check: Does data match model? (could be wrong distribution, not all effects modeled)
pp_check(mod, ndraws = 100) #posterior predictive checks - are predicted values similar to posterior distribution?

# Pairs plots to diagnose sampling problems (should show Gaussian blobs)
pairs(mod)

## PROBLEM: not all pairs show Gaussian blobs, means model isn't 'well-behaved'
## SOLUTION: leave as is and report in results

# Skewness: observed (black line) and simulated (grey distribution) SKEW metric (1000 simulated datasets)
ppc_stat(y = dat$height_mm, 
         yrep = posterior_predict(mod, ndraws = 1000),
         stat = "skew")

## PROBLEM: skew of data not modeled well
## SOLUTION: report in results

# Leave-one-out Crossvalidation (LOO) for marginal (pointwise) posterior predictive checks
model_loo <- loo(mod, save_psis = TRUE, cores=4) #higher elpd => better fit 

w <- weights(model_loo$psis_object)

# Probability integral transform (PIT) uses CDF properties to convert continuous random 
# variables into uniformly distributed ones. However, this only holds exactly if the distribution 
# used to generate the continuous random variables is the true distribution.
# -> If LOO-PIT values concentrated at 0/1 => possibly under-dispersed model
# -> If LOO-PIT values concentrated near 0.5 => possibly over-dispersed model
ppc_loo_pit_overlay(dat$height_mm, 
                    posterior_predict(mod), 
                    lw = w)


# # Conclusion: low ESS for some parameters and some model fit issues



