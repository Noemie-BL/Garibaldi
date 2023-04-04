# Aims:
# 1. Analyses in Bayesian framework to model disturbance effects on plant height, diameter, repro, and plant cover
# 2. Visualize model fit
# 3. Plot results

# Author: Nathalie Chardon
# Date created: 13 Mar 2023
# Date updated: 4 Apr 2023 (NC)

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

# Transform 0s and 1s for Beta distribution repro models
quad <- quad %>% 
  mutate(rel_repro = if_else(rel_repro == 0, 0.0001, rel_repro)) %>% 
  mutate(rel_repro = if_else(rel_repro == 1, 0.9999, rel_repro)) 


summary(quad) #check that no infinite values exist

  
# Skew function (from: https://towardsdatascience.com/evaluating-bayesian-mixed-models-in-r-python-27d344a03016_)
skew <- function(y){ # Fisher-Pearson Skew function based on NIST definition
  n <- length(y)
  dif <- y - mean(y)
  skew_stat <- (sqrt(n-1)/(n-2))*n *(sum(dif^3)/(sum(dif^2)^1.5))
  return(skew_stat)
}




####################################################################################################

# # PHYEMP # #
# Family: negative binomial (height & diameter), Beta (rel_repro)
# Fixed effects: dist * altitude
# Random effects: (1|trans.pair)

####################################################################################################

# # Model for HEIGHT

dat <- quad %>% #rename DF
  filter_at(vars(height_mm, dist, altitude), all_vars(!is.na(.))) %>% #remove NAs in variables used in model
  filter(species == 'phyemp')

height_nb <- brms::brm(height_mm ~ dist * altitude + (1|trans.pair),
                       data = dat, family = negbinomial(link = "log", link_shape = "log"), 
                       # fitting information
                       chains = 3, iter = 5000, warmup = 1000, cores = 4, #for computers with 4 cores
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
ps <- powerscale_sensitivity(mod) #look at 'diagnosis' column to see if prior isn't appropriate
unique(ps$sensitivity$diagnosis)


# # Model Fit

# sample size (Bulk_ESS & Tail_ESS) should > 1000 & rhat < 1.1
summary(mod) 

plot(mod) #model convergence (L: does distribution mean match estimate? R: did all values get explored?) 

# Posterior predictive check: Does data match model? (could be wrong distribution, not all effects modeled)
pp_check(mod, ndraws = 100) #posterior predictive checks - are predicted values similar to posterior distribution?

# Pairs plots to diagnose sampling problems (should show Gaussian blobs)
pairs(mod)

# Skewness: observed (black line) and simulated (grey distribution) SKEW metric (1000 simulated datasets)
ppc_stat(y = dat$height_mm, 
         yrep = posterior_predict(mod, ndraws = 1000),
         stat = "skew")

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


# # Conclusion: 
## - major model fit issues (divergent transitions, low BF, large R-hat, low ESS) with 
## species as a random slope
## - for individual species models: data modeled much better, and good model fit! => use this approach


## Model for DIAMETER

dat <- quad %>% #rename DF
  filter_at(vars(mxdiam_mm, dist, altitude), all_vars(!is.na(.))) %>% #remove NAs in variables used in model
  filter(species == 'phyemp')

diam_nb <- brms::brm(mxdiam_mm ~ dist * altitude + (1|trans.pair),
                       data = dat, family = negbinomial(link = "log", link_shape = "log"), 
                       # fitting information
                       chains = 3, iter = 5000, warmup = 1000, cores = 4, #for computers with 4 cores
                       file = 'trampling_analyses/outputs/diam_nb.rds', file_refit = 'on_change')
mod <- diam_nb #generic model name

summary(mod) 
plot(conditional_effects(mod), ask = FALSE) #fitted parameters and their CI

prior_summary(mod) #can define priors in brm(priors = ...)
ps <- powerscale_sensitivity(mod) #look at 'diagnosis' column to see if prior isn't appropriate
unique(ps$sensitivity$diagnosis)

plot(mod) #model convergence (L: does distribution mean match estimate? R: did all values get explored?) 
pp_check(mod, ndraws = 100) #posterior predictive checks - are predicted values similar to posterior distribution?
pairs(mod)

ppc_stat(y = dat$mxdiam_mm, 
         yrep = posterior_predict(mod, ndraws = 1000),
         stat = "skew")
model_loo <- loo(mod, save_psis = TRUE, cores=4) #higher elpd => better fit 
w <- weights(model_loo$psis_object)
ppc_loo_pit_overlay(dat$mxdiam_mm, 
                    posterior_predict(mod), 
                    lw = w)

# # Conclusion: 
# very good model fit!


## Model for REPRO

dat <- quad %>% #rename DF
  filter_at(vars(rel_repro, dist, altitude), all_vars(!is.na(.))) %>% #remove NAs in variables used in model
  filter(species == 'phyemp')

# Compare beta and ZI beta models
repro_beta <- brms::brm(rel_repro ~ dist * altitude + (1|trans.pair),
                     data = dat, family = Beta(link = "logit", link_phi = "log"), 
                     init = '0',
                     
                     # fitting information
                     chains = 3, iter = 5000, warmup = 1000, cores = 4, #for computers with 4 cores
                     file = 'trampling_analyses/outputs/repro_beta2.rds', file_refit = 'on_change')

repro_zibeta <- brms::brm(rel_repro ~ dist * altitude + (1|trans.pair),
                        data = dat, family = zero_inflated_beta(link = "logit", link_phi = "log", link_zi = "logit"), 
                        init = '0',
                        
                        # fitting information
                        chains = 3, iter = 5000, warmup = 1000, cores = 4, #for computers with 4 cores
                        file = 'trampling_analyses/outputs/repro_zerobeta.rds', file_refit = 'on_change')

## PROBLEM: Stan model XXX does not contain samples
## DIAGNOSIS: Possible older brms version; Searching for initial values outside of possible interval 
## and should set init = '0' for Beta models (https://discourse.mc-stan.org/t/initialization-error-try-specifying-initial-values-reducing-ranges-of-constrained-values-or-reparameterizing-the-model/4401)
## SOLUTION: Update brms and devtools; Set init = '0' 

pp_check(repro_beta, ndraws = 100) #slightly better
pp_check(repro_zibeta, ndraws = 100)

pairs(repro_beta)
pairs(repro_zibeta)

ppc_stat(y = dat$rel_repro, yrep = posterior_predict(repro_beta, ndraws = 1000), stat = "skew")
ppc_stat(y = dat$rel_repro, yrep = posterior_predict(repro_zibeta, ndraws = 1000), stat = "skew")

beta_loo <- loo(repro_beta, save_psis = TRUE, cores=4)
w <- weights(beta_loo$psis_object)
ppc_loo_pit_overlay(dat$rel_repro, posterior_predict(repro_beta), lw = w)

zibeta_loo <- loo(repro_zibeta, save_psis = TRUE, cores=4)
w <- weights(zibeta_loo$psis_object)
ppc_loo_pit_overlay(dat$rel_repro, posterior_predict(repro_zibeta), lw = w)

loo_compare(beta_loo, zibeta_loo)

# # Conclusion: pp_check, skew, and PIT are almost identical and poor for both models, better elpd for Beta
# # => use Beta because better elpd & fits faster

mod <- repro_beta

summary(mod) 
plot(conditional_effects(mod), ask = FALSE) #fitted parameters and their CI

prior_summary(mod) #can define priors in brm(priors = ...)
ps <- powerscale_sensitivity(mod) #look at 'diagnosis' column to see if prior isn't appropriate
unique(ps$sensitivity$diagnosis)

plot(mod) #model convergence (L: does distribution mean match estimate? R: did all values get explored?) 
pp_check(mod, ndraws = 100) #posterior predictive checks - are predicted values similar to posterior distribution?
pairs(mod)

ppc_stat(y = dat$rel_repro, 
         yrep = posterior_predict(mod, ndraws = 1000), stat = "skew")
model_loo <- loo(mod, save_psis = TRUE, cores=4) #higher elpd => better fit 
w <- weights(model_loo$psis_object)
ppc_loo_pit_overlay(dat$rel_repro, 
                    posterior_predict(mod), lw = w)


####### PLOTS ########

mytheme <-   theme_classic() +
  theme(axis.text.x = element_text(colour = 'black', size = 25), #x axis text size
        axis.text.y = element_text(colour = 'black', size = 25), #y axis text size
        axis.title.x = element_text(size = 28), #x axis label size
        axis.title.y = element_text(size = 28), #x axis label size
        plot.title = element_text(size = 30, #title size
                                  hjust = 0.5), #align title to center
        legend.title = element_text(size = 24), legend.text = element_text(size = 22)) 

theme_set(mytheme)

(modelPlot <- dat %>%
    add_predicted_draws(mod) %>%  # adding the posterior distribution
    ggplot(aes(x = altitude, y = height_mm)) +  
    stat_lineribbon(aes(y = .prediction), .width = c(.95, .80, .50),  # regression line and CI
                    alpha = 0.5, colour = "black") +
    geom_point(data = dat) +   # raw data
    scale_fill_brewer(palette = "Greys") +
    ylab("Plant height (mm)\n") +  # latin name for red knot
    xlab("\nElevation (m)"))



(modelPlotDist <- dat %>%
    group_by(dist) %>%
    add_predicted_draws(mod) %>%
    ggplot(aes(x = altitude, y = height_mm, color = dist, fill = dist)) +
    stat_lineribbon(aes(y = .prediction), .width = c(.95), alpha = 0.33) +
    geom_point(data = dat) +
    ylab("Plant height (mm)\n") +
    xlab("\nElevation (m)") +
    theme(legend.title = element_blank()))




####################################################################################################

# # CASMER # #
# Family: negative binomial (height & diameter), Beta (rel_repro)
# Fixed effects: dist * altitude
# Random effects: (1|trans.pair)

####################################################################################################

# # Model for HEIGHT

dat <- quad %>% #rename DF
  filter_at(vars(height_mm, dist, altitude), all_vars(!is.na(.))) %>% 
  filter(species == 'casmer') ###*** change here

height_nb <- brms::brm(height_mm ~ dist * altitude + (1|trans.pair), data = dat, 
                       family = negbinomial(link = "log", link_shape = "log"), 
                       chains = 3, iter = 5000, warmup = 1000, cores = 4, 
                       file = 'trampling_analyses/outputs/height_nb_casmer.rds', ###*** change here
                       file_refit = 'on_change')
mod <- height_nb #generic model name

# Model results
summary(mod) 
plot(conditional_effects(mod), ask = FALSE) 

# Model fit
prior_summary(mod)
ps <- powerscale_sensitivity(mod) 
unique(ps$sensitivity$diagnosis)

plot(mod)  
pp_check(mod, ndraws = 100) 
pairs(mod)

ppc_stat(y = dat$height_mm, ###*** change here
         yrep = posterior_predict(mod, ndraws = 1000), stat = "skew")
model_loo <- loo(mod, save_psis = TRUE, cores=4) 
w <- weights(model_loo$psis_object)
ppc_loo_pit_overlay(dat$height_mm, ###*** change here
                    posterior_predict(mod), lw = w)

# # Conclusion: Very good fit; ignored because not a fit, but efficiency, issue:
# # 265 transitions after warmup that exceeded the maximum treedepth



## Model for DIAMETER

dat <- quad %>% #rename DF
  filter_at(vars(mxdiam_mm, dist, altitude), all_vars(!is.na(.))) %>% 
  filter(species == 'casmer') ###*** change here

diam_nb <- brms::brm(mxdiam_mm ~ dist * altitude + (1|trans.pair), data = dat, 
                     family = negbinomial(link = "log", link_shape = "log"), 
                     chains = 3, iter = 5000, warmup = 1000, cores = 4, 
                     file = 'trampling_analyses/outputs/diam_nb_casmer.rds', ###*** change here
                     file_refit = 'on_change')
mod <- diam_nb #generic model name

# Model results
summary(mod) 
plot(conditional_effects(mod), ask = FALSE) 

# Model fit
prior_summary(mod)
ps <- powerscale_sensitivity(mod) 
unique(ps$sensitivity$diagnosis)

plot(mod)  
pp_check(mod, ndraws = 100) 
pairs(mod)

ppc_stat(y = dat$mxdiam_mm, ###*** change here
         yrep = posterior_predict(mod, ndraws = 1000), stat = "skew")
model_loo <- loo(mod, save_psis = TRUE, cores=4) 
w <- weights(model_loo$psis_object)
ppc_loo_pit_overlay(dat$mxdiam_mm, ###*** change here
                    posterior_predict(mod), lw = w)

# # Conclusion: skew not modeled well, otherwise good
# # ignored: 1815 transitions after warmup that exceeded the maximum treedepth



## STOP 4 APRIL 2023



## Model for REPRO

dat <- quad %>% #rename DF
  filter_at(vars(rel_repro, dist, altitude), all_vars(!is.na(.))) %>% 
  filter(species == 'casmer') ###*** change here

repro_beta <- brms::brm(rel_repro ~ dist * altitude + (1|trans.pair), data = dat, 
                        family = Beta(link = "logit", link_phi = "log"), init = '0',
                        chains = 3, iter = 5000, warmup = 1000, cores = 4, 
                        file = 'trampling_analyses/outputs/repro_beta_casmer.rds', ###*** change here
                        file_refit = 'on_change')
mod <- repro_beta

# Model results
summary(mod) 
plot(conditional_effects(mod), ask = FALSE) 

# Model fit
prior_summary(mod)
ps <- powerscale_sensitivity(mod) 
unique(ps$sensitivity$diagnosis)

plot(mod)  
pp_check(mod, ndraws = 100) 
pairs(mod)

ppc_stat(y = dat$rel_repro, ###*** change here
         yrep = posterior_predict(mod, ndraws = 1000), stat = "skew")
model_loo <- loo(mod, save_psis = TRUE, cores=4) 
w <- weights(model_loo$psis_object)
ppc_loo_pit_overlay(dat$rel_repro, ###*** change here
                    posterior_predict(mod), lw = w)

# # Conclusion: 




####################################################################################################

# # VACOVA # #
# Family: negative binomial (height & diameter), Beta (rel_repro)
# Fixed effects: dist * altitude
# Random effects: (1|trans.pair)

####################################################################################################




####################################################################################################

# # CARSPP # #
# Family: negative binomial (height & diameter) - no repro model because no repro data
# Fixed effects: dist * altitude
# Random effects: (1|trans.pair)

####################################################################################################
