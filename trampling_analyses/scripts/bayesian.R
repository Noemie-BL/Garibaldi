# Aims:
# 1. Analyses in Bayesian framework to model disturbance effects on plant height, diameter, repro, and plant cover
# 2. Visualize model fit
# 3. Plot results

# Author: Nathalie Chardon
# Date created: 13 Mar 2023
# Date updated: 5 May 2023 (NC)

# # LIBRARIES # #
library(rjags)
library(R2jags)
library(ggplot2)
library(brms)
library(tidybayes)
library(priorsense) # remotes::install_github("n-kall/priorsense")
library(bayesplot)
library(tidyverse)
library(loo)


rm(list=ls()) 


# # INPUT FILES # #
load('trampling_analyses/compiled_data/quad.RData') ##updated with reproductive metric & plant area (repro.R)


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

# Data transformations
quad <- quad %>% 
  
  #Transform 0s and 1s for Beta distribution repro models
  mutate(rel_repro = if_else(rel_repro == 0, 0.0001, rel_repro)) %>% 
  mutate(rel_repro = if_else(rel_repro == 1, 0.9999, rel_repro)) %>% 
  
  # Integer values for negative binomial distribution height & diam models
  mutate(height_mm = round(height_mm, 0)) %>% 
  mutate(mxdiam_mm = round(mxdiam_mm, 0)) 

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

height_nb <- brms::brm(height_mm ~ dist + altitude + (1|trans.pair), seed = 050523,
                       data = dat, family = negbinomial(link = "log", link_shape = "log"), 
                       # fitting information
                       chains = 3, iter = 5000, warmup = 1000, cores = 4, #for computers with 4 cores
                       file = 'trampling_analyses/outputs/ms_results/phyemp_height_nb.rds', file_refit = 'on_change')
mod <- height_nb #generic model name

# RESULTS FOR MS TABLE
ss <- summary(mod)
ii <- paste(round(ss$fixed[1, 1], 2), '(', round(ss$fixed[1, 3], 2), ',', round(ss$fixed[1, 4], 2), ')')
dd <- paste(round(ss$fixed[2, 1], 2), '(', round(ss$fixed[2, 3], 2), ',', round(ss$fixed[2, 4], 2), ')')
ee <- paste(round(ss$fixed[3, 1], 2), '(', round(ss$fixed[3, 3], 2), ',', round(ss$fixed[3, 4], 2), ')')

brm.res <- data.frame(Species = unique(as.character(dat$species)), Trait = ss$formula[4], N = ss$nobs, 
                               Iterations = ss$iter, Intercept = ii, Disturbance = dd, 
                               Elevation = ee)

brm.tab <- brm.res
brm.tab


## PROBLEM: transitions after warmup exceeded max treedepth
## SOLUTION: increase max_treedepth > 10 BUT this is not recommended and this warning is only an 
## efficiency, not modeling problem (https://mc-stan.org/misc/warnings.html#maximum-treedepth-exceeded)
## => taken care of when removing disturbance*elevation interaction

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


# # Conclusions after trying several models: 
## - major model fit issues (divergent transitions, low BF, large R-hat, low ESS) with 
## species as a random slope
## - for individual species models: data modeled much better, and good model fit! => use this approach
## - no max tree depth issues without interaction term

## Model for DIAMETER

dat <- quad %>% #rename DF
  filter_at(vars(mxdiam_mm, dist, altitude), all_vars(!is.na(.))) %>% #remove NAs in variables used in model
  filter(species == 'phyemp')

diam_nb <- brms::brm(mxdiam_mm ~ dist + altitude + (1|trans.pair), seed = 050523,
                       data = dat, family = negbinomial(link = "log", link_shape = "log"), 
                       # fitting information
                       chains = 3, iter = 8000, warmup = 1000, cores = 4, #for computers with 4 cores
                       file = 'trampling_analyses/outputs/ms_results/phyemp_diam_nb.rds', file_refit = 'on_change')
mod <- diam_nb #generic model name

# RESULTS FOR MS TABLE
ss <- summary(mod)
ii <- paste(round(ss$fixed[1, 1], 2), '(', round(ss$fixed[1, 3], 2), ',', round(ss$fixed[1, 4], 2), ')')
dd <- paste(round(ss$fixed[2, 1], 2), '(', round(ss$fixed[2, 3], 2), ',', round(ss$fixed[2, 4], 2), ')')
ee <- paste(round(ss$fixed[3, 1], 2), '(', round(ss$fixed[3, 3], 2), ',', round(ss$fixed[3, 4], 2), ')')

brm.res <- data.frame(Species = unique(as.character(dat$species)), Trait = ss$formula[4], N = ss$nobs, 
                               Iterations = ss$iter, Intercept = ii, Disturbance = dd, 
                               Elevation = ee)
brm.res

brm.tab <- bind_rows(brm.tab, brm.res)
brm.tab


# CHECK MODEL
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

repro_beta <- brms::brm(rel_repro ~ dist + altitude + (1|trans.pair), seed = 050523,
                     data = dat, family = Beta(link = "logit", link_phi = "log"), 
                     init = '0',
                     
                     # fitting information
                     chains = 3, iter = 5000, warmup = 1000, cores = 4, #for computers with 4 cores
                     file = 'trampling_analyses/outputs/ms_results/phyemp_repro_beta.rds', file_refit = 'on_change')
mod <- repro_beta

# RESULTS FOR MS TABLE
ss <- summary(mod)
ii <- paste(round(ss$fixed[1, 1], 2), '(', round(ss$fixed[1, 3], 2), ',', round(ss$fixed[1, 4], 2), ')')
dd <- paste(round(ss$fixed[2, 1], 2), '(', round(ss$fixed[2, 3], 2), ',', round(ss$fixed[2, 4], 2), ')')
ee <- paste(round(ss$fixed[3, 1], 2), '(', round(ss$fixed[3, 3], 2), ',', round(ss$fixed[3, 4], 2), ')')

brm.res <- data.frame(Species = unique(as.character(dat$species)), Trait = ss$formula[4], N = ss$nobs, 
                               Iterations = ss$iter, Intercept = ii, Disturbance = dd, 
                               Elevation = ee)
brm.res

brm.tab <- bind_rows(brm.tab, brm.res)
brm.tab


# CHECK MODEL

## PROBLEM: Stan model XXX does not contain samples
## DIAGNOSIS: Possible older brms version; Searching for initial values outside of possible interval 
## and should set init = '0' for Beta models (https://discourse.mc-stan.org/t/initialization-error-try-specifying-initial-values-reducing-ranges-of-constrained-values-or-reparameterizing-the-model/4401)
## SOLUTION: Update brms and devtools; Set init = '0' 

# pp_check(repro_beta, ndraws = 100) #slightly better
# pp_check(repro_zibeta, ndraws = 100)
# 
# pairs(repro_beta)
# pairs(repro_zibeta)
# 
# ppc_stat(y = dat$rel_repro, yrep = posterior_predict(repro_beta, ndraws = 1000), stat = "skew")
# ppc_stat(y = dat$rel_repro, yrep = posterior_predict(repro_zibeta, ndraws = 1000), stat = "skew")
# 
# beta_loo <- loo(repro_beta, save_psis = TRUE, cores=4)
# w <- weights(beta_loo$psis_object)
# ppc_loo_pit_overlay(dat$rel_repro, posterior_predict(repro_beta), lw = w)
# 
# zibeta_loo <- loo(repro_zibeta, save_psis = TRUE, cores=4)
# w <- weights(zibeta_loo$psis_object)
# ppc_loo_pit_overlay(dat$rel_repro, posterior_predict(repro_zibeta), lw = w)
# 
# loo_compare(beta_loo, zibeta_loo)

# # Conclusion: pp_check, skew, and PIT are almost identical and poor for both models, better elpd for Beta
# # => use Beta because better elpd & fits faster

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

####### PHYEMP PLOTS ########

#height
(PhyempHeightModPlot <- dat %>%
   group_by(dist) %>%
   add_predicted_draws(height_nb, allow_new_levels = TRUE) %>%
   ggplot(aes(x = altitude, y = height_mm, color = dist, fill = dist)) +
   stat_lineribbon(aes(y = .prediction), .width = c(.95), alpha = 0.33) +
   geom_point(data = dat) +
   ylab("Plant height (mm)\n") +
   xlab("\nElevation (m)") +
   scale_color_manual(values = c("#999999", "#E69F00"), labels = c("Undisturbed", "Disturbed"), name = "Disturbance") +
   scale_fill_manual(values = c("#999999", "#E69F00"), labels = c("Undisturbed", "Disturbed"), name = "Disturbance") +
   theme(legend.title = element_blank()))

ggsave(PhyempHeightModPlot, file = 'trampling_analyses/outputs/ms_figs/PhyempHeightModPlot.pdf')

#diameter
(PhyempDiamModPlot <- dat %>%
    group_by(dist) %>%
    add_predicted_draws(diam_nb, allow_new_levels = TRUE) %>%
    ggplot(aes(x = altitude, y = mxdiam_mm, color = dist, fill = dist)) +
    stat_lineribbon(aes(y = .prediction), .width = c(.95), alpha = 0.33) +
    geom_point(data = dat) +
    ylab("Plant diameter (mm)\n") +
    xlab("\nElevation (m)") +
    scale_color_manual(values = c("#999999", "#E69F00"), labels = c("Undisturbed", "Disturbed"), name = "Disturbance") +
    scale_fill_manual(values = c("#999999", "#E69F00"), labels = c("Undisturbed", "Disturbed"), name = "Disturbance") +
    theme(legend.title = element_blank()))

ggsave(PhyempDiamModPlot, file = 'trampling_analyses/outputs/ms_figs/PhyempDiamModPlot.pdf')

#reproductive output
(PhyempReproModPlot <- dat %>%
    group_by(dist) %>%
    add_predicted_draws(repro_beta, allow_new_levels = TRUE) %>%
    ggplot(aes(x = altitude, y = mxdiam_mm, color = dist, fill = dist)) +
    stat_lineribbon(aes(y = .prediction, fill = dist), .width = c(.95), alpha = 0.33) + #### problem: probability distributions not showing up
    geom_point(data = dat) +
    labs(x = expression(paste("Elevation (m)")),
         y = expression(paste("Reproduction (counts/", cm^2, ")"))) +
    scale_color_manual(values = c("#999999", "#E69F00"), labels = c("Undisturbed", "Disturbed"), name = "Disturbance") +
    scale_fill_manual(values = c("#999999", "#E69F00"), labels = c("Undisturbed", "Disturbed"), name = "Disturbance") +
    theme(legend.title = element_blank()))

ggsave(PhyempReproModPlot, file = 'trampling_analyses/outputs/ms_figs/PhyempReproModPlot.pdf')




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

height_nb <- brms::brm(height_mm ~ dist + altitude + (1|trans.pair), data = dat, seed = 050523,
                       family = negbinomial(link = "log", link_shape = "log"), 
                       chains = 3, iter = 5000, warmup = 1000, cores = 4, 
                       file = 'trampling_analyses/outputs/ms_results/height_nb_casmer.rds', ###*** change here
                       file_refit = 'on_change')
mod <- height_nb #generic model name


# RESULTS FOR MS TABLE
ss <- summary(mod)
ii <- paste(round(ss$fixed[1, 1], 2), '(', round(ss$fixed[1, 3], 2), ',', round(ss$fixed[1, 4], 2), ')')
dd <- paste(round(ss$fixed[2, 1], 2), '(', round(ss$fixed[2, 3], 2), ',', round(ss$fixed[2, 4], 2), ')')
ee <- paste(round(ss$fixed[3, 1], 2), '(', round(ss$fixed[3, 3], 2), ',', round(ss$fixed[3, 4], 2), ')')

brm.res <- data.frame(Species = unique(as.character(dat$species)), Trait = ss$formula[4], N = ss$nobs, 
                               Iterations = ss$iter, Intercept = ii, Disturbance = dd, 
                               Elevation = ee)
brm.res

brm.tab <- bind_rows(brm.tab, brm.res)
brm.tab


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

# # Conclusion: Very good fit


## Model for DIAMETER

dat <- quad %>% #rename DF
  filter_at(vars(mxdiam_mm, dist, altitude), all_vars(!is.na(.))) %>% 
  filter(species == 'casmer') ###*** change here

diam_nb <- brms::brm(mxdiam_mm ~ dist + altitude + (1|trans.pair), data = dat, seed = 050523,
                     family = negbinomial(link = "log", link_shape = "log"), 
                     chains = 3, iter = 5000, warmup = 1000, cores = 4, 
                     file = 'trampling_analyses/outputs/ms_results/diam_nb_casmer.rds', ###*** change here
                     file_refit = 'on_change')
mod <- diam_nb #generic model name

# RESULTS FOR MS TABLE
ss <- summary(mod)
ii <- paste(round(ss$fixed[1, 1], 2), '(', round(ss$fixed[1, 3], 2), ',', round(ss$fixed[1, 4], 2), ')')
dd <- paste(round(ss$fixed[2, 1], 2), '(', round(ss$fixed[2, 3], 2), ',', round(ss$fixed[2, 4], 2), ')')
ee <- paste(round(ss$fixed[3, 1], 2), '(', round(ss$fixed[3, 3], 2), ',', round(ss$fixed[3, 4], 2), ')')

brm.res <- data.frame(Species = unique(as.character(dat$species)), Trait = ss$formula[4], N = ss$nobs, 
                               Iterations = ss$iter, Intercept = ii, Disturbance = dd, 
                               Elevation = ee)
brm.res

brm.tab <- bind_rows(brm.tab, brm.res)
brm.tab


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


## Model for REPRO

dat <- quad %>% #rename DF
  filter_at(vars(rel_repro, dist, altitude), all_vars(!is.na(.))) %>% 
  filter(species == 'casmer') ###*** change here

repro_beta <- brms::brm(rel_repro ~ dist + altitude + (1|trans.pair), data = dat, seed = 050523,
                        family = Beta(link = "logit", link_phi = "log"), init = '0',
                        chains = 3, iter = 5000, warmup = 1000, cores = 4, 
                        file = 'trampling_analyses/outputs/ms_results/repro_beta_casmer.rds', ###*** change here
                        file_refit = 'on_change')
mod <- repro_beta

# RESULTS FOR MS TABLE
ss <- summary(mod)
ii <- paste(round(ss$fixed[1, 1], 2), '(', round(ss$fixed[1, 3], 2), ',', round(ss$fixed[1, 4], 2), ')')
dd <- paste(round(ss$fixed[2, 1], 2), '(', round(ss$fixed[2, 3], 2), ',', round(ss$fixed[2, 4], 2), ')')
ee <- paste(round(ss$fixed[3, 1], 2), '(', round(ss$fixed[3, 3], 2), ',', round(ss$fixed[3, 4], 2), ')')

brm.res <- data.frame(Species = unique(as.character(dat$species)), Trait = ss$formula[4], N = ss$nobs, 
                               Iterations = ss$iter, Intercept = ii, Disturbance = dd, 
                               Elevation = ee)
brm.res

brm.tab <- bind_rows(brm.tab, brm.res)
brm.tab


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

# # Conclusion: skew not well modeled, 1 observation with pareto K > 0.7


####### CASMER PLOTS ########

#height
(CasmerHeightModPlot <- dat %>%
   group_by(dist) %>%
   add_predicted_draws(height_nb, allow_new_levels = TRUE) %>%
   ggplot(aes(x = altitude, y = height_mm, color = dist, fill = dist)) +
   stat_lineribbon(aes(y = .prediction), .width = c(.95), alpha = 0.33) +
   geom_point(data = dat) +
   ylab("Plant height (mm)\n") +
   xlab("\nElevation (m)") +
   scale_color_manual(values = c("#999999", "#E69F00"), labels = c("Undisturbed", "Disturbed"), name = "Disturbance") +
   scale_fill_manual(values = c("#999999", "#E69F00"), labels = c("Undisturbed", "Disturbed"), name = "Disturbance") +
   theme(legend.title = element_blank()))

ggsave(CasmerHeightModPlot, file = 'trampling_analyses/outputs/ms_figs/CasmerHeightModPlot.pdf')

#diameter
(CasmerDiamModPlot <- dat %>%
    group_by(dist) %>%
    add_predicted_draws(diam_nb, allow_new_levels = TRUE) %>%
    ggplot(aes(x = altitude, y = mxdiam_mm, color = dist, fill = dist)) +
    stat_lineribbon(aes(y = .prediction), .width = c(.95), alpha = 0.33) +
    geom_point(data = dat) +
    ylab("Plant diameter (mm)\n") +
    xlab("\nElevation (m)") +
    scale_color_manual(values = c("#999999", "#E69F00"), labels = c("Undisturbed", "Disturbed"), name = "Disturbance") +
    scale_fill_manual(values = c("#999999", "#E69F00"), labels = c("Undisturbed", "Disturbed"), name = "Disturbance") +
    theme(legend.title = element_blank()))

ggsave(CasmerDiamModPlot, file = 'trampling_analyses/outputs/ms_figs/CasmerDiamModPlot.pdf')

#reproductive output
(CasmerReproModPlot <- dat %>%
    group_by(dist) %>%
    add_predicted_draws(repro_beta, allow_new_levels = TRUE) %>%
    ggplot(aes(x = altitude, y = mxdiam_mm, color = dist, fill = dist)) +
    stat_lineribbon(aes(y = .prediction, fill = dist), .width = c(.95), alpha = 0.33) + #### problem: probability distributions not showing up
    geom_point(data = dat) +
    labs(x = expression(paste("Elevation (m)")),
         y = expression(paste("Reproduction (counts/", cm^2, ")"))) +
    scale_color_manual(values = c("#999999", "#E69F00"), labels = c("Undisturbed", "Disturbed"), name = "Disturbance") +
    scale_fill_manual(values = c("#999999", "#E69F00"), labels = c("Undisturbed", "Disturbed"), name = "Disturbance") +
    theme(legend.title = element_blank()))

ggsave(CasmerReproModPlot, file = 'trampling_analyses/outputs/ms_figs/CasmerReproModPlot.pdf')




####################################################################################################

# # VACOVA # #
# Family: negative binomial (height & diameter), Beta (rel_repro)
# Fixed effects: dist * altitude
# Random effects: (1|trans.pair)

####################################################################################################

# # Model for HEIGHT

dat <- quad %>% #rename DF
  filter_at(vars(height_mm, dist, altitude), all_vars(!is.na(.))) %>% 
  filter(species == 'vacova') ###*** change here

height_nb <- brms::brm(height_mm ~ dist + altitude + (1|trans.pair), data = dat, seed = 050523,
                       family = negbinomial(link = "log", link_shape = "log"), 
                       chains = 3, iter = 5000, warmup = 1000, cores = 4, 
                       file = 'trampling_analyses/outputs/ms_results/height_nb_vacova.rds', ###*** change here
                       file_refit = 'on_change')
mod <- height_nb #generic model name

# RESULTS FOR MS TABLE
ss <- summary(mod)
ii <- paste(round(ss$fixed[1, 1], 2), '(', round(ss$fixed[1, 3], 2), ',', round(ss$fixed[1, 4], 2), ')')
dd <- paste(round(ss$fixed[2, 1], 2), '(', round(ss$fixed[2, 3], 2), ',', round(ss$fixed[2, 4], 2), ')')
ee <- paste(round(ss$fixed[3, 1], 2), '(', round(ss$fixed[3, 3], 2), ',', round(ss$fixed[3, 4], 2), ')')

brm.res <- data.frame(Species = unique(as.character(dat$species)), Trait = ss$formula[4], N = ss$nobs, 
                      Iterations = ss$iter, Intercept = ii, Disturbance = dd, 
                      Elevation = ee)
brm.res

brm.tab <- bind_rows(brm.tab, brm.res)
brm.tab


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

ppc_stat(y = dat$height_mm, 
         yrep = posterior_predict(mod, ndraws = 1000), stat = "skew")
model_loo <- loo(mod, save_psis = TRUE, cores=4) 
w <- weights(model_loo$psis_object)
ppc_loo_pit_overlay(dat$height_mm, 
                    posterior_predict(mod), lw = w)

# # Conclusion: Skew not modeled well and 1 obs w pareto_k > 0.7



## Model for DIAMETER

dat <- quad %>% #rename DF
  filter_at(vars(mxdiam_mm, dist, altitude), all_vars(!is.na(.))) %>% 
  filter(species == 'vacova') ###*** change here

diam_nb <- brms::brm(mxdiam_mm ~ dist + altitude + (1|trans.pair), data = dat, seed = 050523,
                     family = negbinomial(link = "log", link_shape = "log"), 
                     chains = 3, iter = 8000, warmup = 1000, cores = 4, 
                     file = 'trampling_analyses/outputs/April2023/diam_nb_vacova.rds', ###*** change here
                     file_refit = 'on_change')
mod <- diam_nb #generic model name

# RESULTS FOR MS TABLE
ss <- summary(mod)
ii <- paste(round(ss$fixed[1, 1], 2), '(', round(ss$fixed[1, 3], 2), ',', round(ss$fixed[1, 4], 2), ')')
dd <- paste(round(ss$fixed[2, 1], 2), '(', round(ss$fixed[2, 3], 2), ',', round(ss$fixed[2, 4], 2), ')')
ee <- paste(round(ss$fixed[3, 1], 2), '(', round(ss$fixed[3, 3], 2), ',', round(ss$fixed[3, 4], 2), ')')

brm.res <- data.frame(Species = unique(as.character(dat$species)), Trait = ss$formula[4], N = ss$nobs, 
                      Iterations = ss$iter, Intercept = ii, Disturbance = dd, 
                      Elevation = ee)
brm.res

brm.tab <- bind_rows(brm.tab, brm.res)
brm.tab


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

ppc_stat(y = dat$mxdiam_mm, 
         yrep = posterior_predict(mod, ndraws = 1000), stat = "skew")
model_loo <- loo(mod, save_psis = TRUE, cores=4) 
w <- weights(model_loo$psis_object)
ppc_loo_pit_overlay(dat$mxdiam_mm, 
                    posterior_predict(mod), lw = w)

# # Conclusion: Increased iterations to increase ESS; model fit well except for skew


## Model for REPRO

dat <- quad %>% #rename DF
  filter_at(vars(rel_repro, dist, altitude), all_vars(!is.na(.))) %>% 
  filter(species == 'vacova') ###*** change here

repro_beta <- brms::brm(rel_repro ~ dist + altitude + (1|trans.pair), data = dat, seed = 050523,
                        family = Beta(link = "logit", link_phi = "log"), init = '0',
                        chains = 3, iter = 5000, warmup = 1000, cores = 4, 
                        file = 'trampling_analyses/outputs/ms_results/repro_beta_vacova.rds', ###*** change here
                        file_refit = 'on_change')
mod <- repro_beta

# RESULTS FOR MS TABLE
ss <- summary(mod)
ii <- paste(round(ss$fixed[1, 1], 2), '(', round(ss$fixed[1, 3], 2), ',', round(ss$fixed[1, 4], 2), ')')
dd <- paste(round(ss$fixed[2, 1], 2), '(', round(ss$fixed[2, 3], 2), ',', round(ss$fixed[2, 4], 2), ')')
ee <- paste(round(ss$fixed[3, 1], 2), '(', round(ss$fixed[3, 3], 2), ',', round(ss$fixed[3, 4], 2), ')')

brm.res <- data.frame(Species = unique(as.character(dat$species)), Trait = ss$formula[4], N = ss$nobs, 
                      Iterations = ss$iter, Intercept = ii, Disturbance = dd, 
                      Elevation = ee)
brm.res

brm.tab <- bind_rows(brm.tab, brm.res)
brm.tab


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

ppc_stat(y = dat$rel_repro, 
         yrep = posterior_predict(mod, ndraws = 1000), stat = "skew")
model_loo <- loo(mod, save_psis = TRUE, cores=4) 
w <- weights(model_loo$psis_object)
ppc_loo_pit_overlay(dat$rel_repro, 
                    posterior_predict(mod), lw = w)

# # Conclusion: good model fit except for skew and dispersion


####### VACOVA PLOTS ########

#height
(VacovaHeightModPlot <- dat %>%
   group_by(dist) %>%
   add_predicted_draws(height_nb, allow_new_levels = TRUE) %>%
   ggplot(aes(x = altitude, y = height_mm, color = dist, fill = dist)) +
   stat_lineribbon(aes(y = .prediction), .width = c(.95), alpha = 0.33) +
   geom_point(data = dat) +
   ylab("Plant height (mm)\n") +
   xlab("\nElevation (m)") +
   scale_color_manual(values = c("#999999", "#E69F00"), labels = c("Undisturbed", "Disturbed"), name = "Disturbance") +
   scale_fill_manual(values = c("#999999", "#E69F00"), labels = c("Undisturbed", "Disturbed"), name = "Disturbance") +
   theme(legend.title = element_blank()))

ggsave(VacovaHeightModPlot, file = 'trampling_analyses/outputs/ms_figs/VacovaHeightModPlot.pdf')

#diameter
(VacovaDiamModPlot <- dat %>%
    group_by(dist) %>%
    add_predicted_draws(diam_nb, allow_new_levels = TRUE) %>%
    ggplot(aes(x = altitude, y = mxdiam_mm, color = dist, fill = dist)) +
    stat_lineribbon(aes(y = .prediction), .width = c(.95), alpha = 0.33) +
    geom_point(data = dat) +
    ylab("Plant diameter (mm)\n") +
    xlab("\nElevation (m)") +
    scale_color_manual(values = c("#999999", "#E69F00"), labels = c("Undisturbed", "Disturbed"), name = "Disturbance") +
    scale_fill_manual(values = c("#999999", "#E69F00"), labels = c("Undisturbed", "Disturbed"), name = "Disturbance") +
    theme(legend.title = element_blank()))

ggsave(VacovaDiamModPlot, file = 'trampling_analyses/outputs/ms_figs/VacovaDiamModPlot.pdf')

#reproductive output
(VacovaReproModPlot <- dat %>%
    group_by(dist) %>%
    add_predicted_draws(repro_beta, allow_new_levels = TRUE) %>%
    ggplot(aes(x = altitude, y = mxdiam_mm, color = dist, fill = dist)) +
    stat_lineribbon(aes(y = .prediction, fill = dist), .width = c(.95), alpha = 0.33) + #### problem: probability distributions not showing up
    geom_point(data = dat) +
    labs(x = expression(paste("Elevation (m)")),
         y = expression(paste("Reproduction (counts/", cm^2, ")"))) +
    scale_color_manual(values = c("#999999", "#E69F00"), labels = c("Undisturbed", "Disturbed"), name = "Disturbance") +
    scale_fill_manual(values = c("#999999", "#E69F00"), labels = c("Undisturbed", "Disturbed"), name = "Disturbance") +
    theme(legend.title = element_blank()))

ggsave(VacovaReproModPlot, file = 'trampling_analyses/outputs/ms_figs/VacovaReproModPlot.pdf')




####################################################################################################

# # CARSPP # #
# Family: negative binomial (height & diameter) - no repro model because no repro data
# Fixed effects: dist * altitude
# Random effects: (1|trans.pair)

####################################################################################################

# # Model for HEIGHT

dat <- quad %>% #rename DF
  filter_at(vars(height_mm, dist, altitude), all_vars(!is.na(.))) %>% 
  filter(species == 'carspp') ###*** change here

height_nb <- brms::brm(height_mm ~ dist + altitude + (1|trans.pair), data = dat, seed = 050523,
                       family = negbinomial(link = "log", link_shape = "log"), 
                       chains = 3, iter = 5000, warmup = 1000, cores = 4, 
                       file = 'trampling_analyses/outputs/ms_results/height_nb_carspp.rds', ###*** change here
                       file_refit = 'on_change')
mod <- height_nb #generic model name

# RESULTS FOR MS TABLE
ss <- summary(mod)
ii <- paste(round(ss$fixed[1, 1], 2), '(', round(ss$fixed[1, 3], 2), ',', round(ss$fixed[1, 4], 2), ')')
dd <- paste(round(ss$fixed[2, 1], 2), '(', round(ss$fixed[2, 3], 2), ',', round(ss$fixed[2, 4], 2), ')')
ee <- paste(round(ss$fixed[3, 1], 2), '(', round(ss$fixed[3, 3], 2), ',', round(ss$fixed[3, 4], 2), ')')

brm.res <- data.frame(Species = unique(as.character(dat$species)), Trait = ss$formula[4], N = ss$nobs, 
                      Iterations = ss$iter, Intercept = ii, Disturbance = dd, 
                      Elevation = ee)
brm.res

brm.tab <- bind_rows(brm.tab, brm.res)
brm.tab


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

ppc_stat(y = dat$height_mm, 
         yrep = posterior_predict(mod, ndraws = 1000), stat = "skew")
model_loo <- loo(mod, save_psis = TRUE, cores=4) 
w <- weights(model_loo$psis_object)
ppc_loo_pit_overlay(dat$height_mm, 
                    posterior_predict(mod), lw = w)

# # Conclusion: good fit, 1 obs > 0.7 pareto k, skew good, dispersion moderate


## Model for DIAMETER

dat <- quad %>% #rename DF
  filter_at(vars(mxdiam_mm, dist, altitude), all_vars(!is.na(.))) %>% 
  filter(species == 'carspp') ###*** change here

diam_nb <- brms::brm(mxdiam_mm ~ dist + altitude + (1|trans.pair), data = dat, seed = 050523,
                     family = negbinomial(link = "log", link_shape = "log"), 
                     chains = 3, iter = 8000, warmup = 1000, cores = 4, 
                     file = 'trampling_analyses/outputs/ms_results/diam_nb_carspp.rds', ###*** change here
                     file_refit = 'on_change')
mod <- diam_nb #generic model name

## PROBLEM: divergent transitions after warmup
## SOLUTION: ncreasing adapt_delta above 0.8 may help. 
## See http://mc-stan.org/misc/warnings.html#divergent-transitions-after-warmup 


# RESULTS FOR MS TABLE
ss <- summary(mod)
ii <- paste(round(ss$fixed[1, 1], 2), '(', round(ss$fixed[1, 3], 2), ',', round(ss$fixed[1, 4], 2), ')')
dd <- paste(round(ss$fixed[2, 1], 2), '(', round(ss$fixed[2, 3], 2), ',', round(ss$fixed[2, 4], 2), ')')
ee <- paste(round(ss$fixed[3, 1], 2), '(', round(ss$fixed[3, 3], 2), ',', round(ss$fixed[3, 4], 2), ')')

brm.res <- data.frame(Species = unique(as.character(dat$species)), Trait = ss$formula[4], N = ss$nobs, 
                      Iterations = ss$iter, Intercept = ii, Disturbance = dd, 
                      Elevation = ee)
brm.res

brm.tab <- bind_rows(brm.tab, brm.res)
brm.tab


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

ppc_stat(y = dat$mxdiam_mm, 
         yrep = posterior_predict(mod, ndraws = 1000), stat = "skew")
model_loo <- loo(mod, save_psis = TRUE, cores=4) 
w <- weights(model_loo$psis_object)
ppc_loo_pit_overlay(dat$mxdiam_mm, 
                    posterior_predict(mod), lw = w)

# # Conclusion: weak likelihood in one prior, skew moderate, dispersion good


## SAVE RESULTS TABLE
save(brm.tab, file = 'trampling_analyses/outputs/ms_results/brm_table.RData')
write.csv(brm.tab, file = 'trampling_analyses/outputs/ms_results/brm_table.csv')


####### Carex PLOTS ########

#height
(CarexHeightModPlot <- dat %>%
   group_by(dist) %>%
   add_predicted_draws(height_nb, allow_new_levels = TRUE) %>%
   ggplot(aes(x = altitude, y = height_mm, color = dist, fill = dist)) +
   stat_lineribbon(aes(y = .prediction), .width = c(.95), alpha = 0.33) +
   geom_point(data = dat) +
   ylab("Plant height (mm)\n") +
   xlab("\nElevation (m)") +
   scale_color_manual(values = c("#999999", "#E69F00"), labels = c("Undisturbed", "Disturbed"), name = "Disturbance") +
   scale_fill_manual(values = c("#999999", "#E69F00"), labels = c("Undisturbed", "Disturbed"), name = "Disturbance") +
   theme(legend.title = element_blank()))

ggsave(CarexHeightModPlot, file = 'trampling_analyses/outputs/ms_figs/CarexHeightModPlot.pdf')

#diameter
(CarexDiamModPlot <- dat %>%
    group_by(dist) %>%
    add_predicted_draws(diam_nb, allow_new_levels = TRUE) %>%
    ggplot(aes(x = altitude, y = mxdiam_mm, color = dist, fill = dist)) +
    stat_lineribbon(aes(y = .prediction), .width = c(.95), alpha = 0.33) +
    geom_point(data = dat) +
    ylab("Plant diameter (mm)\n") +
    xlab("\nElevation (m)") +
    scale_color_manual(values = c("#999999", "#E69F00"), labels = c("Undisturbed", "Disturbed"), name = "Disturbance") +
    scale_fill_manual(values = c("#999999", "#E69F00"), labels = c("Undisturbed", "Disturbed"), name = "Disturbance") +
    theme(legend.title = element_blank()))

ggsave(CarexHeightModPlot, file = 'trampling_analyses/outputs/ms_figs/CarexDiamModPlot.pdf')
