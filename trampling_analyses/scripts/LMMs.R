# Aims:
# 1. Set up data
# 2. Choose appropriate model structure with AICc
# 3. Fit LMMs to test effects of trampling and elevation on plant responses
# 4. Check model residuals & goodness of fit

# Authors: Nathalie Chardon & Philippa Stone
# Date created: 11 Nov 2022
# Date updated: 29 March 2023 (NC)
# NOTE: this script is outdated, as we are using brms for our analyses (repro.R, bayesian.R)


# # LIBRARIES # # 
# install.packages('tidyverse', lmerTest', 'AICcmodavg', 'MuMIn') 
library(tidyverse)
library(lmerTest) #lmer from lme4 with added p-values
library(AICcmodavg) #calculate AICc
library(DHARMa) #model diagnostics
library(nlme) #allows heteroscedastic models

rm(list=ls()) 


# # INPUT FILES # #
load('trampling_analyses/compiled_data/quad.RData') #gps & transect data matched to quad data (merge_fielddata.R)


# # OUTPUT FILES # #
load('trampling_analyses/compiled_data/quad.RData') #updated with reproductive metric & plant area (LMMs.R)




####################################################################################################

# # DATA # # 

####################################################################################################

# Data
dat <- quad #generic dataframe name
str(dat)


# Create transect pair variable to use as random effect
dat <- dat[order(dat$transect.no), ] #order dataframe by transect number

dat$trans.pair <- NA #set up empty vector
oo <- seq(1, 27, 2) #sequence of all odd transects to create pair numbers
ee <- seq(2, 28, 2) #sequence of all even transects to create pair numbers

for (i in 1:nrow(dat)) { #loop through each data row
  
  for (j in 1:14) { #loop through each pair
    
    if (dat$transect.no[i] == oo[j] | dat$transect.no[i] == ee[j]) {
      
      dat$trans.pair[i] <- paste(oo[j], ee[j], sep = '-')
    }
  }
}

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
hist(dat$repro, breaks = 50)

# Calculate relative reproduction
mm <- dat %>% #create DF with max value per species
  group_by(species) %>% 
  summarise(max = max(repro, na.rm = T)) #NA listed in plots not seeded

dat <- left_join(dat, mm) #combine DFs
dat$rel_repro <- dat$repro/dat$max
summary(dat$rel_repro)
hist(dat$rel_repro, breaks = 50)


# Save updated DF
quad <- dat
save(quad, file = 'trampling_analyses/compiled_data/quad.RData')


# Check for correlation in predictor variables
# - not needed for this analyses because only 1 continuous variable - 


# Calculate variance inflation factor (VIF)
# - not needed for this analyses because only 1 continuous variable - 




####################################################################################################

# # IDENTIFY BEST MODEL STRUCTURE # # 

####################################################################################################

# # Model types

# Fixed Effects (FE): dist + altitude; Random Effects (RE): trans.pair + species
mod1 <- lmer(height_mm ~ dist + altitude + (1|trans.pair) + (1|species), data = dat, REML = F) #REML = F because need log-likelihood for AICc

# FE: dist * altitude; RE: trans.pair + species
mod2 <- lmer(height_mm ~ dist * altitude + (1|trans.pair) + (1|species), data = dat, REML = F)

# FE: dist + altitude; RE: trans.pair (slopes) + species (intercept)
mod3 <- lmer(height_mm ~ dist + altitude + (dist+altitude|species) + (1|trans.pair), data = dat, REML = F) #convergence issues => drop

# FE: dist * altitude; RE: trans.pair (slopes) + species (intercept)
mod4 <- lmer(height_mm ~ dist * altitude + (dist*altitude|species) + (1|trans.pair), data = dat, REML = F) #convergence issues => drop

# FE: dist + altitude; RE: trans.pair (slopes)
mod5 <- lmer(height_mm ~ dist + altitude + (1|trans.pair), data = dat, REML = F)

# FE: dist * altitude; RE: trans.pair (slopes)
mod6 <- lmer(height_mm ~ dist * altitude + (1|trans.pair), data = dat, REML = F)


# # Sort models by AICc
aictab(list(mod1 = mod1, mod2 = mod2, mod5 = mod5, mod6 = mod6)) #mod2 has lowest AICc => best model structure



 
####################################################################################################

# # COMPILE MODELS # # 

####################################################################################################

# Check out this resource for a visual explanation of hierarchical models: 
# http://mfviz.com/hierarchical-models/

# Question: Does disturbance have varying effects on plant height at different elevations?

# Fit model
mod <- lmer(height_mm ~ dist * altitude + (1|trans.pair) + (1|species), data = dat, REML = F)

# Model summary
summary(mod)

# Goodness of fit
ff <- fitted(mod) #predicted values
oo <- dat$height_mm #observed values
cor(ff, oo) #correlation between predicted and observed values



###*** TO DO: copy code above and modify to:
# Test other questions with additional measured response variables





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


######## model testing with DHARMa #######

#DHARMa test for over/underdispersion
simulationOutput<- simulateResiduals(fittedModel = mod)
plot(simulationOutput)
#patterns visible in data (graph on right), so data is not homoscedastic

#DHARMa nonparametric disperision test using standard deviation of fitted vs simulated residuals
testDispersion(mod)

# test for homoscedasticity
testCategorical(simulationOutput, catPred = dat$dist) 
#according to Levene's test the on and off trail groups have equal variance
#but there are significant within group deviations from uniformity (i.e. with on and iff trail populations)



####### Trying a heteroscedastic model ############

#lme4 does not allow heteroscedasticity in models, so need to use nlme
#nlme requires data in a different form, all random effects are nested by default, we have mixture of crossed (species) and random
#reStruct 
dat$dummy <- factor(1)
reStruct = list(dummy=pdBlocked(list(pdIdent(~location - 1),pdIdent( ~ trans.pair - 1),pdIdent(~ transect - 1),pdIdent( ~ species - 1))))

# # Identifying the best model 

#same as model "mod" using above, but in different package (nlme)
model_homoscedastic = nlme::lme(mxdiam_mm ~ dist * altitude, data = dat, random = reStruct,method="ML")

#add heteroscedastic structure for different residual variances by altitude
model_heteroscedastic1 = nlme::lme(mxdiam_mm ~ dist * altitude, data = dat, weights = nlme::varPower(form = ~ altitude), random = reStruct, method="ML")

#add heteroscedastic structure for different residual variances by disturbance
model_heteroscedastic2 = nlme::lme(mxdiam_mm ~ dist * altitude, data = dat, weights = nlme::varIdent(form = ~dist), random = reStruct, method="ML")

#add heteroscedastic structure for different residual variances by both dist and altitude
model_heteroscedastic3 = nlme::lme(mxdiam_mm ~ dist * altitude, data = dat, weights = nlme::varComb(nlme::varIdent(form = ~dist), nlme::varPower(form = ~altitude)), random = reStruct, method="ML")

#model_heteroscedastic1 and model_heteroscedastic3 have the lowest AIC
#model_heteroscedastic1 is less complex so should be used
AIC(model_homoscedastic)
AIC(model_heteroscedastic1)
AIC(model_heteroscedastic2)
AIC(model_heteroscedastic3)

#Final heteroscedastic model
#Use REML as it will estimate standard errors better
model_heteroscedastic1 = nlme::lme(mxdiam_mm ~ dist * altitude, data = dat, weights = nlme::varPower(form = ~ altitude), random = reStruct, method="REML")

plot(model_heteroscedastic1) 

###*** TO DO: 
# Check additional assumptions to check with hierarchical models


