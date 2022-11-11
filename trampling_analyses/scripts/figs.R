### *** NEED TO RUN MERGE_FIELDDATA.R BEFORE RUNNING ANYTHING HERE

# Aims:
# 1. Visualize data

# Author: Nathalie Chardon
# Date created: 11 Nov 2022
# Date updated: 11 Nov 2022

# # LIBRARIES # #
library(ggplot2)


rm(list=ls()) 


# # WORKING DIRECTORIES # #
comp_dat <- '~/Desktop/Code/Garibaldi/trampling_analyses/compiled_data/' #WD for NC
figs <- '~/Desktop/Code/Garibaldi/trampling_analyses/figures/BC_PARF/' #WD for NC


# # INPUT FILES # #
setwd(comp_dat)
load('quad.R') #gps & transect data matched to quad data (merge_fielddata.R)


# # OUTPUT FILES # #




####################################################################################################

# # DATA PLOTS # # 

####################################################################################################

# # DATA # #

dat <- quad #generic dataframe name


# # PLOT THEME # #

setwd(figs) #location to save figures

mytheme <-   theme_classic() +
  theme(axis.text.x = element_text(colour = 'black', size = 25), #x axis text size
        axis.text.y = element_text(colour = 'black', size = 25), #y axis text size
        axis.title.x = element_text(size = 28), #x axis label size
        axis.title.y = element_text(size = 28), #x axis label size
        plot.title = element_text(size = 30, #title size
                                  hjust = 0.5), #align title to center
        legend.title = element_text(size = 24), legend.text = element_text(size = 22)) 


# # PLOTS

# height ~ elev by trampling & species
### *** need to double check variable names

ggplot(dat, aes(elev, height, color = trail)) +
  geom_point(size = 5, alpha = 0.5) +
  ylab('Plant Height [mm]') + xlab('Elevation [m]') + labs(title = '') + 
  geom_smooth() +
  facet_wrap(~species, scales = "free_y") #separate panels into species




####################################################################################################

# # INTERACTION PLOTS # # 

####################################################################################################

# # EXAMPLE FROM ZUUR ET AL # #

#Figure 11: coplot to visualize potential presence of interactions
Sparrows <- read.table(file = "SparrowsElphick.txt", header = TRUE)

#Take the data from species 1, Sex = 0 and Wing length >= 65
I1 <- Sparrows$SpeciesCode == 1 & Sparrows$Sex != "0" & Sparrows$wingcrd < 65
Wing1<- Sparrows$wingcrd[I1]
Wei1 <- Sparrows$wt[I1]
Mon1 <- factor(Sparrows$Month[I1])
Sex1<- factor(Sparrows$Sex[I1])


#Define Month and Sex as categorical variables
fMonth1 <- factor(Mon1,levels=c(5,6,7,8,9),
                  labels=c("May","Jun","Jul","Aug","Sep"))
fSex1   <- factor(Sex1, levels=c(4,5),labels=c("Male","Female"))

M1 <- lm(Wei1 ~ Wing1*fMonth1*fSex1)
summary(M1)
anova(M1)


#Make the coplot: if all bivariate regression lines are parallel, then there are probably no 
#significant interactions

coplot(Wei1 ~ Wing1 | fMonth1 * fSex1, ylab = "Weight (g)",
       xlab = "Wing length (mm)",
       panel = function(x, y, ...) {
         tmp <- lm(y ~ x, na.action = na.omit)
         abline(tmp)
         points(x, y) })
