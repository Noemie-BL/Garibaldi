# libraries

library(plyr)
library(readr)
library(tidyverse)
library(stringr)
library(tidyr)
library(chron)
library(timeDate)
library(dplyr)
library(reshape2)
library(scales)
library(lubridate)
library(BiodiversityR)
library(vegan)
library(car)
library(ggplot2)
library(ggthemes)


# install.packages("devtools")
# library(devtools)
# install_github("https://github.com/TundraEcologyLab/tundra_lab_r_package")

#------------------------------
# first change path to where you want the figures output to
setwd("~/GitHub/Garibaldi/Phenology_greenness_traits_analysis")

#---------------------------
# phenology photo data
photo_pheno_data <- read.table("./data/Photo_Phenology_Data.csv", header=TRUE, sep =",", dec = ".")

# extract site, treatment, plot
photo_pheno_data$file <- photo_pheno_data$Site

photo_pheno_data$Plot <- sub("([A-Z]{3,4})_*", "", photo_pheno_data$file, ignore.case = TRUE)
photo_pheno_data$Trmt <- sub("([0-9]{1,2})*", "", photo_pheno_data$Plot, ignore.case = TRUE)
photo_pheno_data$Site <- sub("*_([0-9]{1,2})([W,C]{1})$", "", photo_pheno_data$file, ignore.case = TRUE)

#---------------------------------------
# clean up data

# phenology data dates
# convert #-month to day of year

# check all date formats

# https://www.r-bloggers.com/2013/08/date-formats-in-r/
# https://stackoverflow.com/questions/11609252/r-tick-data-merging-date-and-time-into-a-single-object

# Alex
photo_pheno_data2 <- photo_pheno_data

photo_pheno_data2$Year <- rep("2022", nrow(photo_pheno_data2))

for (i in 3:7){
  photo_pheno_data2[,i] <- paste(photo_pheno_data2$Year, photo_pheno_data2[,i], sep = "-")
  photo_pheno_data2[which(photo_pheno_data2[,i]==2022-NA),i] <- NA
  photo_pheno_data2[,i] <- as.POSIXct(photo_pheno_data2[,i], format="%Y-%d-%m")
  photo_pheno_data2[,i] <- yday(photo_pheno_data2[,i])
}

photo_pheno_data2$Site <- as.factor(photo_pheno_data2$Site)
photo_pheno_data2$Spp <- as.factor(photo_pheno_data2$Spp)
photo_pheno_data2$Trmt <- as.factor(photo_pheno_data2$Trmt)

# make phenology data long

photo_pheno_data2_long <- gather(photo_pheno_data2, Stage, DOY, Elongation.S:Senescence, factor_key=TRUE)

#------------------------------------------
# Linear Mixed Effect Models
# https://www.r-bloggers.com/2017/12/linear-mixed-effect-models-in-r/

library(nlme)
library(lme4)

# phenology photo data - example 
Pheno_LM <- lm(DOY~Site*Trmt*Stage,  data=photo_pheno_data2_long)
summary(Pheno_LM)
par(mfrow = c(2,2))
plot(Pheno_LM)

#-----------------------------------
# Figures

# example of how to save a jpeg image in R
jpeg("./figures/NAME.jpg", width = 856, height = 540)
# sets the bottom, left, top and right margins # default below
par(mar=c(5.1,4.1,4.1,2.1))
#<plot code>
dev.off()

#----------------
# Phenology

jpeg("./figures/Photo_phenology_boxplot.jpg", width = 1000, height = 1000)
# sets the bottom, left, top and right margins
par(mar=c(10,4.1,4.1,2.1))
boxplot(DOY~Trmt+Site+Stage, data=photo_pheno_data2_long, las=2, col="light blue", xlab="Stage/TRTMT", ylab="DOY", main="Phenology")
dev.off()







