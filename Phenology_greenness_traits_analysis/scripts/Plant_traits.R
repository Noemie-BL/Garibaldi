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

#----------------------------------
# import data
# https://www.geeksforgeeks.org/merge-multiple-csv-files-using-r/?ref=rp

# Mead flower counts
read_csv_withnames <- function(x){
  y <- read.table(x, header=TRUE, sep =",", dec = ".")
  y$filename <- rep(x, nrow(y))
  y$file <- sub(".csv$", "", y$filename, ignore.case = TRUE)
  y$file <- sub("./data/Sentinel_MEAD/Flower_Counts/", "", y$file, ignore.case = TRUE)
  y$Plot <- sub("(MEAD)_*", "", y$file, ignore.case = TRUE)
  y$Trmt <- sub("(MEAD)_([0-9]{1,2})*", "", y$file, ignore.case = TRUE)
  y$Site <- sub("*_([0-9]{1,2})([W,C]{1})", "", y$file, ignore.case = TRUE)
  return(y)
}

MEAD_FLWR_count_data <- list.files(path = "./data/Sentinel_MEAD/Flower_Counts", pattern = "*.csv", full.names = TRUE) %>%
  lapply(read_csv_withnames) %>%
  bind_rows

#----------------------------
# Mead traits
read_csv_withnames <- function(x){
  y <- read.csv(x, header=TRUE, sep =",", colClasses = "character")
  y$filename <- rep(x, nrow(y))
  y$file <- sub(".csv$", "", y$filename, ignore.case = TRUE)
  y$file <- sub("./data/Sentinel_MEAD/Traits/", "", y$file, ignore.case = TRUE)
  y$Plot <- sub("(MEAD)_*", "", y$file, ignore.case = TRUE)
  y$Trmt <- sub("(MEAD)_([0-9]{1,2})*", "", y$file, ignore.case = TRUE)
  y$Site <- sub("*_([0-9]{1,2})([W,C]{1})", "", y$file, ignore.case = TRUE)
  return(y)
}

MEAD_trait_data <- list.files(path = "./data/Sentinel_MEAD/Traits", pattern = "*.csv", full.names = TRUE) %>%
  lapply(read_csv_withnames) %>%
  bind_rows

#---------------------------------
# Cassiope flower count data

read_csv_withnames <- function(x){
  y <- read.table(x, header=TRUE, sep =",", dec = ".")
  y$filename <- rep(x, nrow(y))
  y$file <- sub("_FC.csv$", "", y$filename, ignore.case = TRUE)
  y$file <- sub("./data/Sphinx_CASS/Flower_Counts/", "", y$file, ignore.case = TRUE)
  y$Plot <- sub("(CASS)_*", "", y$file, ignore.case = TRUE)
  y$Trmt <- sub("(CASS)_([0-9]{1,2})*", "", y$file, ignore.case = TRUE)
  y$Site <- sub("*_([0-9]{1,2})([W,C]{1})", "", y$file, ignore.case = TRUE)
  return(y)
}

CASS_FLWR_count_data <- list.files(path = "./data/Sphinx_CASS/Flower_Counts/", pattern = "*.csv", full.names = TRUE) %>%
  lapply(read_csv_withnames) %>%
  bind_rows

#-------------------------------
# Cass trait data

read_csv_withnames <- function(x){
  y <- read.csv(x, header=TRUE, sep =",", colClasses = "character")
  y$filename <- rep(x, nrow(y))
  y$file <- sub(".csv$", "", y$filename, ignore.case = TRUE)
  y$file <- sub("./data/Sphinx_CASS/Traits/", "", y$file, ignore.case = TRUE)
  y$Plot <- sub("(CASS)_*", "", y$file, ignore.case = TRUE)
  y$Trmt <- sub("(CASS)_([0-9]{1,2})*", "", y$file, ignore.case = TRUE)
  y$Site <- sub("*_([0-9]{1,2})([W,C]{1})", "", y$file, ignore.case = TRUE)
  return(y)
}

CASS_trait_data <- list.files(path = "./data/Sphinx_CASS/Traits/", pattern = "*.csv", full.names = TRUE) %>%
  lapply(read_csv_withnames) %>%
  bind_rows

#---------------------------------------
# clean up data

#-------------------------
# trait data

# join MEAD and CASS data

trait_data <- rbind(MEAD_trait_data, CASS_trait_data)

#make veg and flow columns long
flwr_trait_data <- trait_data[,c(1:2,8:16)]
veg_trait_data <- trait_data[,c(1:7,13:16)]

flwr_trait_data_long <- gather(flwr_trait_data, Flower, Flwr_height, FLOW_1:FLOW_5, factor_key=TRUE)
veg_trait_data_long <- gather(veg_trait_data, Leaf, Leaf_height, VEG_1:VEG_5, factor_key=TRUE)


# convert n/a to NA
flwr_trait_data_long$Flwr_height[which(flwr_trait_data_long$Flwr_height=="n/a")] <- NA

veg_trait_data_long$Leaf_height[which(veg_trait_data_long$Leaf_height=="n/a")] <- NA


# convert Height columns to numeric
veg_trait_data_long$Leaf_height <- as.numeric(veg_trait_data_long$Leaf_height)
flwr_trait_data_long$Flwr_height <- as.numeric(flwr_trait_data_long$Flwr_height)


# convert TRMT, site and Spp to factors
veg_trait_data_long$Trmt <- as.factor(veg_trait_data_long$Trmt)
flwr_trait_data_long$Trmt <- as.factor(flwr_trait_data_long$Trmt)

veg_trait_data_long$Site <- as.factor(veg_trait_data_long$Site)
flwr_trait_data_long$Site <- as.factor(flwr_trait_data_long$Site)

veg_trait_data_long$plant <- as.factor(veg_trait_data_long$plant)
flwr_trait_data_long$plant <- as.factor(flwr_trait_data_long$plant)

#--------------------------------------
# flower count data

# Cassiope

CASS_FLWR_count_data$Site <- as.factor(CASS_FLWR_count_data$Site)
CASS_FLWR_count_data$Plant.Flower.count <- as.factor(CASS_FLWR_count_data$Plant.Flower.count)
CASS_FLWR_count_data$Trmt <- as.factor(CASS_FLWR_count_data$Trmt)

CASS_FLWR_count_data$X <- as.numeric(CASS_FLWR_count_data$X)

# Meadow

MEAD_FLWR_count_data$Site <- as.factor(MEAD_FLWR_count_data$Site)
MEAD_FLWR_count_data$Plant <- as.factor(MEAD_FLWR_count_data$Plant)
MEAD_FLWR_count_data$Trmt <- as.factor(MEAD_FLWR_count_data$Trmt)

MEAD_FLWR_count_data$Inflouresc_Count <- as.numeric(MEAD_FLWR_count_data$Inflouresc_Count)


#------------------------------------------
# Linear Mixed Effect Models
# https://www.r-bloggers.com/2017/12/linear-mixed-effect-models-in-r/

library(nlme)
library(lme4)

# trait data - example 

Veg_traits_LM <- lm(Leaf_height~Trmt*Site*plant, data=veg_trait_data_long)
summary(Veg_traits_LM)
par(mfrow = c(2,2))
plot(Veg_traits_LM)

Flower_traits_LM <- lm(Flwr_height~Trmt*Site*plant, data=flwr_trait_data_long)
summary(Flower_traits_LM)
par(mfrow = c(2,2))
plot(Flower_traits_LM)

#-----------------------------------
# Figures

# example of how to save a jpeg image in R
jpeg("./figures/NAME.jpg", width = 856, height = 540)
# sets the bottom, left, top and right margins # default below
par(mar=c(5.1,4.1,4.1,2.1))
#<plot code>
dev.off()

#------------------
# trait data
jpeg("./figures/Flower_height_boxplot.jpg", width = 3000, height = 1000)
# sets the bottom, left, top and right margins
par(mar=c(15,4.1,4.1,2.1))
boxplot(Flwr_height~Trmt+Site+plant, data=flwr_trait_data_long, las=2, col="light blue", xlab="Spp/Site/TRTMT", ylab="Flower Height (mm)", main="Flwr Height")
dev.off()

jpeg("./figures/Leaf_height_boxplot.jpg", width = 3000, height = 1000)
# sets the bottom, left, top and right margins
par(mar=c(15,4.1,4.1,2.1))
boxplot(Leaf_height~Trmt+Site+plant, data=veg_trait_data_long, las=2, col="light blue", xlab="Spp/Site/TRTMT", ylab="Leaf Height (mm)", main="Veg Height")
dev.off()

#-------------------
# flower count data

jpeg("./figures/MEAD_flower_count_boxplot.jpg", width = 3000, height = 1000)
# sets the bottom, left, top and right margins
par(mar=c(15,4.1,4.1,2.1))
boxplot(Inflouresc_Count~Trmt+Site+Plant, data=MEAD_FLWR_count_data, las=2, col="light blue", 
        xlab="Spp/Site/TRTMT", ylab="Flower count", main="Flower count Meadow")
dev.off()

jpeg("./figures/CASS_flower_count_boxplot.jpg", width = 3000, height = 1000)
# sets the bottom, left, top and right margins
par(mar=c(15,4.1,4.1,2.1))
boxplot(X~Trmt+Site+Plant.Flower.count, data=CASS_FLWR_count_data, las=2, col="light blue", 
        xlab="Spp/Site/TRTMT", ylab="Flower count", main="Flower count Cassiope")
dev.off()

#-----------------------------
