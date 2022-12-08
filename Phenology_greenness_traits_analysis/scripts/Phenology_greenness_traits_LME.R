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

#------------------------------
# import the data
greenness_data <- read.table("./data/Photo_Phenology_Data.csv", header=TRUE, sep =",", dec = ".")



#----------------------------
# phenology photo data
photo_pheno_data <- read.table("./data/Photo_Phenology_Data.csv", header=TRUE, sep =",", dec = ".")

# extract site, treatment, plot
photo_pheno_data$file <- photo_pheno_data$Site

photo_pheno_data$Plot <- sub("([A-Z]{3,4})_*", "", photo_pheno_data$file, ignore.case = TRUE)
photo_pheno_data$Trmt <- sub("([0-9]{1,2})*", "", photo_pheno_data$Plot, ignore.case = TRUE)
photo_pheno_data$Site <- sub("*_([0-9]{1,2})([W,C]{1})$", "", photo_pheno_data$file, ignore.case = TRUE)


#----------------------------------
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


#-------------------------
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


library(nlme)
library(lme4)

# phenology photo data 
LM <- lm(DOY~Site+Trmt+Stage,  data=photo_pheno_data2_long)
summary(LM)
par(mfrow = c(2,2))
plot(LM)

# trait data



# flower count data





#-----------------------------------
# Figures

# example of how to save a jpeg image in R
jpeg("./figures/NAME.jpg", width = 856, height = 540)
# sets the bottom, left, top and right margins # default below
par(mar=c(5.1,4.1,4.1,2.1))
#<plot code>
dev.off()

jpeg("./figures/Photo_phenology_boxplot.jpg", width = 1000, height = 1000)
# sets the bottom, left, top and right margins
par(mar=c(10,4.1,4.1,2.1))
boxplot(DOY~Trmt+Site+Stage, data=photo_pheno_data2_long, las=2, col="light blue", xlab="Stage/TRTMT", ylab="DOY", main="Phenology")
dev.off()


jpeg("./figures/Flower_height_boxplot.jpg", width = 3000, height = 1000)
# sets the bottom, left, top and right margins
par(mar=c(15,4.1,4.1,2.1))
boxplot(Flwr_height~Trmt+Site+plant, data=flwr_trait_data_long, las=2, col="light blue", xlab="Stage/TRTMT", ylab="DOY", main="Phenology")
dev.off()

jpeg("./figures/Leaf_height_boxplot.jpg", width = 3000, height = 1000)
# sets the bottom, left, top and right margins
par(mar=c(15,4.1,4.1,2.1))
boxplot(DOY~Trmt+Site+Stage, data=veg_trait_data_long, las=2, col="light blue", xlab="Stage/TRTMT", ylab="DOY", main="Phenology")
dev.off()









#---------------------------------
# other plotting code
# other box plot code
plot(DOY~Trmt+Stage, data=photo_pheno_data2_long, cex=1.2, xlab="Stage/TRTMT", ylab="DOY", main="Phenology")
stripchart(DOY~Trmt+Stage, vertical = TRUE, method = "jitter", pch = 16,cex.lab=1.5,cex.axis=1.2,
           col = "black", data=photo_pheno_data2_long, add=TRUE)


# code to make histrogram for greenness?
hist(total.fruits, col = "grey", main = "Total fruits", xlab = NULL)








