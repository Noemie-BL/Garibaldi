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


install.packages("devtools")
library(devtools)
install_github("https://github.com/TundraEcologyLab/tundra_lab_r_package")


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
  y$Trmt <- sub("(MEAD)_([0-9]{2})*", "", y$file, ignore.case = TRUE)
  y$Site <- sub("*_([0-9]{2})([W,C]{1})", "", y$file, ignore.case = TRUE)
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
  y$Trmt <- sub("(MEAD)_([0-9]{2})*", "", y$file, ignore.case = TRUE)
  y$Site <- sub("*_([0-9]{2})([W,C]{1})", "", y$file, ignore.case = TRUE)
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
  y$Trmt <- sub("(CASS)_([0-9]{2})*", "", y$file, ignore.case = TRUE)
  y$Site <- sub("*_([0-9]{2})([W,C]{1})", "", y$file, ignore.case = TRUE)
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
  y$Plot <- sub("(MEAD)_*", "", y$file, ignore.case = TRUE)
  y$Trmt <- sub("(MEAD)_([0-9]{2})*", "", y$file, ignore.case = TRUE)
  y$Site <- sub("*_([0-9]{2})([W,C]{1})", "", y$file, ignore.case = TRUE)
  return(y)
}

CASS_trait_data <- list.files(path = "./data/Sphinx_CASS/Traits/", pattern = "*.csv", full.names = TRUE) %>%
  lapply(read_csv_withnames) %>%
  bind_rows

#---------------------------------------
# clean up data

#-------------------------
# trait data
# convert n/a to NA
data_column[which(data_column==orig_name1)] <- new_name1

# remove spaces from species names


# convert TRMT, site and Spp to factors


# convert VEG and FLOW columns to numeric


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

#------------------------------------------
# Linear Mixed Effect Models

library(lme)


Y ~ TRMT * SITE  





#-----------------------------------
# Figures











