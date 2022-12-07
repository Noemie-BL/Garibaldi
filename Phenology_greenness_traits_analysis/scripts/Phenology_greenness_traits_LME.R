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
photo_pheno_data <- read.table("./data/Photo_Phenology_Data.csv", header=TRUE, sep =",", dec = ".")
greenness_data <- read.table("./data/Photo_Phenology_Data.csv", header=TRUE, sep =",", dec = ".")

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

# trait data
# convert n/a to NA
data_column[which(data_column==orig_name1)] <- new_name1

# remove spaces from species names


# convert TRMT, site and Spp to factors


# convert VEG and FLOW columns to numeric



#------------------------------------------
# Linear Mixed Effect Models

library(lme)


Y ~ TRMT * SITE  





#-----------------------------------
# Figures











