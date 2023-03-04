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
library(zoo)
library(distillery)
library(psych) 


# install.packages("devtools")
# library(devtools)
# install_github("https://github.com/TundraEcologyLab/tundra_lab_r_package")


#------------------------------
# first change path to where you want the figures output to
setwd("~/GitHub/Garibaldi/Greenness_Phenos/")

#------------------------------
# import the data - ideally this is the only part of the script that changes per dataset
# currently works for quadrant data only

greenness_data_poster_raw <- read.table("./data/2022_poster_method.csv", header=FALSE, sep =",", dec = ".")
colnames(greenness_data_poster_raw) <- c("Site", "Plot", "Trmt", "Filename", "Date", "Quad1", "Quad2", "Quad3", "Quad4")
data_name <- "Poster"
greenness_data_raw <- greenness_data_poster_raw

greenness_data_2G_RBI_quad_raw <- read.table("./data/2022_2G-RBI_quadrants.csv", header=FALSE, sep =",", dec = ".")
colnames(greenness_data_2G_RBI_quad_raw) <- c("Site", "Plot", "Trmt", "Filename", "Date", "Quad1", "Quad2", "Quad3", "Quad4")
data_name <- "2G_RBI_quad"
greenness_data_raw <- greenness_data_2G_RBI_quad_raw

greenness_data_GCC_Quad_raw <- read.table("./data/2022_GCC_quadrants.csv", header=FALSE, sep =",", dec = ".")
colnames(greenness_data_GCC_Quad_raw) <- c("Site", "Plot", "Trmt", "Filename", "Date", "Quad1", "Quad2", "Quad3", "Quad4")
data_name <- "GCC_Quad"
greenness_data_raw <- greenness_data_GCC_Quad_raw

greenness_data_oldmeth_Quad_raw <- read.table("./data/2022_oldmethod_quadrants.csv", header=FALSE, sep =",", dec = ".")
colnames(greenness_data_oldmeth_Quad_raw) <- c("Site", "Plot", "Trmt", "Filename", "Date", "Quad1", "Quad2", "Quad3", "Quad4")
data_name <- "oldmeth_Quad"
greenness_data_raw <- greenness_data_oldmeth_Quad_raw


#---------------------
# calculate Quadrant average
greenness_data_raw$mean <- rowMeans(subset(greenness_data_raw, select = c("Quad1", "Quad2", "Quad3", "Quad4")), na.rm = TRUE)
greenness_data_raw$harm_mean <- apply(subset(greenness_data_raw, select = c("Quad1", "Quad2", "Quad3", "Quad4")), 1, harmonic.mean, na.rm=TRUE)

# calculate SD
greenness_data_raw$SD <- apply(subset(greenness_data_raw, select = c("Quad1", "Quad2", "Quad3", "Quad4")), 1, sd, na.rm=TRUE)

# remove values with high variation or NA
greenness_data_raw <- na.omit(greenness_data_raw)
#greenness_data_raw <- greenness_data_raw[-which(greenness_data_raw$mean < 0.1),]
#greenness_data_raw <- greenness_data_raw[-which(greenness_data_raw$SD > 1.0),]

#-----------------------
# Set data set for analysis - can choose mean, median or harmonic mean
greenness_data_raw$Greenness_Index <- greenness_data_raw$mean
greenness_data <- greenness_data_raw

#------------------------
# Calculate rolling average
greenness_data$Plot <- as.factor(greenness_data$Plot)
greenness_data_by_plot <- group_by(greenness_data, Plot=Plot)

# Rolling mean
greenness_roll_mean <- summarise(greenness_data_by_plot, rolling_mean=zoo::rollmean(Greenness_Index, k=7, fill = NA))
greenness_data$Greenness_Index_rolling <- greenness_roll_mean$rolling_mean

# Calculate rolling median over 5 days
greenness_roll_median<- summarise(greenness_data_by_plot, rolling_median=zoo::rollmedian(Greenness_Index, k=7, fill = NA))
greenness_data$Greenness_Index_rolling_med <- greenness_roll_median$rolling_median

#--------------------------------
# For each plot normalize greenness to first value

greenness_normalize <- function(Greenness){  
  Greenness_norm =  Greenness - mean(Greenness,na.rm=TRUE)
  return(Greenness_norm)
}
greenness_data_by_plot <- group_by(greenness_data, Plot=Plot)
greenness_normalize <- summarise(greenness_data_by_plot, normalized=greenness_normalize(Greenness_Index_rolling))

greenness_data$Greenness_Index_rolling_norm <- greenness_normalize$normalized

#-----------------------------------------
# find slopes for greenness over time

greenness_slope <- function(Greenness, Date){  
  mod <- lm(Greenness ~ Date)
  cf <- coef(mod)
  Slope <- cf[2]
  return(Slope)
}

# remove NA rows
greenness_data <- na.omit(greenness_data)

greenness_data$Plot <- as.factor(greenness_data$Plot)
greenness_data$Date_form <- as.POSIXct(greenness_data$Date, format="%Y-%m-%d")
greenness_data_by_plot <- group_by(greenness_data, Plot=Plot)
greenness_plot_slope <- summarise(greenness_data_by_plot, slope=greenness_slope(Greenness_Index_rolling_norm, Date_form))

greenness_plot_slope$Plot_num <- as.numeric(as.character(greenness_plot_slope$Plot))
greenness_plot_slope$Trmt <- greenness_plot_slope$Plot_num
greenness_plot_slope$Trmt[which(distillery::is.even(greenness_plot_slope$Plot_num))] <- "W"
greenness_plot_slope$Trmt[which(distillery::is.odd(greenness_plot_slope$Plot_num))] <- "C"

# greenness_plot_slope$Site <- c("SAL","SAL","SAL","SAL","SAL","SAL","SAL","SAL",
#                                "CASS","CASS","CASS","CASS","CASS","CASS","CASS","CASS",
#                                "MEAD","MEAD","MEAD","MEAD","MEAD","MEAD","MEAD","MEAD")

greenness_plot_slope$Site <- greenness_plot_slope$Trmt
greenness_plot_slope$Site[which(greenness_plot_slope$Plot_num < 9 )] <- "SAL"
greenness_plot_slope$Site[which(greenness_plot_slope$Plot_num > 16 )] <- "MEAD"
greenness_plot_slope$Site[which(greenness_plot_slope$Plot_num < 17 & greenness_plot_slope$Plot_num > 8)] <- "CASS"

greenness_plot_slope$Site <- as.factor(greenness_plot_slope$Site)
greenness_plot_slope$Trmt <- as.factor(greenness_plot_slope$Trmt)

#------------------------------------------
# Linear Mixed Effect Models
# https://www.r-bloggers.com/2017/12/linear-mixed-effect-models-in-r/

library(nlme)
library(lme4)

# greenness photo data 
Green_Slope_LM <- lm(slope~Trmt,  data=greenness_plot_slope)
summary(Green_Slope_LM)
par(mfrow = c(2,2))
plot(Green_Slope_LM)


#-----------------------------------
# Figures

# X= date, Y=greenness, lines for each plot
#plotting daily values for a year

# edit date format
#greenness_data$Trmt <- greenness_data$Plot
#greenness_data$Trmt[which(distillery::is.even(greenness_data$Plot))] <- "W"
#greenness_data$Trmt[which(distillery::is.odd(greenness_data$Plot))] <- "C"

# plot
jpeg(paste0("./figures/", data_name, "_Greenness_per_day_Site.jpg"), width = 2000, height = 850)
par(mar=c(20,20,4,4))
ggplot(data=greenness_data,
       aes(x = Date_form, y = Greenness_Index_rolling_norm, colour = factor(Trmt), group=Trmt))+
  geom_hline(yintercept=0,linetype="dotted",size=1)+
  geom_line(aes(group=factor(Plot)))+
  geom_smooth(aes(group=Trmt),size=2,se=FALSE)+
  facet_wrap(~Site, scales = "free")+
  theme_bw()+
  theme(legend.title=element_text(size=20,face="bold"),legend.text=element_text(size=20),legend.position="top",legend.key = element_rect(colour = "white"),legend.key.size=unit(1,"cm"),axis.text.x=element_text(size=20,face="bold"),axis.text.y=element_text(hjust=1,size=20),axis.title.x=element_text(size=20,face="bold"),axis.title.y=element_text(angle=90,size=20,face="bold",vjust=0.5),axis.ticks = element_blank(),panel.grid.minor=element_blank(),panel.grid.major=element_blank())+
  ylab("Greenness 5 day moving avg")+
  xlab("Date")+
  guides(col=guide_legend(nrow=2,title="Trmt "))
dev.off()


# Boxplot

jpeg(paste0("./figures/", data_name, "_Greenness_slope_boxplot.jpg"), width = 800, height = 500)
ggplot(data=greenness_plot_slope, aes(x=Trmt, y=slope))+
  geom_boxplot(aes(x=Trmt, y=slope, fill=Trmt))+
  facet_wrap(~Site, scales = "free")+ theme_classic()+
  scale_fill_manual(values=c( "#89C5DA", "#DA5724"))+
  labs(fill="Treatment")
dev.off()









#---------------------------------
# other older plotting code
# 
# # other box plot code
# plot(DOY~Trmt+Stage, data=photo_pheno_data2_long, cex=1.2, xlab="Stage/TRTMT", ylab="DOY", main="Phenology")
# stripchart(DOY~Trmt+Stage, vertical = TRUE, method = "jitter", pch = 16,cex.lab=1.5,cex.axis=1.2,
#            col = "black", data=photo_pheno_data2_long, add=TRUE)
# 
# 
# # code to make histrogram for greenness?
# hist(total.fruits, col = "grey", main = "Total fruits", xlab = NULL)

