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
greenness_data <- read.table("./data/2022_Photo_greenness.csv", header=TRUE, sep =",", dec = ".")

#-----------------------------------------
# find slopes for greenness over time

greenness_slope <- function(Moving_Avg_5, Date){  
  mod <- lm(Moving_Avg_5 ~ Date)
  cf <- coef(mod)
  Slope <- cf[2]
  return(Slope)
}

greenness_data_by_plot <- group_by(greenness_data, PlotTrmt)

greenness_plot_slope <- summarise(greenness_data_by_plot, slope=greenness_slope(Moving_Avg_5, Date))

greenness_plot_slope$Plot <- as.factor(greenness_plot_slope$PlotTrmt)

greenness_plot_slope$Site <- c("SAL","SAL","SAL","SAL","SAL","SAL","SAL","SAL",
                               "CASS","CASS","CASS","CASS","CASS","CASS","CASS","CASS",
                               "MEAD","MEAD","MEAD","MEAD","MEAD","MEAD","MEAD","MEAD")
greenness_plot_slope$Site <- as.factor(greenness_plot_slope$Site)

greenness_plot_slope$Trmt <- c("C","W","C","W","C","W","C","W","C","W","C","W","C","W","C","W","C","W","C","W","C","W","C","W")
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
greenness_data$Date_form <- as.POSIXct(greenness_data$Date, format="%Y-%m-%d")
greenness_data$Trmt <- factor(greenness_data$X, levels = c("W", "C"))

# plot
jpeg(paste0("./figures/Greenness_per_day_Site.jpg"), width = 2000, height = 600)
par(mar=c(20,20,4,4))
ggplot(data=greenness_data,
       aes(x = Date_form, y = Moving_Avg_5, colour = factor(Trmt), group=Trmt))+
  geom_hline(yintercept=0,linetype="dotted",size=1)+
  geom_line(aes(group=factor(PlotTrmt)))+
  geom_smooth(aes(group=Trmt),size=2,se=FALSE)+
  facet_wrap(~Site, scales = "free")+
  theme_bw()+
  theme(legend.title=element_text(size=20,face="bold"),legend.text=element_text(size=20),legend.position="top",legend.key = element_rect(colour = "white"),legend.key.size=unit(1,"cm"),axis.text.x=element_text(size=20,face="bold"),axis.text.y=element_text(hjust=1,size=20),axis.title.x=element_text(size=20,face="bold"),axis.title.y=element_text(angle=90,size=20,face="bold",vjust=0.5),axis.ticks = element_blank(),panel.grid.minor=element_blank(),panel.grid.major=element_blank())+
  ylab("Greenness 5 day moving avg")+
  xlab("Date")+
  guides(col=guide_legend(nrow=2,title="Trmt "))
dev.off()


# Boxplot

#jpeg("./figures/greenness_slope_boxplot.jpg", width = 3000, height = 1000)
ggplot(data=greenness_plot_slope, aes(x=Trmt, y=slope))+
  geom_boxplot(aes(x=Trmt, y=slope, fill=Trmt))+
  facet_wrap(~Site, scales = "free")+ theme_classic()+
  scale_fill_manual(values=c( "#89C5DA", "#DA5724"))+
  labs(fill="Treatment")
#dev.off()









#---------------------------------
# other plotting code
# other box plot code
plot(DOY~Trmt+Stage, data=photo_pheno_data2_long, cex=1.2, xlab="Stage/TRTMT", ylab="DOY", main="Phenology")
stripchart(DOY~Trmt+Stage, vertical = TRUE, method = "jitter", pch = 16,cex.lab=1.5,cex.axis=1.2,
           col = "black", data=photo_pheno_data2_long, add=TRUE)


# code to make histrogram for greenness?
hist(total.fruits, col = "grey", main = "Total fruits", xlab = NULL)








