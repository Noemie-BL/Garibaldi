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

#------------------------------------------
# Linear Mixed Effect Models
# https://www.r-bloggers.com/2017/12/linear-mixed-effect-models-in-r/

library(nlme)
library(lme4)

# phenology photo data 
Pheno_LM <- lm(DOY~Site*Trmt*Stage,  data=photo_pheno_data2_long)
summary(Pheno_LM)
par(mfrow = c(2,2))
plot(Pheno_LM)


#-----------------------------------
# Figures

# Boxplot?
jpeg("./figures/CASS_flower_count_boxplot.jpg", width = 3000, height = 1000)
# sets the bottom, left, top and right margins
par(mar=c(15,4.1,4.1,2.1))
boxplot(X~Trmt+Site+Plant.Flower.count, data=CASS_FLWR_count_data, las=2, col="light blue", 
        xlab="Spp/Site/TRTMT", ylab="Flower count", main="Flower count Cassiope")
dev.off()



# #plotting function daily values for a year
# data_OTCdiff_DOYSUB <- data_OTCdiff_DOY[which(data_OTCdiff_DOY$DOY>150 & data_OTCdiff_DOY$DOY<250),]
# jpeg(paste0("C:/Users/gaiaa/Documents/Cassandra/PhD/OTC_Climate_data/ITEX_climate_data/Figures/", filename, "_OTCdiff_DOY_SUMMER.jpg"), width = 1000, height = 500)
# par(mar=c(20,20,4,4))
# ggplot(data=data_OTCdiff_DOYSUB,
#        aes(x = DOY, y = OTCdiff, colour = factor(Year)))+
#   geom_hline(yintercept=0,linetype="dotted",size=1)+
#   geom_line(aes(group=factor(Year)))+
#   geom_smooth(aes(group=1),size=2,se=FALSE)+
#   theme_bw()+
#   theme(legend.title=element_text(size=20,face="bold"),legend.text=element_text(size=20),legend.position="top",legend.key = element_rect(colour = "white"),legend.key.size=unit(1,"cm"),axis.text.x=element_text(size=20,face="bold"),axis.text.y=element_text(hjust=1,size=20),axis.title.x=element_text(size=20,face="bold"),axis.title.y=element_text(angle=90,size=20,face="bold",vjust=0.5),axis.ticks = element_blank(),panel.grid.minor=element_blank(),panel.grid.major=element_blank())+
#   ylab("Difference between Warm and Control (W-C, °C)\n")+
#   xlab("\nDAY OF THE YEAR")+
#   guides(col=guide_legend(nrow=2,title="YEAR "))
# dev.off()







#---------------------------------
# other plotting code
# other box plot code
plot(DOY~Trmt+Stage, data=photo_pheno_data2_long, cex=1.2, xlab="Stage/TRTMT", ylab="DOY", main="Phenology")
stripchart(DOY~Trmt+Stage, vertical = TRUE, method = "jitter", pch = 16,cex.lab=1.5,cex.axis=1.2,
           col = "black", data=photo_pheno_data2_long, add=TRUE)


# code to make histrogram for greenness?
hist(total.fruits, col = "grey", main = "Total fruits", xlab = NULL)








