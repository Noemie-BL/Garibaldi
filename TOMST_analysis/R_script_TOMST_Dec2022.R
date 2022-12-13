install.packages("plyr")
install.packages("readr")
install.packages("tidyverse")
install.packages("stringr")
install.packages("tidyr")
install.packages("chron")
install.packages("timeDate")
install.packages("dplyr")
install.packages("reshape2")
install.packages("ggplot2")
install.packages("scales")
install.packages("lubridate")

#--------------------------
library(plyr)
library(readr)
library(tidyverse)
library(stringr)
library(tidyr)
library(chron)
library(timeDate)
library(dplyr)
library(reshape2)
library(ggplot2)
library(scales)
library(lubridate)

pathname <- "/home/celphin/projects/def-henryg/Garibaldi_Lake_data_summer2022/celphin_TOMST_workingdir/output/"

# Split filename column into different factors and change column headers before import

TOMSTx <- read.table("TOMST_compiled_data_2022.csv", header=TRUE, sep =";", dec = ",")

###################################################
# convert  all treatment names to OTC and Control 
plot_naming <- function(data_column, orig_Cname, orig_Wname){
  data_column <- as.character(data_column)
  data_column[which(data_column==orig_Cname)] <- "control"
  data_column[which(data_column==orig_Wname)] <- "OTC"
  data_column
}

TOMSTx$treatment <- plot_naming(TOMST$treatment, "C", "W")
TOMSTx$otc_treatment <- TOMST$treatment
##################################################
# check all date formats

# https://www.r-bloggers.com/2013/08/date-formats-in-r/
# https://stackoverflow.com/questions/11609252/r-tick-data-merging-date-and-time-into-a-single-object

# All measurement runs in UTC, please use time zone parameter for recalculation to the local time. 

TOMSTx$form_date_time <- as.POSIXct(TOMST$date_time_UTC , format="%Y.%m.%d %H:%M", tz = "UTC")

###################################################
# remove data before placed in field - start July 23? DOY203

TOMSTx$DOY=yday(TOMSTx$form_date_time)

TOMST <- TOMSTx[which(TOMSTx$DOY>"203"),]

####################################################
# Functions
#  { }=.data[ [ ] ] 
# https://cran.r-project.org/web/packages/dplyr/vignettes/programming.html

OTC_diff_hourly <- function(mydata, treatment, variable){
  # add hour and month columns
  mydata$Hour <- format.Date(mydata$form_date_time, "%H")
  mydata$month <-format.Date(mydata$form_date_time, "%m")
  
  # hourly average difference per month
  data_OTC_hour <- group_by(mydata, across({{ treatment }}), month, Hour)
  data_OTCavg_hour <- summarise(data_OTC_hour, across({{ variable }}, ~ mean(., na.rm = TRUE)))
  
  # take difference between warm and control plots
  data_OTCdiff_hour <- spread(data_OTCavg_hour, {{ treatment }}, {{ variable }})
  data_OTCdiff_hour$OTCdiff <- (data_OTCdiff_hour$OTC - data_OTCdiff_hour$control)
  data_OTCdiff_hour$mix <- paste0(data_OTCdiff_hour$month,"-", data_OTCdiff_hour$Hour)
  return(data_OTCdiff_hour)
}

#Example usage
#Dryas_air_temp_hourly_diff <- OTC_diff_hourly(Dryas_air_temp, "otc_treatment", "air_temp") 


#---------------------------------------
OTC_diff_monthly <- function(mydata, treatment, variable){
  
  # add year and month columns
  mydata$Year=format.Date(mydata$form_date_time, "%Y")
  mydata$month <-format.Date(mydata$form_date_time, "%m")
  
  # monthly average difference for each year
  data_OTC_month <- group_by(mydata, across({{ treatment }}), month, Year)
  data_OTCavg_month <- summarise(data_OTC_month, across({{ variable }}, ~ mean(., na.rm = TRUE)))
  
  # take difference between warm and control plots
  data_OTCdiff_month <- spread(data_OTCavg_month, {{ treatment }}, {{ variable }})
  data_OTCdiff_month$OTCdiff <- (data_OTCdiff_month$OTC - data_OTCdiff_month$control)
  data_OTCdiff_month$mix <- paste0(data_OTCdiff_month$Year,"-", data_OTCdiff_month$month)
  
  return(data_OTCdiff_month)
  
}

#Example usage
#Dryas_air_temp_monthly_diff <- OTC_diff_monthly(Dryas_air_temp, "otc_treatment", "air_temp") 


#---------------------------------------
OTC_diff_DOY <- function(mydata, treatment, variable){
  # add year and DOY columns
  mydata$Year=format.Date(mydata$form_date_time, "%Y")
  mydata$DOY=yday(mydata$form_date_time)
  
  # daily average difference for each year
  data_OTC_DOY <- group_by(mydata, across({{ treatment }}), DOY, Year)
  data_OTCavg_DOY <- summarise(data_OTC_DOY, across({{ variable }}, ~ mean(., na.rm = TRUE)))

  # take difference between warm and control plots
  data_OTCdiff_DOY<- spread(data_OTCavg_DOY, {{ treatment }}, {{ variable }})
  data_OTCdiff_DOY$OTCdiff <- data_OTCdiff_DOY$OTC - data_OTCdiff_DOY$control
  data_OTCdiff_DOY$mix <- paste0(data_OTCdiff_DOY$Year,"-", data_OTCdiff_DOY$DOY)
  
  return(data_OTCdiff_DOY)
}

#Example usage
#Dryas_air_temp_DOY_diff <- OTC_diff_DOY(Dryas_air_temp, "otc_treatment", "air_temp") 

#---------------------------------------
# plotting function

OTC_plot_DOY <- function(data_OTCdiff_DOY, filename, Title){
  data_OTCdiff_DOY_SUB <- data_OTCdiff_DOY
  #data_OTCdiff_DOY_SUB2 <- data_OTCdiff_DOY_SUB[which(data_OTCdiff_DOY_SUB$DOY>130 & data_OTCdiff_DOY_SUB<270),]
  
  jpeg(paste("C:/Users/gaiaa/Documents/Cassandra/PhD/OTC_Climate_data/R_Figures_Final/", filename, "_OTCdiff_Daily.jpg", sep=""), width = 2000, height = 1000)
  par(mar=c(20,20,4,4))
  ggplot(data=data_OTCdiff_DOY_SUB,
         aes(x = DOY, y = OTCdiff, colour = factor(Year)))+
    geom_hline(yintercept=0,linetype="dotted",size=1)+
    geom_line(aes(group=factor(Year)))+
    geom_smooth(aes(group=1),size=2,se=FALSE)+
    ggtitle(Title)+
    theme_bw()+
    theme(legend.title=element_text(size=20,face="bold"),legend.text=element_text(size=20),legend.position="top",legend.key = element_rect(colour = "white"),legend.key.size=unit(1,"cm"),axis.text.x=element_text(size=20,face="bold"),axis.text.y=element_text(hjust=1,size=20),axis.title.x=element_text(size=20,face="bold"),axis.title.y=element_text(angle=90,size=20,face="bold",vjust=0.5),axis.ticks = element_blank(),panel.grid.minor=element_blank(),panel.grid.major=element_blank())+
    ylim(-10,10)+
    ylab("Temperature Difference (°C)\n")+
    xlab("\nDAY OF THE YEAR")+
    guides(col=guide_legend(nrow=2,title="YEAR "))
  dev.off()
}

OTC_plot_hourly <- function(data_OTCdiff_hour, filename, Title){
  data_OTCdiff_hour_SUB <- data_OTCdiff_hour[c(which(data_OTCdiff_hour$month=="06"), which(data_OTCdiff_hour$month=="07"), which(data_OTCdiff_hour$month=="08")),]
  jpeg(paste0("C:/Users/gaiaa/Documents/Cassandra/PhD/OTC_Climate_data/R_Figures_Final/", filename, "_OTCdiff_hourly.jpg"), width = 1000, height = 500)
  par(mar=c(20,20,4,4))
  ggplot(data=data_OTCdiff_hour_SUB,
         aes(x = Hour, y = OTCdiff, colour = factor(month)))+
    geom_hline(yintercept=0,linetype="dotted",size=1)+
    geom_line(aes(group=factor(month)))+
    geom_smooth(aes(group=1),size=2,se=FALSE)+
    ggtitle(Title)+
    theme_bw()+
    theme(legend.title=element_text(size=20,face="bold"),legend.text=element_text(size=20),legend.position="top",legend.key = element_rect(colour = "white"),legend.key.size=unit(1,"cm"),axis.text.x=element_text(size=20,face="bold"),axis.text.y=element_text(hjust=1,size=20),axis.title.x=element_text(size=20,face="bold"),axis.title.y=element_text(angle=90,size=20,face="bold",vjust=0.5),axis.ticks = element_blank(),panel.grid.minor=element_blank(),panel.grid.major=element_blank())+
    ylab("Temperature Difference (°C)\n")+
    xlab("\nHour")+
    guides(col=guide_legend(nrow=2,title="Month "))
  dev.off()
  print(
    ggplot(data=data_OTCdiff_hour_SUB,
           aes(x = Hour, y = OTCdiff, colour = factor(month)))+
      geom_hline(yintercept=0,linetype="dotted",size=1)+
      geom_line(aes(group=factor(month)))+
      geom_smooth(aes(group=1),size=2,se=FALSE)+
      ggtitle(Title)+
      theme_bw()+
      theme(legend.title=element_text(size=20,face="bold"),legend.text=element_text(size=20),legend.position="top",legend.key = element_rect(colour = "white"),legend.key.size=unit(1,"cm"),axis.text.x=element_text(size=20,face="bold"),axis.text.y=element_text(hjust=1,size=20),axis.title.x=element_text(size=20,face="bold"),axis.title.y=element_text(angle=90,size=20,face="bold",vjust=0.5),axis.ticks = element_blank(),panel.grid.minor=element_blank(),panel.grid.major=element_blank())+
      ylab("Temperature Difference (°C)\n")+
      xlab("\nHour")+
      guides(col=guide_legend(nrow=2,title="Month "))
  )
}


#######################################
# year lists

Year_list <- function(mydata){
  mydata$Year=format.Date(mydata$form_date_time, "%Y")
  Yr_list <- unique(mydata$Year)
  print(Yr_list)
}

Year_list(TOMST)

###############################################
# running code

mydata <- TOMST
mydata$measure <-  as.numeric(as.character(mydata$Temp_2))
mydata <- mydata[-which(mydata$measure>50 | mydata$measure<(-50)),]
filename <- "TOMST_tmp2_mid"

mydata$measure <-  as.numeric(as.character(mydata$Temp_3))
mydata <- mydata[-which(mydata$measure>50 | mydata$measure<(-50)),]
filename <- "TOMST_tmp3_air"

mydata$measure <-  as.numeric(as.character(mydata$Temp_1))
mydata <- mydata[-which(mydata$measure>50 | mydata$measure<(-50)),]
filename <- "TOMST_tmp1_soil"

# soil mositure no relative values
mydata$measure <-  as.numeric(as.character(mydata$soil_moisture))
filename <- "TOMST_soil_moist"

#-----------------------------------
# relative soil mositure
# take the starting value for each plot and subtract it from all values

TOMST$soil_moisture_rel <- 

##############################################
# function to take OTC - control difference each hour avg each month

#OTC_diff_FN <- function(mydata){
# hourly average difference per month
data_OTC_hour <- group_by(mydata, otc_treatment, Hour=format.Date(form_date_time, "%H"), month=format.Date(form_date_time, "%m"))

data_OTCavg_hour <- data_OTC_hour  %>% summarise(
  avg = mean(measure, na.rm=TRUE)#,
  #count = n()
) 

#-----------------------------------------------
# function to take OTC - control difference each hour avg each site

#OTC_diff_FN <- function(mydata){
# hourly average difference per site
data_OTC_hour_site <- group_by(mydata, otc_treatment, site, Hour=format.Date(form_date_time, "%H"))

data_OTCavg_hour_site <- data_OTC_hour_site  %>% summarise(
  avg = mean(measure, na.rm=TRUE)#,
  #count = n()
) 

#-----------------------------------------------
# daily average difference for each site

data_OTC_DOY <- group_by(mydata, otc_treatment, DOY=yday(form_date_time), site)

data_OTCavg_DOY <- data_OTC_DOY  %>% summarise(
  avg = mean(measure, na.rm=TRUE)#,
  #count = n()
) 

#------------------------------------------------------------------
# monthly average difference for each site

data_OTC_month <- group_by(mydata, otc_treatment,  month=format.Date(form_date_time, "%m"),  site)

data_OTCavg_month <- data_OTC_month  %>% summarise(
  avg = mean(measure, na.rm=TRUE)#,
  #count = n()
) 

##################################################
# take difference between warm and control plots

# spread

data_OTCdiff_hour <- spread(data_OTCavg_hour, otc_treatment, avg)
data_OTCdiff_DOY<- spread(data_OTCavg_DOY, otc_treatment, avg)
data_OTCdiff_month <- spread(data_OTCavg_month, otc_treatment, avg)
data_OTCdiff_hour_site <- spread(data_OTCavg_hour_site, otc_treatment, avg)

data_OTCdiff_hour$OTCdiff <- data_OTCdiff_hour$OTC-data_OTCdiff_hour$control
data_OTCdiff_DOY$OTCdiff <- data_OTCdiff_DOY$OTC-data_OTCdiff_DOY$control
data_OTCdiff_month$OTCdiff <- data_OTCdiff_month$OTC-data_OTCdiff_month$control
data_OTCdiff_hour_site$OTCdiff <- data_OTCdiff_hour_site$OTC-data_OTCdiff_hour_site$control

data_OTCdiff_DOY$mix <- paste0(data_OTCdiff_DOY$site,"-", data_OTCdiff_DOY$DOY)
data_OTCdiff_month$mix <- paste0(data_OTCdiff_month$site,"-", data_OTCdiff_month$month)
data_OTCdiff_hour_site$mix <- paste0(data_OTCdiff_hour_site$site,"-", data_OTCdiff_hour_site$Hour)
data_OTCdiff_hour$mix <-  paste0(data_OTCdiff_hour$month,"-", data_OTCdiff_hour$Hour)

#######################################
#plots

#plotting function daily values for summer

jpeg(paste0(pathname, filename, "_OTCdiff_DOY_site.jpg"), width = 1000, height = 500)
par(mar=c(20,20,4,4))
ggplot(data=data_OTCdiff_DOY,
       aes(x = DOY, y = OTCdiff, colour = factor(site)))+
  geom_hline(yintercept=0,linetype="dotted",size=1)+
  geom_line(aes(group=factor(site)))+
  geom_smooth(aes(group=1),size=2,se=FALSE)+
  theme_bw()+
  theme(legend.title=element_text(size=20,face="bold"),legend.text=element_text(size=20),legend.position="top",legend.key = element_rect(colour = "white"),legend.key.size=unit(1,"cm"),axis.text.x=element_text(size=20,face="bold"),axis.text.y=element_text(hjust=1,size=20),axis.title.x=element_text(size=20,face="bold"),axis.title.y=element_text(angle=90,size=20,face="bold",vjust=0.5),axis.ticks = element_blank(),panel.grid.minor=element_blank(),panel.grid.major=element_blank())+
  ylab("Difference between Warm and Control (W-C, °C)\n")+
  xlab("\nDAY OF THE YEAR")+
  guides(col=guide_legend(nrow=2,title="Site "))
dev.off()

jpeg(paste0(pathname, filename, "_OTCdiff_hourly_site.jpg"), width = 1000, height = 500)
par(mar=c(20,20,4,4))
ggplot(data=data_OTCdiff_hour_site,
       aes(x = Hour, y = OTCdiff, colour = factor(site)))+
  geom_hline(yintercept=0,linetype="dotted",size=1)+
  geom_line(aes(group=factor(site)))+
  geom_smooth(aes(group=1),size=2,se=FALSE)+
  theme_bw()+
  theme(legend.title=element_text(size=20,face="bold"),legend.text=element_text(size=20),legend.position="top",legend.key = element_rect(colour = "white"),legend.key.size=unit(1,"cm"),axis.text.x=element_text(size=20,face="bold"),axis.text.y=element_text(hjust=1,size=20),axis.title.x=element_text(size=20,face="bold"),axis.title.y=element_text(angle=90,size=20,face="bold",vjust=0.5),axis.ticks = element_blank(),panel.grid.minor=element_blank(),panel.grid.major=element_blank())+
  ylab("Temperature Difference (°C)\n")+
  xlab("\nHour")+
  guides(col=guide_legend(nrow=2,title="Site "))
dev.off()

jpeg(paste0(pathname, filename, "_OTCdiff_hourly_month.jpg"), width = 1000, height = 500)
par(mar=c(20,20,4,4))
ggplot(data=data_OTCdiff_hour,
       aes(x = Hour, y = OTCdiff, colour = factor(month)))+
  geom_hline(yintercept=0,linetype="dotted",size=1)+
  geom_line(aes(group=factor(month)))+
  geom_smooth(aes(group=1),size=2,se=FALSE)+
  theme_bw()+
  theme(legend.title=element_text(size=20,face="bold"),legend.text=element_text(size=20),legend.position="top",legend.key = element_rect(colour = "white"),legend.key.size=unit(1,"cm"),axis.text.x=element_text(size=20,face="bold"),axis.text.y=element_text(hjust=1,size=20),axis.title.x=element_text(size=20,face="bold"),axis.title.y=element_text(angle=90,size=20,face="bold",vjust=0.5),axis.ticks = element_blank(),panel.grid.minor=element_blank(),panel.grid.major=element_blank())+
  ylab("Temperature Difference (°C)\n")+
  xlab("\nHour")+
  guides(col=guide_legend(nrow=2,title="Month "))
dev.off()

jpeg(paste0(pathname, filename, "_OTCdiff_site_month.jpg"), width = 1000, height = 500)
par(mar=c(20,20,4,4))
ggplot(data=data_OTCdiff_month,
       aes(x = month, y = OTCdiff, colour = factor(site)))+
  geom_hline(yintercept=0,linetype="dotted",size=1)+
  geom_line(aes(group=factor(site)))+
  geom_smooth(aes(group=1),size=2,se=FALSE)+
  theme_bw()+
  theme(legend.title=element_text(size=20,face="bold"),legend.text=element_text(size=20),legend.position="top",legend.key = element_rect(colour = "white"),legend.key.size=unit(1,"cm"),axis.text.x=element_text(size=8),axis.text.y=element_text(hjust=1,size=20),axis.title.x=element_text(size=20,face="bold"),axis.title.y=element_text(angle=90,size=20,face="bold",vjust=0.5),axis.ticks = element_blank(),panel.grid.minor=element_blank(),panel.grid.major=element_blank())+
  ylab("Temperature Difference (°C)\n")+
  xlab("\nMonth")+
  guides(col=guide_legend(nrow=2,title="YEAR "))
dev.off()


####################################################
####################################################
#######################################################

