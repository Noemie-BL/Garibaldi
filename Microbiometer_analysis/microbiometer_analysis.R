library(ggplot2)
library(tidyr)
library(dplyr)
library(tidyverse)
library(lubridate)
library(lme4)
library(lmerTest)

dat<-read.csv("Microbiometer_analysis/Microbiometer_Data.csv")
dat<-rename(dat, treatment=treatment..W.C.)
str(dat)

#make pre/post flood 
dat$plot<-as.numeric(dat$Plot)

#S1= first sampling S2= second sampling
dat<-mutate(dat, doy=yday(Date))%>%
  mutate(time=if_else(doy<250, "S1", "S2"))%>%
        mutate(., flood=case_when(plot<7 ~"flood",
                               TRUE~ "no flood"))

#dat<-mutate(dat, time=fct_relevel(time, "pre", "post"))

#Mic biomass by treatment 
            ggplot(dat, aes(x=Site, y=microbial.biomass.C..ug.g., fill=treatment))+
              geom_boxplot(aes(fill=treatment))+
             ylab("Soil Microbial biomass C")+ theme_bw() + xlab("Plant community")+
            scale_fill_manual(values=c( "#89C5DA", "#DA5724")) 
            
            ggplot(dat, aes(x=Site, y=microbial.biomass.C..ug.g.))+
              geom_jitter(mapping = aes(x=Site, y=microbial.biomass.C..ug.g., colour=treatment), 
                          size=1, alpha=0.5, position=position_dodge(width=1))+
              geom_pointrange(mapping = aes(x=Site, y=microbial.biomass.C..ug.g., colour=treatment),
                              stat = "summary", position=position_dodge(width=1), size=1.2)+
              scale_color_manual(values=c( "#89C5DA", "#DA5724")) 
            
#F:B by treatment             
            ggplot(dat, aes(x=Site, y=F.B, fill=treatment))+
            geom_boxplot(aes(fill=treatment))+
              ylab("Fungal:Bacterial ratio")+ theme_bw() + xlab("Plant community")+
              scale_fill_manual(values=c( "#89C5DA", "#DA5724")) 
            
            ggplot(dat, aes(x=Site, y=F.B))+
            geom_jitter(mapping = aes(x=Site, y=F.B, colour=treatment), 
                        size=1, alpha=0.5, position=position_dodge(width=1))+
              geom_pointrange(mapping = aes(x=Site, y=F.B, colour=treatment),
                              stat = "summary", position=position_dodge(width=1), size=1.2)+
              scale_color_manual(values=c( "#89C5DA", "#DA5724")) 
            

    ggplot(dat, aes(x=Site, y=F.B, fill=treatment))+
              geom_boxplot(aes(fill=treatment))+
              ylab("Fungal:Bacterial ratio")+ theme_bw() + xlab("Plant community")+
              scale_fill_manual(values=c( "#89C5DA", "#DA5724")) 

#plots for BC PARF talk 
#first trip vs. second trip by treatment     
#MBC
ggplot(dat, aes(x=time, y=microbial.biomass.C..ug.g., fill=treatment))+
      geom_boxplot(aes(fill=treatment))+
      ylab("Soil Microbial biomass C")+ theme_bw() + xlab("Plant community")+
      scale_fill_manual(values=c( "#89C5DA", "#DA5724")) +
      facet_wrap(~Site)
#F:B
ggplot(dat, aes(x=time, y= F.B, fill=treatment))+
  geom_boxplot(aes(fill=treatment))+
  ylab("Fungi:Bacteria")+ theme_bw() + xlab("Plant community")+
  scale_fill_manual(values=c( "#89C5DA", "#DA5724")) +
  facet_wrap(~Site)

#during vs. post flood 
#Subset for flooded plots 
#MBC
ggplot(subset(dat,Site=="Salix"&flood!="no flood"), aes(x=flood, y=microbial.biomass.C..ug.g., fill=treatment))+
  geom_boxplot(aes(fill=treatment))+
  ylab("Soil Microbial biomass C")+ theme_bw() + xlab("Plant community")+
  scale_fill_manual(values=c( "#89C5DA", "#DA5724")) +
  facet_wrap(~time)

#F:B
ggplot(subset(dat,Site=="Salix"&flood!="no flood"), aes(x=flood, y=F.B, fill=treatment))+
  geom_boxplot(aes(fill=treatment))+
  ylab("Fungi:Bacteria")+ theme_bw() + xlab("Plant community")+
  scale_fill_manual(values=c( "#89C5DA", "#DA5724")) +
  facet_wrap(~time)
