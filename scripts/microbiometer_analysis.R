library(ggplot2)
library(tidyr)
library(dplyr)

setwd("C:/Users/court/Google Drive/UBC Postdoc/Garibaldi project/Data_August_2022/Microbiometer")
dat<-read.csv("Microbiometer Data.csv")

dat<-rename(dat, treatment=treatment..W.C.)
dat<-mutate(dat, treatment= fct_relevel("W", "C"))

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
            

