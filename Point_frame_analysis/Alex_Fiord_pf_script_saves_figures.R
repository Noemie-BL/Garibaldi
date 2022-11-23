# point frame R script

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

# first change path to where you want the figures output to
pathname <- "~/GitHub/Alexandra_Fiord/Data_papers/Point_frame_analysis/figures"
setwd("~/GitHub/Alexandra_Fiord/Data_papers/Point_frame_analysis")

# Split filename column into different factors and change column headers before import

pf_data <- read.table("./data/compiled_point_frame_1995-2019.csv", header=TRUE, sep =",", dec = ".")

###################################################
# clean up the compiled file
# check all species, site and tissue names

plot_naming <- function(data_column, orig_name1, new_name1){
  data_column <- as.character(data_column)
  data_column[which(data_column==orig_name1)] <- new_name1
  data_column
}

colnames(pf_data)

unique(pf_data$location)       
unique(pf_data$plot_id)                
unique(pf_data$year)                
unique(pf_data$day)
unique(pf_data$site)              
unique(pf_data$plot)           
unique(pf_data$otc_treatment)           
unique(pf_data$snow_treatment)
unique(pf_data$x)         
unique(pf_data$y)             
unique(pf_data$hit_order)             
unique(pf_data$species)
unique(pf_data$status)         
unique(pf_data$tissue)             
unique(pf_data$leaf_state)            
unique(pf_data$flower_state)
unique(pf_data$fert_treatment)
unique(pf_data$canopy_height_mm)
unique(pf_data$observer)

#################################################
# data already looks mostly clean
unique(pf_data$species)

pf_data$species <- plot_naming(pf_data$species, "", "NA")
pf_data$species <- plot_naming(pf_data$species, "moss", "MOSS")
pf_data$species <- plot_naming(pf_data$species, "pan_trap", "X")
pf_data$species <- plot_naming(pf_data$species, "hole", "X")
pf_data$species <- plot_naming(pf_data$species, "XXXAGARICUS", "X")
pf_data$species <- plot_naming(pf_data$species, "XXXSEEDCASSIOPE96MONOCOT", "X")
pf_data$species <- plot_naming(pf_data$species, "XXXSEEDCASSIOPE96DICOT", "X")
pf_data$species <- plot_naming(pf_data$species, "tag", "X")
pf_data$species <- plot_naming(pf_data$species, "MORE", "X")
pf_data$species <- plot_naming(pf_data$species, "MO", "X")
pf_data$species <- plot_naming(pf_data$species, "LUZSPP", "LUZARC")
pf_data$species <- plot_naming(pf_data$species, "SP1", "X")
pf_data$species <- plot_naming(pf_data$species, "litter__moss", "litter")
pf_data$species <- plot_naming(pf_data$species, "LICHENCRU__LICHENSPP", "LICHENCRU")
pf_data$species <- plot_naming(pf_data$species, "LX", "X")
pf_data$species <- plot_naming(pf_data$species, "LEMPOO", "X")
pf_data$species <- plot_naming(pf_data$species, "ERISPP", "ERITRI")
pf_data$species <- plot_naming(pf_data$species, "ARCLAT SALARC", "ARCLAT")
pf_data$species <- plot_naming(pf_data$species, "wood", "X")
pf_data$species <- plot_naming(pf_data$species, "ARCTIC", "X")
pf_data$species <- plot_naming(pf_data$species, "feather", "X")
pf_data$species <- plot_naming(pf_data$species, "WOOLYBEAR", "X")
pf_data$species <- plot_naming(pf_data$species, "excrament", "X")
pf_data$species <- plot_naming(pf_data$species, "sensor", "X")
pf_data$species <- plot_naming(pf_data$species, "GYNOPHERA", "X")
pf_data$species <- plot_naming(pf_data$species, "XXXUNK", "X")
pf_data$species <- plot_naming(pf_data$species, "pollen_trap", "X")
pf_data$species <- plot_naming(pf_data$species, "Stetson", "X")
pf_data$species <- plot_naming(pf_data$species, "mushroom", "X")
pf_data$species <- plot_naming(pf_data$species, "NOSTOC", "X")
pf_data$species <- plot_naming(pf_data$species, "Stetson", "X")

pf_data$species <- plot_naming(pf_data$species, "PERCAP", "PEDCAP")
pf_data$species <- plot_naming(pf_data$species, "CARSTA", "CARAQU")
pf_data$species <- plot_naming(pf_data$species, "EQUAVR", "EQUARV")
pf_data$species <- plot_naming(pf_data$species, "ERITRI", "ERIANG")
pf_data$species <- plot_naming(pf_data$species, "ERISCH", "ERIANG")

pf_data$species <- plot_naming(pf_data$species, "grass", "POASPP")
pf_data$species <- plot_naming(pf_data$species, "LICHENCRU", "LICHENSPP")
pf_data$species <- plot_naming(pf_data$species, "LICHENFOL", "LICHENSPP")
pf_data$species <- plot_naming(pf_data$species, "LICHENFRU", "LICHENSPP")

pf_data$species <- plot_naming(pf_data$species, "DRALAC", "DRASPP")
pf_data$species <- plot_naming(pf_data$species, "DRACIN", "DRASPP")
pf_data$species <- plot_naming(pf_data$species, "SILACU", "SILACA")


#################################################

# write out pf file here
write.csv(pf_data, file = "./data/cleaned_Alex_point_frame_data.csv")

#####################################   
#
#  Point frame species diversity analysis
#
#####################################

#packages
#if you have not installed some of these package before use: install.packages("pkgname")

# e.g. install.packages("vegan")
# e.g. install.packages("BiodiversityR")
# e.g. install.packages("car")
# etc.
# also agree to any other installations from CRAN

library(plyr)
library(vegan)
library(BiodiversityR) 
library(car)
library(ggplot2)

#import point frame data after removing blank rows (NA)
pfdata <- read.csv("./data/cleaned_Alex_point_frame_data_rmNAx.csv", header=TRUE)
#pfdata <- read.csv("./data/cleaned_Alex_point_frame_data_rmNAlitX.csv", header=TRUE)
pfdata <- pfdata[,-1]

#####################################
#
#  Change data to required format
#  separate species and site matrices
#
#####################################

#find all unique species names
specieslist <-  unique(pfdata$species)
specieslist <- as.vector(specieslist)

#create empty matrix
speciesmat <-  NULL

# for loop to determine presence or absence of each species at each order within each point
for (i in 1:length(specieslist)){
  X <- rep(0,nrow(pfdata))
  X[which(pfdata$species==specieslist[i])]=1
  speciesmat <- cbind(speciesmat,X)
}

#add species names as column names
colnames(speciesmat)=specieslist

#create dataframe of the presence or absence of each species
species.data <- speciesmat
site.data <- pfdata[,c("plot_id", "year", "site", "plot", "otc_treatment", "fert_treatment", "x", "y")]
df<- cbind(site.data, species.data)

#create empty matrix
aggspeciesmat <-  NULL

# for loop to determine presence or absence of each species at each point
for (i in 1:length(specieslist)){
  aggX <- aggregate(df[,(8+i)]~ plot_id+year+site+plot+otc_treatment+fert_treatment+x+y, data=df, "max", na.rm=FALSE)[9]
  aggspeciesmat <- cbind(aggspeciesmat, as.matrix(aggX))
}
colnames(aggspeciesmat)=specieslist

species.data <- aggspeciesmat

site.data <- aggregate(df[,9]~ plot_id+year+site+plot+otc_treatment+fert_treatment+x+y, data=df, "max", na.rm=FALSE)[1:8]

df2 <- cbind(site.data, species.data)
unique(site.data$year)

#-----------------------------------------------

#create empty matrix
aggspeciesmat1 <-  NULL

# for loop to determine presence or absence of each species at each point
for (i in 1:length(specieslist)){
  aggX <- aggregate(df2[,(8+i)] ~ plot_id+year+site+plot+otc_treatment+fert_treatment, data=df2, "sum", na.rm=FALSE)[7]
  aggspeciesmat1 <- cbind(aggspeciesmat1, as.matrix(aggX))
}

colnames(aggspeciesmat1)=specieslist

species.data <- aggspeciesmat1

df2$dfsum <- apply(df2[,9:58], 1, sum)

site.data <- aggregate(df2[,59]~  plot_id+year+site+plot+otc_treatment+fert_treatment, data=df2, "sum", na.rm=FALSE)[1:6]

#create mixed treatment of tree species and location
site.data$site.otc_treatment<- paste(site.data$site, site.data$otc_treatment, sep=".")
site.data$site.year <- paste(site.data$site, site.data$year, sep=".")
site.data$site.year.otc <- paste(site.data$site, site.data$year, site.data$otc_treatment, sep=".")
site.data$year.site.otc <- paste(site.data$year, site.data$site,site.data$otc_treatment,  sep=".")
site.data$year.otc <- paste(site.data$year,site.data$otc_treatment, sep=".")

#write files to csv
write.csv(species.data, file = "./data/Alex_pf_species_matrix.csv")
write.csv(site.data, file = "./data/Alex_pf_site_matrix.csv")

#####################################
#
# Preliminary data compiling
#
#####################################

# check all species seen at least 1 time
colSums(species.data)

#--------------------------------------
# remove species seen less than 20 times

#species.data = species.data[,colSums(species.data) > 20]

#-------------------------------------
#create dataframe of site and species data
df<- cbind(site.data, species.data)

#------------------------------------
# divide up by site

unique(df$site)
# "Dome"        "Dryas"       "Cassiope"    "Beach_Ridge" "Meadow"      "Willow"      "Fert"

df.Dome <- df[which(df$site == "Dome"),]
Dome.spp <- colSums(df.Dome[,12:ncol(df.Dome)])
species.Dome <- df.Dome[,(which(Dome.spp>0)+11)]

df.Dryas <- df[which(df$site == "Dryas"),]
Dryas.spp <- colSums(df.Dryas[,12:ncol(df.Dryas)])
species.Dryas <- df.Dryas[,(which(Dryas.spp>0)+11)]

df.Cassiope <- df[which(df$site == "Cassiope"),]
Cassiope.spp <- colSums(df.Cassiope[,12:ncol(df.Cassiope)])
species.Cassiope <- df.Cassiope[,(which(Cassiope.spp>0)+11)]

df.Beach_Ridge <- df[which(df$site== "Beach_Ridge"),]
Beach_Ridge.spp <- colSums(df.Beach_Ridge[,12:ncol(df.Beach_Ridge)])
species.Beach_Ridge <- df.Beach_Ridge[,(which(Beach_Ridge.spp>0)+11)]

df.Meadow <- df[which(df$site == "Meadow"),]
Meadow.spp <- colSums(df.Meadow[,12:ncol(df.Meadow)])
species.Meadow <- df.Meadow[,(which(Meadow.spp>0)+11)]

df.Willow <- df[which(df$site == "Willow"),]
Willow.spp <- colSums(df.Willow[,12:ncol(df.Willow)])
species.Willow <- df.Willow[,(which(Willow.spp>0)+11)]

df.Fert <- df[which(df$site == "Fert"),]
Fert.spp <- colSums(df.Fert[,12:ncol(df.Fert)])
species.Fert <- df.Fert[,(which(Fert.spp>0)+11)]

#---------------------------------
site.Dome <- df.Dome[,1:11]

site.Dryas <- df.Dryas[,1:11]

site.Cassiope <- df.Cassiope[,1:11]

site.Beach_Ridge <- df.Beach_Ridge[,1:11]

site.Meadow <- df.Meadow[,1:11]

site.Willow <- df.Willow[,1:11]

site.Fert <- df.Fert[,1:11]

#####################################
#
# Diversity indices {vegan}
#
#####################################

# calculate species diversity
diversity(species.data, index = "shannon")#this is the Shannon-Wiener index
diversity(species.data, index = "simpson")#this is the Simpson index
#fisher.alpha(species.data) #this is Fisher's alpha from the log-series distribution, fairly independent of sample size

site.data$shannon<-(diversity(species.data, index = "shannon"))#makes a new column in site data with the shannon values

# effects of year on shannon diversity
model1<-lm(shannon~year, data =site.data) 
#summary(lm(shannon~year, data =site.data))
anova(model1)

# effects of site on shannon diversity
model2<-lm(shannon~site, data =site.data) 
#summary(lm(shannon~site, data =site.data))
anova(model2)

# effects of otc_treatment on shannon diversity
model3<-lm(shannon~otc_treatment, data =site.data) 
#summary(lm(shannon~otc_treatment, data =site.data))
anova(model3)


# create graph of species diversity separated by X1 and coloured by X2
#plot(site.data$shannon ~ site.data$otc_treatment,  main= "Shannon Diversity Index changes with otc_treatment", xlab="otc_treatment", ylab="Shannon Diversity", pch=20)
#abline(model1) #adds the trend line

# ggplot(data=site.data, aes(x=site, y=shannon, colour=otc_treatment)) + geom_point(size=3)+
#   stat_smooth(method = "lm")#add the line

jpeg("./figures/Shannon_diversity_year_site.jpg", width = 856, height = 540)
ggplot(data=site.data, aes(x=year, y=shannon, colour=site)) + geom_point(size=3, position = "jitter")
dev.off()

jpeg("./figures/Shannon_diversity_site_year.jpg", width = 856, height = 540)
ggplot(data=site.data, aes(x=site, y=shannon, colour=year)) + geom_point(size=3, position = "jitter")
dev.off()

jpeg("./figures/Shannon_diversity_year_OTCcol.jpg", width = 856, height = 540)
ggplot(data=site.data, aes(x=year.otc, y=shannon, colour=otc_treatment)) + geom_point(size=3, position = "jitter")
dev.off()

jpeg("./figures/Shannon_diversity_site_OTC.jpg", width = 856, height = 540)
ggplot(data=site.data, aes(x=site, y=shannon, colour=otc_treatment)) + geom_point(size=3, position = "jitter")
dev.off()

jpeg("./figures/Shannon_diversity_year_site_OTC.jpg", width = 856, height = 540)
ggplot(data=site.data, aes(x=site.year.otc, y=shannon, colour=site)) + geom_point(size=3, position = "jitter")
dev.off()

jpeg("./figures/Shannon_diversity_site_OTC_yearcol.jpg", width = 856, height = 540)
ggplot(data=site.data, aes(x=site.otc_treatment, y=shannon, colour=year)) + geom_point(size=3, position = "jitter")
dev.off()

jpeg("./figures/Shannon_diversity_site_OTC_OTCcol.jpg", width = 856, height = 540)
ggplot(data=site.data, aes(x=site.otc_treatment, y=shannon, colour=otc_treatment)) + geom_point(size=3, position = "jitter")
dev.off()

jpeg("./figures/Shannon_diversity_year_site_OTCcol.jpg", width = 856, height = 540)
ggplot(data=site.data, aes(x=site.year.otc, y=shannon, colour=site.otc_treatment)) + geom_point(size=3, position = "jitter")
dev.off()

jpeg("./figures/Shannon_diversity_year_site_fert.jpg", width = 856, height = 540)
ggplot(data=site.data, aes(x=site.year, y=shannon, colour=fert_treatment)) + geom_point(size=3, position = "jitter")
dev.off()

#####################################
#
# Evenness index {vegan}
#
#####################################

PielouJ <- diversity(species.data, index = "shannon")/log(specnumber(species.data)) # Pielou's J
site.data <- cbind(site.data, PielouJ)

model4<-lm(PielouJ~site, data =site.data) 
anova(model4)

model5<-lm(PielouJ~site.year.otc, data =site.data) 
anova(model5)

#-----------------------------------
# plots
site_col <- as.numeric(unique(site.data$site))
jpeg("./figures/Shannon_diversity_boxplot_site.jpg", width = 856, height = 540)
boxplot(shannon~site, data=site.data, col="light blue", xlab="Site", ylab="Shannon Diversity Index", main="Shannon Diversity")
dev.off()

jpeg("./figures/Shannon_diversity_boxplot_site_otc.jpg", width = 856, height = 540)
boxplot(shannon~site.otc_treatment, data=site.data, col="light blue", xlab="site/otc_treatment", ylab="Shannon Diversity Index", main="Shannon Diversity")
dev.off()

jpeg("./figures/Shannon_diversity_boxplot_site_otc_year.jpg", width = 856, height = 540)
boxplot(shannon~site.year.otc, data=site.data, col="light blue", xlab="site/otc_treatment", ylab="Shannon Diversity Index", main="Shannon Diversity")
dev.off()

jpeg("./figures/Shannon_diversity_boxplot_fertilizer.jpg", width = 856, height = 540)
boxplot(shannon~fert_treatment+site, data=site.data, col="light blue", xlab="fertilizer", ylab="Shannon Diversity Index", main="Shannon Diversity")
dev.off()

jpeg("./figures/PielouJ_Evenness_boxplot_year_site.jpg", width = 856, height = 540)
boxplot(PielouJ~site.year, data=site.data, col="light blue", xlab="site_otc_year", ylab="Pielou's J Evenness", main="Pielou's J evenness")
dev.off()

#####################################
#
# Rarefaction {vegan}
#
#####################################

min(rowSums(species.data))
rarefy(species.data, 15) #if only 15 individuals had been sampled in each treatment, what would the diversity have been?

#####################################
#
# Species - Frequency plots, traditional for log series {vegan}
#
#####################################

dev.off()
par(mfrow=c(1,1)) 
plot(fisherfit(colSums(species.data))) #here is a plot of the number of species for each "bin" of abundances, abundances are summed over all sites

#####################################
#
# Rank- abundance plots, with log normal and other fits overlain {vegan}
#
#####################################
plot(rad.lognormal(colSums(species.data))) #sum abundances across all sites with colSums, fit log normal, then plot

jpeg("./figures/AIC_model_scores.jpg", width = 856, height = 540)
radlattice(radfit(colSums(species.data))) #other functions for rank-abundance, the lowest AIC is the best if >2 units lower than any other
dev.off()
#####################################
#
# Rank- abundance plots, the raw values linked with a line
# Requires BiodiversityR package
#
#####################################

RankAbun.1 <- rankabundance(species.data) 
RankAbun.1 # a data frame of the rank of each species

jpeg("./figures/rank_abundance_plot.jpg", width = 3000, height = 1500)
rankabunplot(RankAbun.1,scale='abundance', addit=FALSE, specnames=c(1:50)) #rank abundance plot, labelling the most common 3 species
dev.off()


# rank composition plots
site.data$otc_treatment <- as.factor(site.data$otc_treatment)
site.data$site <- as.factor(site.data$site)
site.data$year <- as.factor(site.data$year)
site.data$site.otc_treatment <- as.factor(site.data$site.otc_treatment)

rankabuncomp(species.data, y=site.data, factor=c('site'),scale='proportion', legend=TRUE, specnames=c(1:3)) #click on where on plot you want to have the legend

rankabuncomp(species.data, y=site.data, factor=c('year'),scale='proportion', legend=TRUE) #click on where on plot you want to have the legend

rankabuncomp(species.data, y=site.data, factor=c('site.otc_treatment'),scale='proportion', legend=TRUE) #click on where on plot you want to have the legend


#------------------------------------
# site specific plots needs to fix error

RankAbun.Dryas <- rankabundance(species.data[which(site.data$site=="Dryas"),]) 
RankAbun.Dryas # a dataframe of the rank of each species
RankAbun.Dryas1<-RankAbun.Dryas[-c(29:65),]
rankabunplot(RankAbun.Dryas1,scale='abundance', addit=FALSE, specnames=c(1:28)) #rank abudnance plot, labelling the most common 3 species

site.Dryas$year <- as.factor(site.Dryas$year)
site.Dryas$otc_treatment <- as.factor(site.Dryas$otc_treatment)
rankabuncomp(species.Dryas, y=site.Dryas, factor=c('otc_treatment'),scale='proportion', legend=TRUE) #click on where on plot you want to have the legend
rankabuncomp(species.Dryas, y=site.Dryas, factor=c('year'),scale='proportion', legend=TRUE) #click on where on plot you want to have the legend

site.Fert$year <- as.factor(site.Fert$year)
site.Fert$otc_treatment <- as.factor(site.Fert$otc_treatment)
rankabuncomp(species.Fert, y=site.Fert, factor=c('otc_treatment'),scale='proportion', legend=TRUE) #click on where on plot you want to have the legend
rankabuncomp(species.Fert, y=site.Fert, factor=c('year'),scale='proportion', legend=TRUE) #click on where on plot you want to have the legend

site.Cassiope$year <- as.factor(site.Cassiope$year)
site.Cassiope$otc_treatment <- as.factor(site.Cassiope$otc_treatment)
rankabuncomp(species.Cassiope, y=site.Cassiope, factor=c('otc_treatment'),scale='proportion', legend=TRUE) #click on where on plot you want to have the legend
rankabuncomp(species.Cassiope, y=site.Cassiope, factor=c('year'),scale='proportion', legend=TRUE) #click on where on plot you want to have the legend

#####################################
#
# Similarity matrices (vegan package required)
#
#####################################

dissim.mat<-vegdist(species.data, method="bray", binary=FALSE)

# try each site
# "Dome"        "Dryas"       "Cassiope"    "Beach_Ridge" "Meadow"      "Willow"      "Fert"

dissim.mat.Dome<-vegdist(species.Dome, method="bray", binary=FALSE)
dissim.mat.Dryas<-vegdist(species.Dryas, method="bray", binary=FALSE)
dissim.mat.Cassiope<-vegdist(species.Cassiope, method="bray", binary=FALSE)
dissim.mat.Beach_Ridge<-vegdist(species.Beach_Ridge, method="bray", binary=FALSE)
dissim.mat.Meadow<-vegdist(species.Meadow, method="bray", binary=FALSE)
dissim.mat.Willow<-vegdist(species.Willow, method="bray", binary=FALSE)
dissim.mat.Fert<-vegdist(species.Fert, method="bray", binary=FALSE)


#?vegdist #link to other dissimilarity metrics you could use in place of "bray"
#these include: "manhattan", "euclidean", "canberra", "bray", "kulczynski", "jaccard", "gower", "altGower", "morisita", "horn", "mountford", "raup" , "binomial", "chao", "cao" or "mahalanobis".

#dissim.mat<-vegdist(species.data, method="jaccard", binary=TRUE)
#dissim.mat

#####################################

# dissimilarity matrix based on CONTINUOUS site variables - none in point framing dataset
#site.mat<-vegdist(site.data$Temperature, method="euclidean") #need to add in column for actual temperature/soil moisture which is continuous
#site.mat

# do a Mantel test, which asks if the dissimilarity between pairs of sites in their species composition is related to that in their environment
#since any given site occurs in many pairs of sites, significance in Mantel tests is assessed via randomization (automatic)
#mantel(dissim.mat, site.mat, method="pearson", permutations=9999) 

#####################################
#
# Cluster analysis (no special package required)
#
#####################################

#cluster dendrogram showing how each of the unique plots are clustered 
fit <- hclust(dissim.mat, method="average") 
plot(fit)

plot(fit); rect.hclust(fit, h=0.5, border="red") # emphasize clusters <0.5 different


# try one site
fit.Dryas <- hclust(dissim.mat.Dryas, method="average") 
plot(fit.Dryas)

plot(fit.Dryas); rect.hclust(fit.Dryas, h=0.5, border="red") # emphasize clusters <0.5 different


#####################################
#
# Ordination: nMDS (requires vegan package)
#
#####################################

myNMDS<-metaMDS(species.data,k=2)
myNMDS #most important: is the stress low?
stressplot(myNMDS) #low stress means that the observed dissimilarity between site pairs matches that on the 2-D plot fairly well (points hug the line)

plot(myNMDS)#sites are open circles and species are red +'s 

#the following commands create layers on a plot, and should be run sequentially
ordiplot(myNMDS,type="n") #this clears the symbols from the plot
orditorp(myNMDS,display="species",col="red",air=0.01) #this adds red species names
#orditorp(myNMDS,display="sites",cex=0.75,air=0.01) #this adds black site labels, cex is the font size


# connect sites in the same treatment with a polygon use "ordihull" 
ordiplot(myNMDS,type="n") 
ordihull(myNMDS,groups=site.data$site,draw="polygon",col="grey90",label=T)
orditorp(myNMDS,display="species",col="red",air=0.01) 
orditorp(myNMDS,display="site",cex=0.75,air=0.01)

# link the sites within a treatment by lines
ordiplot(myNMDS,type="n") 
ordispider(myNMDS,groups=site.data$site.otc_treatment,spiders="centroid",col="black",label=F)
orditorp(myNMDS,display="species",col="red",air=0.01) 
orditorp(myNMDS,display="sites",cex=0.75,air=0.01)

#other plots
ordiplot(myNMDS,type="n") 
ordihull(myNMDS,groups=site.data$site,draw="polygon",col='blue',label=T)

ordiplot(myNMDS,type="n") 
ordihull(myNMDS,groups=site.data$Observer,draw="polygon",col='blue',label=T)

ordiplot(myNMDS,type="n") 
ordispider(myNMDS,groups=site.data$site,spiders="centroid",col="black",label=T)

ordiplot(myNMDS,type="n") 
ordihull(myNMDS,groups=site.data$site,draw="polygon",col='blue',label=T)

#####################################
# Dryas

myNMDS<-metaMDS(species.Dryas,k=2)
myNMDS #most important: is the stress low?
stressplot(myNMDS) #low stress means that the observed dissimilarity between site pairs matches that on the 2-D plot fairly well (points hug the line)

plot(myNMDS)#sites are open circles and species are red +'s 

#the following commands create layers on a plot, and should be run sequentially
ordiplot(myNMDS,type="n") #this clears the symbols from the plot
orditorp(myNMDS,display="species",col="red",air=0.01) #this adds red species names
orditorp(myNMDS,display="sites",cex=0.75,air=0.01) #this adds black site labels, cex is the font size


# connect sites in the same treatment with a polygon use "ordihull" 
ordiplot(myNMDS,type="n") 
ordihull(myNMDS,groups=site.Dryas$otc,draw="polygon",col="grey90",label=T)
orditorp(myNMDS,display="species",col="red",air=0.01) 
#orditorp(myNMDS,display="site",cex=0.75,air=0.01)

#####################################
# Fertilizer

myNMDS<-metaMDS(species.Fert,k=2)
myNMDS #most important: is the stress low?
stressplot(myNMDS) #low stress means that the observed dissimilarity between site pairs matches that on the 2-D plot fairly well (points hug the line)

plot(myNMDS)#sites are open circles and species are red +'s 

#the following commands create layers on a plot, and should be run sequentially
ordiplot(myNMDS,type="n") #this clears the symbols from the plot
orditorp(myNMDS,display="species",col="red",air=0.01) #this adds red species names
orditorp(myNMDS,display="sites",cex=0.75,air=0.01) #this adds black site labels, cex is the font size


# connect sites in the same treatment with a polygon use "ordihull" 
ordiplot(myNMDS,type="n") 
ordihull(myNMDS,groups=site.Fert$fert_treatment,draw="polygon",col="grey90",label=T)
orditorp(myNMDS,display="species",col="red",air=0.01) 
#orditorp(myNMDS,display="site",cex=0.75,air=0.01)

#####################################
#
# Permutational analysis of variance for multivariate data {vegan}
#
#####################################

adonis2(species.data ~ site*otc_treatment, data=site.data, permutations=9999)
adonis2(species.data ~ plot*Observer*DATE, data=site.data, permutations=9999)

#####################################
#
# Recommended online resources
#  https://cran.r-project.org/web/packages/vegan/vignettes/diversity-vegan.pdf
#  https://cran.r-project.org/web/packages/vegan/vignettes/intro-vegan.pdf
#  http://ecology.msu.montana.edu/labdsv/R/labs/lab13/lab13.html  
#
#####################################