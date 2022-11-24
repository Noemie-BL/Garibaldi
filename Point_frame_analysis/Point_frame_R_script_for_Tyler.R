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
pathname <- "/home/celphin/projects/def-henryg/Garibaldi_Lake_data_summer2022/celphin_point_frame_workingdir/output/"
setwd("~/GitHub/Garibaldi/Point_frame_analysis")

# Split filename column into different factors and change column headers before import

pf_data <- read.table("./Point_frame_data/pf_compiled_final.csv", header=TRUE, sep =",", dec = ".")

###################################################
# clean up the compiled file
# check all species, site and tissue names

plot_naming <- function(data_column, orig_name1, new_name1){
  data_column <- as.character(data_column)
  data_column[which(data_column==orig_name1)] <- new_name1
  data_column
}

colnames(pf_data)

unique(pf_data$filename)       
unique(pf_data$X)                
unique(pf_data$Y)                
unique(pf_data$HitOrder)
unique(pf_data$SPP)              
unique(pf_data$STATUS)           
unique(pf_data$TISSUE)           
unique(pf_data$CanopyHeight.mm.)
unique(pf_data$Extra.sp)         
unique(pf_data$SITE)             
unique(pf_data$PLOT)             
unique(pf_data$TRTMT)
unique(pf_data$Observer)         
unique(pf_data$DATE)             
unique(pf_data$TOMST)            
unique(pf_data$Flag)
unique(pf_data$Flag.1)

##############################################

unique(pf_data$filename)

unique(pf_data$X)

unique(pf_data$Y)

unique(pf_data$HitOrder)

unique(pf_data$SPP)

unique(pf_data$STATUS)

unique(pf_data$Extra.sp)

unique(pf_data$SITE)

unique(pf_data$PLOT)

unique(pf_data$TRTMT)

unique(pf_data$Observer)

unique(pf_data$DATE)

unique(pf_data$TOMST)

unique(pf_data$Flag)


############################################################

unique(pf_data$SITE)
#[1] "salix"    "mead"     "Meadow"   "Cassiope" "cassiope"

pf_data$SITE <- plot_naming(pf_data$SITE, "mead", "Meadow")
pf_data$SITE <- plot_naming(pf_data$SITE, "salix", "Salix")
pf_data$SITE <- plot_naming(pf_data$SITE, "cassiope", "Cassiope")

unique(pf_data$SITE)

#-------------------------------------------

unique(pf_data$TRTMT)
#[1] "control" "warming" "c"       "w"       "W"       "C"

pf_data$TRTMT <- plot_naming(pf_data$TRTMT, "w", "warming")
pf_data$TRTMT <- plot_naming(pf_data$TRTMT, "c", "control")
pf_data$TRTMT <- plot_naming(pf_data$TRTMT, "W", "warming")
pf_data$TRTMT <- plot_naming(pf_data$TRTMT, "C", "control")

unique(pf_data$TRTMT)

#-------------------------------------------

unique(pf_data$STATUS)
#[1] ""   "sd" "l"  "ad" "lf" "SD" "L"  "d"  "1"

pf_data$STATUS <- plot_naming(pf_data$STATUS, "l", "live")
pf_data$STATUS <- plot_naming(pf_data$STATUS, "lf", "live")
pf_data$STATUS <- plot_naming(pf_data$STATUS, "L", "live")
pf_data$STATUS <- plot_naming(pf_data$STATUS, "1", "live")
pf_data$STATUS <- plot_naming(pf_data$STATUS, "ad", "attached_dead")
pf_data$STATUS <- plot_naming(pf_data$STATUS, "sd", "standing_dead")
pf_data$STATUS <- plot_naming(pf_data$STATUS, "SD", "standing_dead")
pf_data$STATUS <- plot_naming(pf_data$STATUS, "d", "standing_dead")

unique(pf_data$STATUS)

#---------------------------------------------
unique(pf_data$SPP)
# [1] "x"               "caraur"          "litter"          "soil"
# [5] "triglu"          "furry salix"     "p2 soil sample"  ""
# [9] "carspp"          "equvar"          "na"              "rock"
# [13] "tomst"           "soil p1"         "tag"             "moss"
# [17] "kalmic"          "carapp"          "caruar"          "furry salix "
# [21] " x"              "lichen"          "x "              "caraqu"
# [25] "equarv"          "junmer"          "junarc"          "0"
# [29] "casmer"          "phygla"          "pinvul"          "jundru"
# [33] "galhul"          "lupspp"          "MOSS"            "n/a"
# [37] "antalp"          "triglu (?) pink" "salix hairy"     " equvar"
# [41] "salix gla"       "casspp"          "equvsr"          "kalmi c"
# [45] "carnig"          "salixgla"        "naa"             "root core p3"
# [49] "root core t2"    "galhum"          "lit"             "sd"
# [53] "toms"            "carmer"          "salbar"          "sol"
# [57] "eqivar"          "camera marker"   "tea bag tag"     "?"
# [61] "salix-hairy"


pf_data$SPP <- plot_naming(pf_data$SPP, "lit", "litter")
pf_data$SPP <- plot_naming(pf_data$SPP, " x", "x")
pf_data$SPP <- plot_naming(pf_data$SPP, "x ", "x")

pf_data$SPP <- plot_naming(pf_data$SPP, "", "NA")
pf_data$SPP <- plot_naming(pf_data$SPP, "n/a", "NA")
pf_data$SPP <- plot_naming(pf_data$SPP, "na", "NA")
pf_data$SPP <- plot_naming(pf_data$SPP, "0", "NA")
pf_data$SPP <- plot_naming(pf_data$SPP, "naa", "NA")

pf_data$SPP <- plot_naming(pf_data$SPP, "toms", "tomst")
pf_data$SPP <- plot_naming(pf_data$SPP, "furry salix", "salbar")
pf_data$SPP <- plot_naming(pf_data$SPP, "furry salix ", "salbar")
pf_data$SPP <- plot_naming(pf_data$SPP, "salix-hairy", "salbar")
pf_data$SPP <- plot_naming(pf_data$SPP, "salix hairy", "salbar")
pf_data$SPP <- plot_naming(pf_data$SPP, "MOSS", "moss")
pf_data$SPP <- plot_naming(pf_data$SPP, " equvar", "equvar")
pf_data$SPP <- plot_naming(pf_data$SPP, "eqivar", "equvar")
pf_data$SPP <- plot_naming(pf_data$SPP, "equvsr", "equvar")

pf_data$SPP_sub <- plot_naming(pf_data$SPP, "tomst", "other")
pf_data$SPP_sub <- plot_naming(pf_data$SPP_sub, "soil p1", "other")
pf_data$SPP_sub <- plot_naming(pf_data$SPP_sub, "tag", "other")
pf_data$SPP_sub <- plot_naming(pf_data$SPP_sub, "p2 soil sample", "other")
pf_data$SPP_sub <- plot_naming(pf_data$SPP_sub, "camera marker", "other")
pf_data$SPP_sub <- plot_naming(pf_data$SPP_sub, "?", "other")
pf_data$SPP_sub <- plot_naming(pf_data$SPP_sub, "tea bag tag", "other")
pf_data$SPP_sub <- plot_naming(pf_data$SPP_sub, "sol", "other")
pf_data$SPP_sub <- plot_naming(pf_data$SPP_sub, "root core p3", "other")
pf_data$SPP_sub <- plot_naming(pf_data$SPP_sub, "root core t2", "other")

pf_data$SPP_sub <- plot_naming(pf_data$SPP_sub, "kalmi c", "kalmic")
pf_data$SPP_sub <- plot_naming(pf_data$SPP_sub, "caruar", "caraur")
pf_data$SPP_sub <- plot_naming(pf_data$SPP_sub, "triglu (?) pink", "triglu")


pf_data$SPP_sub <- plot_naming(pf_data$SPP_sub, "casspp", "casmer")
pf_data$SPP_sub <- plot_naming(pf_data$SPP_sub, "salixgla", "salgla")
pf_data$SPP_sub <- plot_naming(pf_data$SPP_sub, "salix gla", "salgla")
pf_data$SPP_sub <- plot_naming(pf_data$SPP_sub, "sd", "other")
pf_data$SPP_sub <- plot_naming(pf_data$SPP_sub, "NA", "other")
pf_data$SPP_sub <- plot_naming(pf_data$SPP_sub, "galhul", "galhum")
pf_data$SPP_sub <- plot_naming(pf_data$SPP_sub, "carmer", "casmer")
pf_data$SPP_sub <- plot_naming(pf_data$SPP_sub, "carapp", "casspp")

unique(pf_data$SPP_sub)

# [1] "x"      "caraur" "litter" "soil"   "triglu" "salbar" "other"  "NA"
# [9] "carspp" "equvar" "rock"   "moss"   "kalmic" "carapp" "lichen" "caraqu"
# [17] "equarv" "junmer" "junarc" "casmer" "phygla" "pinvul" "jundru" "galhul"
# [25] "lupspp" "antalp" "salgla" "carnig" "galhum" "?"      "carmer"

pf_data$SPP <- pf_data$SPP_sub

#---------------------------------------
unique(pf_data$TISSUE)
# [1] ""            "leaf"        "flower"      "stem"        "flower_stem"
# [6] "fl"          "stem "       "f"           "sflst"       "FL"
# [11] "LF"          "ls"

pf_data$TISSUE <- plot_naming(pf_data$TISSUE, "lf", "leaf")
pf_data$TISSUE <- plot_naming(pf_data$TISSUE, "LF", "leaf")
pf_data$TISSUE <- plot_naming(pf_data$TISSUE, "ls", "leaf")
pf_data$TISSUE <- plot_naming(pf_data$TISSUE, "st", "stem")
pf_data$TISSUE <- plot_naming(pf_data$TISSUE, "stem ", "stem")
pf_data$TISSUE <- plot_naming(pf_data$TISSUE, "flwr", "flower")
pf_data$TISSUE <- plot_naming(pf_data$TISSUE, "f", "flower")
pf_data$TISSUE <- plot_naming(pf_data$TISSUE, "fl", "flower")
pf_data$TISSUE <- plot_naming(pf_data$TISSUE, "FL", "flower")
pf_data$TISSUE <- plot_naming(pf_data$TISSUE, "flst", "flower_stem")
pf_data$TISSUE <- plot_naming(pf_data$TISSUE, "sflst", "flower_stem")

unique(pf_data$TISSUE)

#----------------------------------------------
unique(pf_data$PLOT)
# [1] "1"   "2"   "3"   "4"   "5"   "6"   "7"   "8"   "22"  "10W" "11c" "12W"
# [13] "13C" "14W" "15C" "16w" "17c" "18w" "19c" "20w" "21c" "23c" "24w" "9C"

pf_data$PLOT <- plot_naming(pf_data$PLOT, "9C", "9")
pf_data$PLOT <- plot_naming(pf_data$PLOT, "10W", "10")
pf_data$PLOT <- plot_naming(pf_data$PLOT, "11c", "11")
pf_data$PLOT <- plot_naming(pf_data$PLOT, "12W", "12")
pf_data$PLOT <- plot_naming(pf_data$PLOT, "13C", "13")
pf_data$PLOT <- plot_naming(pf_data$PLOT, "14W", "14")
pf_data$PLOT <- plot_naming(pf_data$PLOT, "15C", "15")
pf_data$PLOT <- plot_naming(pf_data$PLOT, "16w", "16")
pf_data$PLOT <- plot_naming(pf_data$PLOT, "17c", "17")
pf_data$PLOT <- plot_naming(pf_data$PLOT, "18w", "18")
pf_data$PLOT <- plot_naming(pf_data$PLOT, "19c", "19")
pf_data$PLOT <- plot_naming(pf_data$PLOT, "20w", "20")
pf_data$PLOT <- plot_naming(pf_data$PLOT, "21c", "21")
pf_data$PLOT <- plot_naming(pf_data$PLOT, "22w", "22")
pf_data$PLOT <- plot_naming(pf_data$PLOT, "23c", "23")
pf_data$PLOT <- plot_naming(pf_data$PLOT, "24w", "24")

unique(pf_data$PLOT)



#----------------------------------------------
# this would be good to clean up
unique(pf_data$Observer)



###########################################################################

# write out pf file here

write.csv(pf_data, file = "./Point_frame_data/cleaned_Garibaldi_point_frame_data.csv")


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

library(vegan)
library(BiodiversityR) 
library(car)
library(ggplot2)

#import point frame data
pfdata <- read.csv("./Point_frame_data/cleaned_Garibaldi_point_frame_data.csv", header=TRUE)

pfdata <- pfdata[,-1]

#####################################
#
#  Change data to required format
#  separate species and site matrices
#
#####################################

#find all unique species names
SPPlist <-  unique(pfdata$SPP)
SPPlist <- as.vector(SPPlist)

#create empty matrix
sppmat <-  NULL

# for loop to determine presence or absence of each species at each order within each point
for (i in 1:length(SPPlist)){
  X <- rep(0,nrow(pfdata))
  X[which(pfdata$SPP==SPPlist[i])]=1
  sppmat <- cbind(sppmat,X)
}

#add species names as column names
colnames(sppmat)=SPPlist

#create dataframe of the presence or absence of each species
species.data <- sppmat
site.data <- pfdata[,c(1:3,10:14)]
df<- cbind(site.data, species.data)

#create empty matrix
aggsppmat <-  NULL

# for loop to determine presence or absence of each species at each point
for (i in 1:length(SPPlist)){
  aggX <- aggregate(df[,(8+i)]~ filename+Y+X+PLOT+TRTMT+SITE+Observer+DATE, data=df, "max")[9]
  aggsppmat <- cbind(aggsppmat, as.matrix(aggX))
}
colnames(aggsppmat)=SPPlist

species.data <- aggsppmat
site.data <- aggregate(df[,(8+1)]~ filename+Y+X+PLOT+TRTMT+SITE+Observer+DATE, data=df, "max", na.rm=FALSE)[1:8]
df2 <- cbind(site.data, species.data)

#-----------------------------------------------

#create empty matrix
aggsppmat1 <-  NULL

# for loop to determine presence or absence of each species at each point
for (i in 1:length(SPPlist)){
  aggX <- aggregate(df2[,(8+i)]~ filename+PLOT+TRTMT+SITE+Observer+DATE, data=df2, "sum")[7]
  aggsppmat1 <- cbind(aggsppmat1, as.matrix(aggX))
}

colnames(aggsppmat1)=SPPlist

species.data <- aggsppmat1
site.data <- aggregate(df2[,(8+1)]~ filename+PLOT+TRTMT+SITE+Observer+DATE, data=df2, "sum", na.rm=FALSE)[1:6]

#create mixed treatment of tree species and location
site.data$SITE.TRTMT<- paste(site.data$SITE, site.data$TRTMT, sep=".")

#write files to csv
write.csv(species.data, file = "garibaldi_pf_species_matrix.csv")
write.csv(site.data, file = "garibaldi_pf_site_matrix.csv")

#####################################
#
# Preliminary data analysis / graphing
#
#####################################

#create dataframe of site and species data
df<- cbind(site.data, species.data)

# calculate species diversity
diversity(species.data, index = "shannon")#this is the Shannon-Wiener index
diversity(species.data, index = "simpson")#this is the Simpson index
fisher.alpha(species.data) #this is Fisher's alpha from the log-series distribution, fairly independent of sample size

site.data$shannon<-(diversity(species.data, index = "shannon"))#makes a new column in site data with the shannon values

# effects of OBSERVER on shannon diversity
model1<-lm(shannon~Observer, data =site.data)
#summary(lm(shannon~Observer, data =site.data))
anova(model1)

# effects of Site on shannon diversity
model2<-lm(shannon~SITE, data =site.data) 
#summary(lm(shannon~SITE, data =site.data))
anova(model2)

# effects of TRTMT on shannon diversity
model3<-lm(shannon~TRTMT, data =site.data) 
#summary(lm(shannon~TRTMT, data =site.data))
anova(model3)

# create graph of species diversity separated by X1 and coloured by X2
#plot(site.data$shannon ~ site.data$TRTMT,  main= "Shannon Diversity Index changes with TRTMT", xlab="TRTMT", ylab="Shannon Diversity", pch=20)
#abline(model1) #adds the trend line

# ggplot(data=site.data, aes(x=SITE, y=shannon, colour=TRTMT)) + geom_point(size=3)+
#   stat_smooth(method = "lm")#add the line

ggplot(data=site.data, aes(x=SITE, y=shannon, colour=TRTMT)) + geom_point(size=3)+
  stat_smooth(method = "lm")#add the line

ggplot(data=site.data, aes(x=SITE, y=shannon, colour=Observer)) + geom_point(size=3)+
  stat_smooth(method = "lm")#add the line

ggplot(data=site.data, aes(x=TRTMT, y=shannon, colour=SITE)) + geom_point(size=3)+
  stat_smooth(method = "lm")#add the line

# example of how to save a jpeg image in R
jpeg("./Point_frame_figures/Shannon_diversity_OTC_site.jpg", width = 856, height = 540)
ggplot(data=site.data, aes(x=TRTMT, y=shannon, colour=SITE)) + geom_point(size=3, position = "jitter")
dev.off()


#####################################
#
# Diversity indices {vegan}
#
#####################################

diversity(species.data, index = "shannon")# Shannon-Wiener index
diversity(species.data, index = "simpson")# Simpson index
fisher.alpha(species.data) #Fisher's alpha from the log-series distribution, fairly independent of sample size

#####################################
#
# Evenness index {vegan}
#
#####################################

PielouJ <- diversity(species.data, index = "shannon")/log(specnumber(species.data)) # Pielou's J
site.data <- cbind(site.data, PielouJ)

model5<-lm(PielouJ~SITE, data =site.data) 
anova(model5)
model5<-lm(PielouJ~TRTMT, data =site.data) 
anova(model5)

boxplot(shannon~SITE, data=site.data, col="light blue", xlab="SITE", ylab="Shannon Diversity Index", main="Shannon Diversity")
boxplot(shannon~SITE.TRTMT, data=site.data, col="light blue", xlab="Site/TRTMT", ylab="Shannon Diversity Index", main="Shannon Diversity")
boxplot(PielouJ~SITE.TRTMT, data=site.data, col="light blue", xlab="TRTMT", ylab="Pielou's J Evenness", main="Pielou's J evenness")

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

radlattice(radfit(colSums(species.data))) #other functions for rank-abundance, the lowest AIC is the best if >2 units lower than any other

#####################################
#
# Rank- abundance plots, the raw values linked with a line
# Requires BiodiversityR package
#
#####################################

RankAbun.1 <- rankabundance(species.data) 
RankAbun.1 # a dataframe of the rank of each species
rankabunplot(RankAbun.1,scale='abundance', addit=FALSE, specnames=c(1:31)) #rank abudnance plot, labelling the most common 3 species

site.data$TRTMT <- as.factor(site.data$TRTMT)
site.data$SITE <- as.factor(site.data$SITE)
site.data$Observer <- as.factor(site.data$Observer)
site.data$SITE.TRTMT <- as.factor(site.data$SITE.TRTMT)

rankabuncomp(species.data, y=site.data, factor=c('TRTMT'),scale='proportion', legend=TRUE) #click on where on plot you want to have the legend
rankabuncomp(species.data, y=site.data, factor=c('SITE'),scale='proportion', legend=TRUE, specnames=c(1:3)) #click on where on plot you want to have the legend
rankabuncomp(species.data, y=site.data, factor=c('Observer'),scale='proportion', legend=TRUE) #click on where on plot you want to have the legend
rankabuncomp(species.data, y=site.data, factor=c('SITE.TRTMT'),scale='proportion', legend=TRUE) #click on where on plot you want to have the legend

#------------------------------------
RankAbun.SAL <- rankabundance(species.data[which(site.data$SITE=="Salix"),]) 
RankAbun.SAL # a dataframe of the rank of each species
rankabunplot(RankAbun.SAL,scale='abundance', addit=FALSE, specnames=c(1:12)) #rank abudnance plot, labelling the most common 3 species

RankAbun.CASS <- rankabundance(species.data[which(site.data$SITE=="Cassiope"),]) 
RankAbun.CASS # a dataframe of the rank of each species
rankabunplot(RankAbun.CASS,scale='abundance', addit=FALSE, specnames=c(1:22)) #rank abudnance plot, labelling the most common 3 species

RankAbun.Mead <- rankabundance(species.data[which(site.data$SITE=="Meadow"),]) 
RankAbun.Mead # a dataframe of the rank of each species
rankabunplot(RankAbun.Mead,scale='abundance', addit=FALSE, specnames=c(1:14)) #rank abudnance plot, labelling the most common 3 species

#####################################
#
# Similarity matrices (vegan package required)
#
#####################################

dissim.mat<-vegdist(species.data, method="bray", binary=FALSE)
#dissim.mat

#?vegdist #link to other dissimilarity metrics you could use in place of "bray"
#these include: "manhattan", "euclidean", "canberra", "bray", "kulczynski", "jaccard", "gower", "altGower", "morisita", "horn", "mountford", "raup" , "binomial", "chao", "cao" or "mahalanobis".

dissim.mat<-vegdist(species.data, method="jaccard", binary=TRUE)
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

#cluster dendrogram showing how each of the 24 species are clustered 
fit <- hclust(dissim.mat, method="average") 
plot(fit)

plot(fit); rect.hclust(fit, h=0.5, border="red") # emphasize clusters <0.5 different

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
orditorp(myNMDS,display="sites",cex=0.75,air=0.01) #this adds black site labels, cex is the font size

# connect sites in the same treatment with a polygon use "ordihull" 
ordiplot(myNMDS,type="n") 
ordihull(myNMDS,groups=site.data$SITE,draw="polygon",col="grey90",label=T)
orditorp(myNMDS,display="species",col="red",air=0.01) 
orditorp(myNMDS,display="sites",cex=0.75,air=0.01)

# link the sites within a treatment by lines
ordiplot(myNMDS,type="n") 
ordispider(myNMDS,groups=site.data$SITE.TRTMT,spiders="centroid",col="black",label=F)
orditorp(myNMDS,display="species",col="red",air=0.01) 
orditorp(myNMDS,display="sites",cex=0.75,air=0.01)

#other plots
ordiplot(myNMDS,type="n") 
ordihull(myNMDS,groups=site.data$SITE,draw="polygon",col='blue',label=T)

ordiplot(myNMDS,type="n") 
ordihull(myNMDS,groups=site.data$Observer,draw="polygon",col='blue',label=T)

ordiplot(myNMDS,type="n") 
ordispider(myNMDS,groups=site.data$SITE,spiders="centroid",col="black",label=T)

ordiplot(myNMDS,type="n") 
ordihull(myNMDS,groups=site.data$SITE,draw="polygon",col='blue',label=T)

#####################################
#
# Permutational analysis of variance for multivariate data {vegan}
#
#####################################

adonis(species.data ~ SITE*TRTMT, data=site.data, permutations=9999)
adonis(species.data ~ PLOT*Observer*DATE, data=site.data, permutations=9999)

#####################################
#
# Recommended online resources
#  https://cran.r-project.org/web/packages/vegan/vignettes/diversity-vegan.pdf
#  https://cran.r-project.org/web/packages/vegan/vignettes/intro-vegan.pdf
#  http://ecology.msu.montana.edu/labdsv/R/labs/lab13/lab13.html  
#
#####################################