##TJ Haltigan Thesis data script
#Richard Borthwick, 10-17-2018

#Step one, load some libraries.

library(vegan)
library(readxl)
library(tidyverse)
library(dplyr)
library(lme4)


#load some data:
tj_data <- read_excel("C:/Users/richard/Downloads/BNF20132018.xlsx",sheet="Sheet1")
View(tj_data)#note that this has no categorical information (no site#, not treatment, no year)

#Dat needs to be formatted so that we can consider diversity, richness, and eveness by treatment and maybe year
tj_data$yr <- strftime(tj_data$Date, "%Y")##create year variable for model incase year is to be used
names(tj_data)
head(tj_data)
##Drop all data not immediately necessary:
tj_data<-tj_data[,c(3:5,7)]

#see what you're dealing with:
head(tj_data)
class(tj_data$Species)
#make factors for R's convenience:
tj_data$Species<-as.factor(tj_data$Species)
tj_data$Treatment<-as.factor(tj_data$Treatment)
tj_data$Block<-as.factor(tj_data$Block)
tj_data$yr<-as.factor(tj_data$yr)
#table them
tj = count(tj_data, c('yr', 'Block','Treatment','Species'))
#remove NA
tj<-na.omit(tj)
tj.2<-reshape(tj, idvar = c("yr","Block","Treatment"), timevar = "Species", direction = "wide")
tj.2[is.na(tj.2)] <- 0

head(tj.2)
names(tj.2)
#calculate diversity
div<-diversity(tj.2[,4:41])
rich<-specnumber(tj.2[,4:41])
even<-div/log(rich)

##combine to new dataset:

tj.3<-as.data.frame(cbind(tj.2$Treatment,div,rich,even))
tj.3$V1<-as.factor(tj.3$V1)

##run an analysis:

m1<-aov(div~V1,data=tj.3)
m2<-aov(rich~V1,data=tj.3)
m3<-aov(even~V1,data=tj.3)

summary(m1)
summary(m2)
summary(m3)

#no sig values based on this subset of data.