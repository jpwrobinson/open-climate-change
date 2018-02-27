

####################
#
# Open access climate change papers 
#
# 2018/02/05
#
####################


library(ggplot2)
library(grid)
library(gridExtra)
library(plyr)

## clean data and merge with journal metric data

rm(list=ls())
setwd("~/GitHub/open-climate-change/")
setwd('/Users/robins64/Documents/git_repos/open-climate-change')

## load data
load("./Data/scopus_OA_climate_clean.Rdata")  ## scopus data filtered by Jimmy - top 30 journals
dat<-read.csv("./Data/ScopusOAData_20180214TT.csv",na.strings=F)  ## cleaned data

head(scop)
head(dat)









## EXTRA STUFF NOT EDITED
dat<-dat[which(dat$X2016.CiteScore <= median(j.dat$X2016.CiteScore,na.rm=T)),]

dat<-dat[which(dat$X2016.CiteScore <= 40),]

summary(j.dat$X2016.CiteScore,na.rm=T)



sum.dat<-ddply(dat,.(Access.Type,Year),summarize,
               CiteAve = mean(Cited.by,na.rm=T),
               Citevar = var(Cited.by,na.rm=T),
               CiteN = length(Cited.by))

tdat<-ddply(dat,.(Year),summarize,
            NTotal = length(Cited.by))

sum.dat<-merge(sum.dat,tdat,by.x="Year",by.y="Year",all.x=T)
sum.dat$PropOA<-sum.dat$CiteN/sum.dat$NTotal*100






str(dat)

fit1<-lm(Cited.by~as.factor(Year)*Access.Type,data=dat)
summary(fit1)
anova(fit1)

fit1<-lm(Cited.by~Year*Access.Type,data=dat)
summary(fit1)
anova(fit1)


fit2<-lm(Cited.by~Access.Type,data=dat)
summary(fit2)
anova(fit2)

fit3<-lm(Cited.by~as.factor(Year)+Access.Type,data=dat)
summary(fit3)
anova(fit3)


















