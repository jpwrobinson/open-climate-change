

library(ggplot2)
library(grid)
library(gridExtra)
library(plyr)
library(lme4)
library(visreg)
library(reshape)

## clean data and merge with journal metric data

rm(list=ls())
setwd("~/GitHub/open-climate-change/")
setwd('/Users/robins64/Documents/git_repos/open-climate-change')

## load data
load("./Data/scopus_OA_climate_clean.Rdata")  ## scopus data filtered by Jimmy - journals with > 200 papers in last 10 years
full.scop<-read.csv("./Data/ScopusOAData_20180214TT.csv",stringsAsFactors = F)

head(scop)
head(full.scop)

## choose filtered data or full data
dat<-scop
dat$Cited.by[which(is.na(dat$Cited.by))]<-0

jour.dat<-dat[,c("Source.title","X2016.CiteScore","X2016.SJR","X2016.SNIP","Journal.Open.Access")]
jour.dat<-jour.dat[-which(duplicated(jour.dat)),]

## bin for journal rankings
#Jour.Var<-"X2016.CiteScore"  ## Choose metric: "X2016.CiteScore" or "X2016.SJR" or "X2016.SNIP"
Jour.Var<-"X2016.SJR"     ## Use this
#Jour.Var<-"X2016.SNIP"

bins<-quantile(jour.dat[,Jour.Var],na.rm=T)  ##quantile bins

## write out journal list with bins
jour.dat$bin<-cut(jour.dat[,Jour.Var],breaks = bins,labels = LETTERS[1:(length(bins)-1)])
jour.dat$bin[which(is.na(jour.dat$bin))]<-"A"

## article journal bins
dat$jour.bin<-cut(dat[,Jour.Var],breaks = bins,labels = LETTERS[1:(length(bins)-1)])
dat$jour.bin[which(is.na(dat$jour.bin))]<-"A"



####### MODEL FIT TO AGGREGATED BY JOURNAL ########
names(dat)
sub.dat<-dat[,c("Source.title","Year","Cited.by","OA","Open.Access","jour.bin")]
mod.dat<-ddply(sub.dat,.(Source.title,Year,Open.Access,OA,jour.bin),summarize,
               MeanCite = mean(Cited.by,na.rm=T))


fit5a<-lmer(MeanCite ~ jour.bin*OA + (1|Year),data=mod.dat)
summary(fit5a)
hist(resid(fit5a))

fit5b<-glmer(MeanCite ~ jour.bin*OA + (1|Year),data=mod.dat,family="poisson")
quartz()
hist(resid(fit5b))

## save model output
save(fit5b, file='./Data/scopus_glmerfit.Rdata')


scop.dat<-expand.grid(OA = unique(mod.dat$OA), jour.bin=unique(mod.dat$jour.bin), year = 2008, Journal='Ecology')
scop.dat$p<-predict(fit5b, newdat=scop.dat, re.form=NA, type='response')

quartz()
ggplot(scop.dat,aes(x=jour.bin,y=p,shape=OA)) + geom_point()










