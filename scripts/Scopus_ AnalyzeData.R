

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
library(lme4)
library(visreg)

## clean data and merge with journal metric data

rm(list=ls())
setwd("~/GitHub/open-climate-change/")
setwd('/Users/robins64/Documents/git_repos/open-climate-change')

## load data
load("./Data/scopus_OA_climate_clean.Rdata")  ## scopus data filtered by Jimmy - journals with > 300 papers in last 10 years
full.scop<-read.csv("./Data/ScopusOAData_20180214TT.csv",stringsAsFactors = F)

head(scop)
head(full.scop)

## choose filtered data or full data
dat<-scop
dat$Cited.by[which(is.na(dat$Cited.by))]<-0

#nrow(dat[which(dat$Cited.by==0),])
#dat<-dat[which(dat$Cited.by>0),]


jour.dat<-dat[,c("Source.title","X2016.CiteScore","X2016.SJR","X2016.SNIP","Journal.Open.Access")]
jour.dat<-jour.dat[-which(duplicated(jour.dat)),]


## plot data
p1<-ggplot(dat)
p1 + geom_boxplot(aes(x=as.factor(Year), y=log(Cited.by+1), colour=Open.Access))
p1 + geom_boxplot(aes(x=as.factor(Year), y=log(Cited.by+1), colour=Open.Access),
                  outlier.shape=NA)



####################################
#
# Analyze all data

## summarize all data
sum.dat<-ddply(dat,.(Open.Access,Year),summarize,
               CiteAve = mean(Cited.by,na.rm=T),
               CiteVar = var(Cited.by,na.rm=T),
               CiteN = length(Cited.by))

## bin for journal rankings
#Jour.Var<-"X2016.CiteScore"  ## Choose metric: "X2016.CiteScore" or "X2016.SJR" or "X2016.SNIP"
Jour.Var<-"X2016.SJR"     ## Use this
#Jour.Var<-"X2016.SNIP"

bins<-quantile(jour.dat[,Jour.Var],na.rm=T)  ##quantile bins
#bins<-c(0,1,2,5,20)

dat$jour.bin<-cut(dat[,Jour.Var],breaks = bins,labels = LETTERS[1:(length(bins)-1)])
dat$jour.bin[which(is.na(dat$jour.bin))]<-"A"

sum.dat2<-ddply(dat,.(Open.Access,Year,jour.bin),summarize,
                CiteAve = mean(Cited.by,na.rm=T),
                CiteVar = var(Cited.by,na.rm=T),
                CiteN = length(Cited.by))

sum.dat2a<-ddply(sum.dat2,.(jour.bin),summarize,
                 N = sum(CiteN))
sum.dat2a$PropN<-sum.dat2a$N/sum(sum.dat2a$N)

## Plots by journal ranking bins
p1<-ggplot(dat)
quartz(width=8,height = 5)
p1 + theme_classic() + 
  ggtitle(Jour.Var) +
  geom_boxplot(aes(x=as.factor(Year),y=log(Cited.by+1),colour=Open.Access),
               outlier.shape=NA) +
  facet_wrap(~jour.bin,nrow = 2,ncol = 2, scales="free")


## FIT MIXED EFFECTS MODEL ##

fit1a<-lmer(Cited.by ~ Open.Access*jour.bin + (1|Year) + (1|Source.title), data=dat)

fit1b<-lmer(Cited.by ~ Open.Access*jour.bin + (1|Year), data=dat)
fit1c<-lmer(Cited.by ~ Open.Access*jour.bin + (1|Source.title), data=dat)
fit1d<-lmer(Cited.by ~ Open.Access + (1|Year) + (1|Source.title), data=dat)
fit1e<-lmer(Cited.by ~ Open.Access + (1|Year), data=dat)
fit1f<-lmer(Cited.by ~ Open.Access + (1|Source.title), data=dat)
fit1g<-lmer(Cited.by ~ 1 + (1|Year) + (1|Source.title), data=dat)
fit1h<-lm(Cited.by ~ Open.Access*jour.bin, data=dat)

fit1i<-lmer(Cited.by ~ Open.Access*jour.bin + (1|Year) + (jour.bin|Source.title), data=dat)
summary(fit1i)


anova(fit1a,fit1b,fit1c,fit1d,fit1e,fit1f,fit1g)
anova(fit1a,fit1d,fit1e)


mod.fit<-fit1i  ## choose model to plot

summary(mod.fit)

coef1<-summary(mod.fit)$coefficients
out.fit1<-data.frame(Open.Access = rep(c("Closed","Open access"),each=4),
                     jour.bin = rep(c(LETTERS[1:4]),times=2))
out.fit1$estimate<-c(coef1[1,1], 
                     coef1[1,1] + coef1[3,1], 
                     coef1[1,1] + coef1[4,1],
                     coef1[1,1] + coef1[5,1],
                     coef1[1,1] + coef1[2,1],
                     coef1[1,1] + coef1[2,1] + coef1[3,1] + coef1[6,1],
                     coef1[1,1] + coef1[2,1] + coef1[4,1] + coef1[7,1],
                     coef1[1,1] + coef1[2,1] + coef1[5,1] + coef1[8,1])
out.fit1$error<-c(coef1[1,2], 
                  coef1[3,2],
                  coef1[4,2],
                  coef1[5,2],
                  coef1[2,2],
                  coef1[6,2],
                  coef1[7,2],
                  coef1[8,2])
out.fit1


## plot output of coefficients and estimates

pfit1<-ggplot(out.fit1)

pf1<-pfit1 + 
  xlab("Journal ranking bin (quantiles)") + ylab("Citations") + 
  geom_pointrange(aes(x=jour.bin, y=estimate, 
                      ymin=(estimate-error), ymax=(estimate+error),
                      colour=Open.Access))

pf1




## fit model to continuous journal ranking

fit2<-lmer(Cited.by ~ Open.Access*X2016.SJR + (1|Year) + (1|Source.title), data=dat)
summary(fit2)

fit3<-lmer(Cited.by ~ Open.Access*X2016.SJR*Year + (1|Source.title), data=dat)
summary(fit3)

anova(fit2,fit3,fit1a)

# find journal ranking where lines interesct
j.int<-summary(fit2)$coefficients[2,1]/-summary(fit2)$coefficients[4,1]

## number and percent of papers here that are in journals where citation rates are higher for OA
nrow(dat[which(dat$X2016.SJR<j.int),])
nrow(dat[which(dat$X2016.SJR<j.int),])/nrow(dat)


## plot all data with slope lines

p2<-ggplot(dat)
p2 + geom_smooth(aes(x=Year,y=log(Cited.by+1),col=Open.Access))

p2 + geom_smooth(aes(x=X2016.SJR,y=log(Cited.by+1),col=Open.Access),method="glm")



# End data analysis for all data
#
####################################
















## EXTRA STUFF for now

## bin for article citation numbers
bins<-quantile(dat$Cited.by,na.rm=T)  ##quantile bins
#bins<-c(0.000,0.99,100,1000,3000)

dat$cite.bin<-cut(dat$Cited.by,breaks = bins,labels = LETTERS[1:(length(bins)-1)])
dat$cite.bin[which(is.na(dat$cite.bin))]<-"A"


sum.dat2<-ddply(dat,.(Open.Access,Year,cite.bin),summarize,
                CiteAve = mean(Cited.by,na.rm=T),
                CiteVar = var(Cited.by,na.rm=T),
                CiteN = length(Cited.by))

## Plots
p1<-ggplot(dat)
quartz(width=8,height = 5)
p1 + theme_classic() + 
  geom_boxplot(aes(x=as.factor(Year),y=log(Cited.by+1),colour=Open.Access),
               outlier.shape=NA) +
  facet_wrap(~cite.bin,nrow = 2,ncol = 2, scales="free")










