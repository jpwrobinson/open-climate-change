

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

#nrow(dat[which(dat$Cited.by==0),])
#dat<-dat[which(dat$Cited.by>0),]


jour.dat<-dat[,c("Source.title","X2016.CiteScore","X2016.SJR","X2016.SNIP","Journal.Open.Access")]
jour.dat<-jour.dat[-which(duplicated(jour.dat)),]


## plot raw data
p1<-ggplot(dat)
p1F<-p1 + geom_boxplot(aes(x=as.factor(Year), y=log(Cited.by+1), colour=Open.Access)) + 
  xlab("Year") + ylab("# citations (log +1)")
p1F<-p1 + geom_boxplot(aes(x=as.factor(Year), y=log(Cited.by+1), colour=Open.Access),
                  outlier.shape=NA) + 
  xlab("Year") + ylab("# citations (log +1)")

#pdf("./figures/exploratory/scopus/cite_byyear_boxplot_nooutliers.pdf")
p1F
dev.off()


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

## write out journal list with bins
jour.dat$bin<-cut(jour.dat[,Jour.Var],breaks = bins,labels = LETTERS[1:(length(bins)-1)])
jour.dat$bin[which(is.na(jour.dat$bin))]<-"A"

write.csv(jour.dat,"./Data/BinJournallist_fromScopus.csv",row.names=F)



## article journal bins
dat$jour.bin<-cut(dat[,Jour.Var],breaks = bins,labels = LETTERS[1:(length(bins)-1)])
dat$jour.bin[which(is.na(dat$jour.bin))]<-"A"

sum.dat2<-ddply(dat,.(Open.Access,Year,jour.bin),summarize,
                CiteAve = mean(Cited.by,na.rm=T),
                CiteVar = var(Cited.by,na.rm=T),
                CiteN = length(Cited.by))

sum.dat2a<-ddply(sum.dat2,.(jour.bin),summarize,
                 N = sum(CiteN))
sum.dat2a$PropN<-sum.dat2a$N/sum(sum.dat2a$N)

sum.dat3<-cast(sum.dat2[,1:4],Year+jour.bin ~ Open.Access)
names(sum.dat3)[4]<-"Open.Access"
sum.dat3$CiteAveRatio<-with(sum.dat3,Open.Access/Closed)

## Plots by journal ranking bins
p1<-ggplot(dat)
#quartz(width=8,height = 5)
p1F<-p1 + theme_classic() + 
  ggtitle(Jour.Var) + 
  xlab("Year") + 
  ylab("# citations (log + 1)") +
  geom_boxplot(aes(x=as.factor(Year),y=log(Cited.by+1),colour=Open.Access),
               outlier.shape=NA) +
  facet_wrap(~jour.bin,nrow = 2,ncol = 2, scales="free") 
  

pdf("./figures/exploratory/scopus/citebySJRbins_byyear_nooutliers.pdf",width=8,height=6)
p1F
dev.off()

p1<-ggplot(sum.dat2)
#quartz(width=8,height = 5)
p1F<-p1 +
  ggtitle(Jour.Var) + 
  xlab("Year") + 
  ylab("Mean # citations") +
  geom_point(aes(x=Year,y=CiteAve,colour=jour.bin, shape=Open.Access)) 


p2<-ggplot(sum.dat3)
#quartz(width=8,height = 5)
p2F<-p2 +
  ggtitle(Jour.Var) + 
  geom_hline(yintercept = 1, lty=2, colour="grey25") +
  xlab("Year") + 
  ylab("Ratio # citations (OA:Closed)") +
  geom_point(aes(x=Year,y=CiteAveRatio,colour=jour.bin)) 



pdf("./figures/exploratory/scopus/meancitebySJRbins_byyear.pdf")
p1F
p2F

dev.off()


############################
## FIT MIXED EFFECTS MODEL ##

fit1a<-lmer(Cited.by ~ Open.Access*jour.bin + (1|Year) + (1|Source.title), data=dat)

fit1b<-lmer(Cited.by ~ Open.Access*jour.bin + (1|Year), data=dat)
fit1c<-lmer(Cited.by ~ Open.Access*jour.bin + (1|Source.title), data=dat)
fit1d<-lmer(Cited.by ~ Open.Access + (1|Year) + (1|Source.title), data=dat)
fit1e<-lmer(Cited.by ~ Open.Access + (1|Year), data=dat)
fit1f<-lmer(Cited.by ~ Open.Access + (1|Source.title), data=dat)
fit1g<-lmer(Cited.by ~ 1 + (1|Year) + (1|Source.title), data=dat)
fit1h<-lm(Cited.by ~ Open.Access*jour.bin, data=dat)
#fit1i<-lmer(Cited.by ~ Open.Access*jour.bin + (1|Year) + (jour.bin|Source.title), data=dat)

anova(fit1a,fit1b,fit1c,fit1d,fit1e,fit1f,fit1g)
anova(fit1a,fit1d,fit1e)

mod.fit<-fit1a  ## choose model to plot

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

pdf("./figures/exploratory/scopus/citebySJRbins_lmerModelfit1a.pdf")
pf1
dev


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

pf2<-p2 + geom_smooth(aes(x=X2016.SJR,y=log(Cited.by+1),col=Open.Access),method="glm") +
  xlab("SJR ranking 2016") + ylab("# citations (log + 1)")

pdf("./figures/exploratory/scopus/citebySJR_modelfit3.pdf")
pf2
dev.off()

##########
## fit model with year as factor
dat$Year.Fac<-as.factor(dat$Year)

fit4a<-lmer(Cited.by ~ Open.Access*jour.bin*Year.Fac + (1|Source.title), data=dat)

anova(fit1a,fit4a)

summary(fit4a)$coefficients[,1:2]


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










