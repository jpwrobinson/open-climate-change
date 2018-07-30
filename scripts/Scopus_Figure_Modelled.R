

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
load("./Data/scopus_OA_climate_clean.Rdata")  ## scopus data filtered by Jimmy - journals with > 100 papers in last 10 years
# load('./Data/scopus_glmerfit.Rdata')


## choose filtered data or full data
dat<-scop
dat$Cited.by[which(is.na(dat$Cited.by))]<-0

jour.dat<-dat[,c("Source.title","X2016.CiteScore","X2016.SJR","X2016.SNIP","Journal.Open.Access")]
jour.dat<-jour.dat[-which(duplicated(jour.dat)),]

## bin for journal rankings
Jour.Var<-"X2016.SJR"     ## Use this
bins<-quantile(jour.dat[,Jour.Var],na.rm=T)  ##quantile bins
bins

## 200 threshold
 #     0%     25%     50%     75%    100% 
 # 0.1380  1.1590  1.7430  2.6635 18.1340

## 100 threshold
 #    0%    25%    50%    75%   100% 
 # 0.138  0.926  1.453  2.197 18.134


# ggplot(jour.dat, aes(x = X2016.SJR)) + geom_histogram() + scale_x_log10()

## write out journal list with bins
jour.dat$bin<-cut(jour.dat[,Jour.Var],breaks = bins,labels = LETTERS[1:(length(bins)-1)])
jour.dat$bin[which(is.na(jour.dat$bin))]<-"A"

## article journal bins
dat$jour.bin<-cut(dat[,Jour.Var],breaks = bins,labels = LETTERS[1:(length(bins)-1)])
dat$jour.bin[which(is.na(dat$jour.bin))]<-"A"


## save journal list
write.csv(jour.dat, file="Data/BinJournallist_fromScopus.csv")

#############################
####### MODEL FIT TO AGGREGATED BY JOURNAL ########
names(dat)
sub.dat<-dat[,c("Source.title","Year","Cited.by","OA","Open.Access","jour.bin")]
mod.dat<-ddply(sub.dat,.(Source.title,Year,Open.Access,OA,jour.bin),summarize,
               MeanCite = mean(Cited.by,na.rm=T))

mod.dat$log10MeanCite<-log10(mod.dat$MeanCite+1)
mod.dat$jour.bin.scaled<-scale(as.numeric(mod.dat$jour.bin))


fit5a<-lmer(log10MeanCite ~ jour.bin.scaled*OA + (1|Year) + (1 | Source.title),data=mod.dat)
with(mod.dat, table(jour.bin, OA))

summary(fit5a)
hist(resid(fit5a))
plot(resid(fit5a)~fitted(fit5a))
plot(fitted(fit5a)~mod.dat$log10MeanCite)

# fit5b<-glmer(log10(Cited.by+1) ~ jour.bin*OA + (1|Year),data=dat,family="poisson")
# hist(resid(fit5b))
# plot(resid(fit5b)~fitted(fit5b))
# plot(fitted(fit5b)~log10(dat$Cited.by+1))


# # quartz()
# # hist(resid(fit5b))
# # plot(resid(fit5b)~fitted(fit5b))

# fit5c<-glmer(MeanCite ~ jour.bin*OA + (1|Year),data=mod.dat,family="poisson")
# hist(resid(fit5c))
# plot(resid(fit5c)~fitted(fit5c))
# plot(fitted(fit5c)~mod.dat$MeanCite)


# fit5d<-glmer(log10(MeanCite+1) ~ jour.bin*OA + (1|Year),data=mod.dat,family="poisson")
# hist(resid(fit5d))
# plot(resid(fit5d)~fitted(fit5d))
# plot(fitted(fit5d)~log10(mod.dat$MeanCite+1))


# fit5e<-glmer(MeanCite ~ jour.bin*OA + (1|Year) + (1|Source.title),data=mod.dat,family="poisson")
# hist(resid(fit5e))
# plot(resid(fit5e)~fitted(fit5e))
# plot(fitted(fit5e)~mod.dat$MeanCite)

# fit5f<-glmer(log10(MeanCite+1) ~ jour.bin*OA + (1|Year) + (1|Source.title),data=mod.dat,family="poisson")
# hist(resid(fit5f))
# plot(resid(fit5f)~fitted(fit5f))
# plot(fitted(fit5f)~log10(mod.dat$MeanCite+1))


# fit5g<-lmer(log10(Cited.by+1) ~ jour.bin*OA + (1|Year) + (1 | Source.title),data=dat)
# summary(fit5g)
# hist(resid(fit5g))
# plot(resid(fit5g)~fitted(fit5g))
# plot(fitted(fit5g)~log10(dat$Cited.by+1))

# fit5h<-lmer(log10(MeanCite+1) ~ jour.bin*OA + (1|Year) + (1 | Source.title),data=mod.dat)
# summary(fit5h)
# hist(resid(fit5h))
# plot(resid(fit5h)~fitted(fit5h))
# plot(fitted(fit5h)~log10(mod.dat$MeanCite+1))


# fit5i<-lmer(log10(MeanCite+1) ~ jour.bin*OA + (1|Year) + (1 | Source.title),data=mod.dat)
# summary(fit5i)
# hist(resid(fit5i))
# plot(resid(fit5i)~fitted(fit5i))
# plot(fitted(fit5i)~log10(mod.dat$MeanCite+1))
# AIC(fit5i)


## save model output
save(fit5a, mod.dat, file='./Data/scopus_glmerfit.Rdata')
###########################



######### PLOT model fit data ##########

# mod.plot<-fit5a

# scop.dat<-expand.grid(OA = unique(mod.dat$OA), jour.bin=unique(mod.dat$jour.bin), year = 2008, Journal='Ecology')
# scop.dat$p<-predict(mod.plot, newdat=scop.dat, re.form=NA, type='response')


# scop.dat$OA<-factor(scop.dat$OA,levels=c(TRUE,FALSE))
# scop.dat$xdummy<-as.numeric(as.factor(scop.dat$jour.bin)) + ifelse(scop.dat$OA==TRUE,-0.15,+0.15)

# pfit1<-ggplot(scop.dat)
# pf1<-pfit1 + theme_classic() + 
#   theme(legend.title =  element_blank(),
#         axis.title.x = element_blank()
#   ) + 
#   ylab("Citations") +
#   geom_point(aes(x=xdummy,y=10^p,colour=jour.bin,shape=OA)) +
#   scale_x_continuous(breaks = seq(1:4),labels=c("Low","Medium","High","Very high"),
#                      expand=c(0,0)) +
#   coord_cartesian(xlim=c(0.5,4.5)) + 
#   scale_shape_manual(values = c(1,19),labels=c("Open","Closed")) + 
#   scale_colour_discrete(guide=F)


# quartz(width=3.5,height=2.5)
# pf1

# pdf("./figures/Scopus_glmer_fig.pdf",width=3.5,height=2.5)
# pf1
# dev.off()

