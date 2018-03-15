

####################
#
# Scopus figure script
#
# 2018/03/14
#
# by TT
#
####################


library(plyr)
library(ggplot2)
library(lme4)

rm(list=ls())
setwd("~/GitHub/open-climate-change/")
setwd('/Users/robins64/Documents/git_repos/open-climate-change')

## load data
load("./Data/scopus_OA_climate_clean.Rdata")  ## scopus data filtered by Jimmy - journals with > 200 papers in last 10 years

j.dat<-read.csv("./Data/BinJournallist_fromScopus.csv",stringsAsFactors = F)

dat<-scop
dat$Cited.by[which(is.na(dat$Cited.by))]<-0
dat<-merge(dat,j.dat[,c("Source.title","bin")],by.x="Source.title",by.y="Source.title",all.x=T)
names(dat)



################## TABLE DATA ###################

## summary of journal bins using JOURNAL open access data
jour.dat<-dat[,c("Source.title","X2016.CiteScore","X2016.SJR","X2016.SNIP","Journal.Open.Access")]
jour.dat<-jour.dat[-which(duplicated(jour.dat)),]

Jour.Var<-"X2016.SJR"     ## Use this
bins<-quantile(jour.dat[,Jour.Var],na.rm=T)  ##quantile bins

jour.dat$bin<-cut(jour.dat[,Jour.Var],breaks = bins,labels = LETTERS[1:(length(bins)-1)])
jour.dat$bin[which(is.na(jour.dat$bin))]<-"A"

ddply(jour.dat,.(bin),summarize,
      Open = length(Journal.Open.Access[which(Journal.Open.Access!="Closed")]),
      Closed = length(Journal.Open.Access[which(Journal.Open.Access=="Closed")]),
      Total = length(Journal.Open.Access))


## summary of journal bins using ARTICLE open access data
tjour.dat<-dat[,c("Source.title","X2016.CiteScore","X2016.SJR","X2016.SNIP","Journal.Open.Access","Article.Open.Access")]
tjour.dat<-tjour.dat[-which(duplicated(tjour.dat)),]
tjour.dat<-merge(tjour.dat,jour.dat[,c("Source.title","bin")],by.x="Source.title",by.y="Source.title",all.x=T)

ddply(tjour.dat,.(bin),summarize,
      Open = length(Article.Open.Access[which(Article.Open.Access!="Closed")]),
      Closed = length(Article.Open.Access[which(Article.Open.Access=="Closed")]),
      Total = length(Article.Open.Access[unique(Source.title)]))


## summary of open access over time
names(dat)

ddply(dat,.(Year),summarize,
      A = length(OA[which(bin=="A" & OA==T)])/length(OA[which(bin=="A")])*100,
      B = length(OA[which(bin=="B" & OA==T)])/length(OA[which(bin=="B")])*100,
      C = length(OA[which(bin=="C" & OA==T)])/length(OA[which(bin=="C")])*100,
      D = length(OA[which(bin=="D" & OA==T)])/length(OA[which(bin=="D")])*100
      )


#################### FIGURE DATA #####################

se<-function(x) sd(x)/sqrt(length(x))

## summary of open access over time
names(dat)

sum.dat<-ddply(dat,.(Year,bin,OA),summarize,
         NoArt = length(OA))

p1<-ggplot(sum.dat)
p1 + geom_line(aes(x=Year,y=NoArt,colour=bin,linetype=OA))



## mean citation rates by bin for all years aggregated 
names(dat)
mean.dat<-ddply(dat,.(bin,OA),summarize,
                MeanCite = mean(Cited.by,na.rm=T),
                SECite = se(Cited.by))
mean.dat$xdummy<-as.numeric(as.factor(mean.dat$bin)) + with(mean.dat,ifelse(OA==T,-0.15,0.15))
mean.dat$OA<-factor(mean.dat$OA,levels = c(TRUE,FALSE))

p1<-ggplot(mean.dat)
p1f<-p1 + theme_classic() + 
  theme(axis.title.x = element_blank()) +
  ylab("Citations") +
  theme(legend.title =  element_blank()) +
  geom_pointrange(aes(x=xdummy,y=MeanCite,ymin=MeanCite-(SECite*1.96),ymax=MeanCite+(SECite*1.96),
                      colour=bin,shape=OA)) +
  scale_x_continuous(breaks = seq(1:4),labels=c("Low","Medium","High","Very high"),
                     expand=c(0,0)) +
  coord_cartesian(xlim=c(0.5,4.5)) + 
  scale_shape_manual(values = c(1,19),labels=c("Open","Closed")) + 
  scale_colour_discrete(guide=F)


pdf("./figures/scopusfig_meanbins_1.pdf",width=3.5,height=2.5)
p1f
dev.off()





## mean Citation rates across time
names(dat)
mean.dat<-ddply(dat,.(Year,bin,OA),summarize,
                MeanCite = mean(Cited.by,na.rm=T),
                SECite = se(Cited.by))

mean.dat$binlabel<-with(mean.dat,ifelse(bin=="A","Low",
                                    ifelse(bin=="B","Medium",
                                       ifelse(bin=="C","High","Very high"))))
mean.dat$binlabel<-factor(mean.dat$binlabel,levels = c("Low","Medium","High","Very high"))

mean.dat$xdummy<-mean.dat$Year + with(mean.dat,ifelse(OA==T,-0.15,0.15))
mean.dat$OA<-factor(mean.dat$OA,levels = c(TRUE,FALSE))


p2<-ggplot(mean.dat)
p2 + geom_pointrange(aes(x=Year,y=MeanCite,ymin=MeanCite-(SECite*1.96),ymax=MeanCite+(SECite*1.96),
                         colour=bin,shape=OA))

p2 + geom_pointrange(aes(x=xdummy,y=MeanCite,ymin=MeanCite-(SECite*1.96),ymax=MeanCite+(SECite*1.96),
                         colour=bin,shape=OA)) +
  scale_shape_manual(values = c(1,19),labels=c("Open","Closed"))
  

p2 + geom_pointrange(aes(x=xdummy,y=MeanCite,ymin=MeanCite-(SECite*1.96),ymax=MeanCite+(SECite*1.96),
                         colour=bin,shape=OA)) +
  scale_shape_manual(values = c(1,19),labels=c("Open","Closed")) + 
  facet_wrap(~bin,scales = "free")




quartz(width=7,height=4)
p2<-ggplot(mean.dat)
p2f<-p2 + theme_classic() + 
  theme(strip.background = element_blank(),
        strip.text = element_text(hjust = 0),
        legend.title =  element_blank()
        #axis.title = element_text(size=6),
        #axis.text = element_text(size=6)
        ) + 
  labs(x="Year",y="Citations") +
  geom_pointrange(aes(x=xdummy,y=MeanCite,ymin=MeanCite-(SECite*1.96),ymax=MeanCite+(SECite*1.96),
                      colour=bin,shape=OA)) +
  facet_wrap(~binlabel,scales="free") + 
  scale_x_continuous(breaks = seq(2007,2016,1),labels=c("",2008,"",2010,"",2012,"",2014,"",2016),
                     expand=c(0,0)) +
  coord_cartesian(xlim=c(2006.5,2016.5)) + 
  scale_shape_manual(values = c(1,19),labels=c("Open","Closed"))
  

pdf("./figures/scopusfig_meanbinyears_1.pdf",width=7,height=4)
p2f
dev.off()





## Plot Model fit 
fit1a<-lmer(Cited.by ~ Open.Access*bin + (1|Year) + (1|Source.title),)
mod.fit<-fit1a  ## choose model to plot

summary(mod.fit)

coef1<-summary(mod.fit)$coefficients
out.fit1<-data.frame(Open.Access = rep(c("Closed","Open"),each=4),
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

out.fit1$xdummy<-as.numeric(as.factor(out.fit1$jour.bin)) + 
  with(out.fit1,ifelse(Open.Access=="Open",-0.15,0.15))
out.fit1$Open.Access<-factor(out.fit1$Open.Access,levels = c("Open","Closed"))



pfit1<-ggplot(out.fit1)

pf1<-pfit1 + theme_classic() + 
  theme(legend.title =  element_blank(),
        axis.title.x = element_blank()
  ) + 
  ylab("Citations") +
  geom_pointrange(aes(x=xdummy,y=estimate,ymin=estimate-(error*1.96),ymax=estimate+(error*1.96),
                      colour=jour.bin,shape=Open.Access)) +
  geom_point(aes(x=c(101:108),y=estimate,shape=Open.Access)) +
  scale_x_continuous(breaks = seq(1:4),labels=c("Low","Medium","High","Very high"),
                     expand=c(0,0)) +
  coord_cartesian(xlim=c(0.5,4.5)) + 
  scale_shape_manual(values = c(1,19),labels=c("Open","Closed")) + 
  scale_colour_discrete(guide=F)


quartz(width=3.5,height=2)

pdf("./figures/scopusfig_model_1.pdf",width=3.5,height=2.5)
pf1
dev.off()











