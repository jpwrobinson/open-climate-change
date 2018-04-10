
########################
## CC Open science
#
## Figure 2
#
## OA articles over time 


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

## summary of open access over time
names(dat)

sum.dat<-ddply(dat,.(Year,bin),summarize,
               OpenArt = length(OA[which(OA==T)]),
               ClosArt = length(OA[which(OA==F)]))
sum.dat$Ratio<-with(sum.dat, OpenArt/ClosArt)

sum.dat$OpenPer<-with(sum.dat, OpenArt/(OpenArt+ClosArt)*100)
sum.dat$ClosPer<-with(sum.dat, ClosArt/(OpenArt+ClosArt)*100)
sum.dat$ClosDif<-with(sum.dat, ClosPer-100)

sum.totdat<-ddply(dat,.(Year),summarize,
                  OpenArt = length(OA[which(OA==T)]),
                  ClosArt = length(OA[which(OA==F)]))
sum.totdat$Tot<-with(sum.totdat,OpenArt + ClosArt)

p1<-ggplot(sum.dat)
p1f<-p1 + theme_classic() + 
  theme(text = element_text(size=6),
        legend.text = element_text(size=4),
        legend.title = element_text(size=4),
        legend.key.height = unit(6,units="points")) +
  geom_line(aes(x=Year,y=OpenPer,colour=bin),lwd=0.3) + 
  geom_line(aes(x=Year,y=(OpenArt/Tot*100)),data=sum.totdat,
            lwd=0.7) +
  ylab("OA publications (%)") + 
  coord_cartesian(expand=F) + 
  scale_color_discrete(labels = c("Low","Medium","High","Very high"),name="Journal impact")
  


pdf(file="./figures/Figure1.pdf", height=2, width=3)
#quartz(height=2,width=3)
p1f
dev.off()






## Not use below ##

## graph of total number of publications for each journal bin across time
## Journal information plot
names(dat)
sum.j<-ddply(dat[dat$Year==2016,],.(Source.title,Article.Open.Access,Journal.Open.Access,bin),summarize,
             SJR = mean(X2016.SJR),
             NoArt = length(SJR))
name.ind<-sum.j$Source.title[duplicated(sum.j$Source.title)]

jour.dat$OAtype<-"Closed"
jour.dat$OAtype[jour.dat$Source.title %in% name.ind]<-"Hybrid"
jour.dat$OAtype[jour.dat$Journal.Open.Access=="DOAJ/ROAD Open Access"]<-"Open"

sum.totdat<-ddply(dat,.(Year,bin),summarize,
                  TotArt = length(bin))

p2<-ggplot(sum.totdat)
p2f<-p2 + theme_classic() +
  geom_area(aes(x=Year,y=TotArt,fill=bin,colour=bin),alpha=0.6) + 
  ylab("Total publications") + 
  coord_cartesian(expand=F)
p2f












