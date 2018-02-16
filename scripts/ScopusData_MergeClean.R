


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

j.dat<-read.csv("./Data/JournalList_Scopus.csv",stringsAsFactors = F,na.strings=c(NA,""))

file.list<-list.files("./Data/ScopusCiteRawData_2007-2016",full.names=T)
out.dat<-data.frame()
for(i in 1:length(file.list)){
  tdat<-read.csv(file.list[i],stringsAsFactors = F,na.strings=c(NA,""))
  out.dat<-rbind(out.dat,tdat)
  rm(tdat)
}

str(out.dat)
out.dat2<-out.dat[-which(duplicated(out.dat)),]
out.dat3<-out.dat2[-which(is.na(out.dat2$Source.title)),]

tdat<-j.dat[,c("Source.Title","X2016.CiteScore","X2016.SJR","X2016.SNIP","Open.Access","Publisher.s.Country")]
dup<-tdat[-which(duplicated(tdat$Source.Title)),]
names(dup)[which(names(dup)=="Open.Access")]<-"Journal.Open.Access"

out.dat4<-merge(out.dat3,dup,by.x="Source.title",by.y="Source.Title",all.x=T)

## Data frame to work with
dat<-out.dat4
names(dat)
dat$Access.Type[is.na(dat$Access.Type)]<-dat$Journal.Open.Access[is.na(dat$Access.Type)]
dat$Access.Type[which(dat$Access.Type=="Article")]<-"Closed"
dat$Access.Type[is.na(dat$Access.Type)]<-"Closed"
dat$Access.Type[which(dat$Access.Type=="DOAJ/ROAD Open Access")]<-"Open Access"
names(dat)[which(names(dat)=="Access.Type")]<-"Article.Open.Access"

## WRite out cleaned up data
#write.csv(dat,"./Data/ScopusOAData_20180214TT.csv",row.names = F)





















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










































