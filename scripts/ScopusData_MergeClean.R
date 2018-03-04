


####################
#
# Open access climate change papers - Merge and clean raw data
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

j.dat<-read.csv("./Data/RawData/Scopus_JournalList_20180226.csv",stringsAsFactors = F,na.strings=c(NA,""))

file.list<-list.files("./Data/RawData/ScopusCiteRawData_2007-2016",full.names=T)
out.dat<-data.frame()
for(i in 1:length(file.list)){
  tdat<-read.csv(file.list[i],stringsAsFactors = F,na.strings=c(NA,""))
  out.dat<-rbind(out.dat,tdat)
  rm(tdat)
}

str(out.dat)
if(length(which(duplicated(out.dat)))>0){   ## remove duplicated rows
  out.dat2<-out.dat[-which(duplicated(out.dat)),]
} else {out.dat2<-out.dat}
out.dat3<-out.dat2[-which(is.na(out.dat2$Source.title)),]  ## remove rows that don't have journal title 

tdat<-j.dat[,c("Source.Title","X2016.CiteScore","X2016.SJR","X2016.SNIP","Open.Access","Publisher.s.Country")]
dup<-tdat[-which(duplicated(tdat$Source.Title)),]
names(dup)[which(names(dup)=="Open.Access")]<-"Journal.Open.Access"

out.dat4<-merge(out.dat3,dup,by.x="Source.title",by.y="Source.Title",all.x=T)

## Data frame. Create two columns: 1) journal = OA, 2) article = OA
dat<-out.dat4
names(dat)

dat$Access.Type[which(dat$Access.Type=="Article")]<-"Closed"
dat$Access.Type[is.na(dat$Access.Type)]<-"Closed"
names(dat)[which(names(dat)=="Access.Type")]<-"Article.Open.Access"  ## article open access

dat$Journal.Open.Access[is.na(dat$Journal.Open.Access)]<-"Closed"  ## journal open access

## add OA identifier
dat$OA<-ifelse(dat$Article.Open.Access== "Closed", FALSE, TRUE)  ## T/F identified
dat$OA[which(dat$Journal.Open.Access=="DOAJ/ROAD Open Access")]<-TRUE
dat$Open.Access<-ifelse(dat$OA==TRUE, "Open access", "Closed")  ## Factor identifier

write.csv(dat,"./Data/CleanScopusOAData_AllData_20180228TT.csv",row.names = F)


##########
# creating cleaner version of full data for analysis. Use big csv for paper referencing if necessary
dat$X...Authors <- NULL
dat$Title <- NULL
dat$Volume <- NULL
dat$Art..No. <- NULL
dat$Page.start <- NULL
dat$Page.end <- NULL
dat$Page.count <- NULL
dat$Link <- NULL
dat$Source <- NULL
dat$EID <- NULL
dat$Publisher.s.Country <- NULL



## remove inactive journals
dat$active<-j.dat$Active.or.Inactive[match(dat$Source.title, j.dat$Source.Title)]
dat<-dat[which(dat$active=='Active'),]
##########

## Write out cleaned up data
write.csv(dat,"./Data/ScopusOAData_20180214TT.csv",row.names = F)




############################################################
############################################################
############################################################
## Read journal list and identify specific climate journals

rm(list=ls())
setwd("~/GitHub/open-climate-change/")
setwd('/Users/robins64/Documents/git_repos/open-climate-change')

j.dat<-read.csv("./Data/RawData/Scopus_JournalList_20180226.csv",stringsAsFactors = F,na.strings=c(NA,""))
scop<-read.csv("./Data/ScopusOAData_20180214TT.csv")

t<-aggregate(Authors ~ Source.title, scop, length)

## fair to just take the journals with highest number of papers?
journal.list<-t$Source.title[t$Authors>200] ## n = 116
journal.list<-data.frame(journal=journal.list)
## add ISSN for altmetric search
journal.list$ISSN<-j.dat$E.ISSN[match(journal.list$journal, j.dat$Source.Title)]
journal.list$ISSN[is.na(journal.list$ISSN)]<-j.dat$Print.ISSN[match(journal.list$journal[is.na(journal.list$ISSN)], j.dat$Source.Title)]

write.csv(journal.list, file='Data/climate_journals.csv')



############################################################
############################################################
############################################################

## Reading in scopus data, cleaning, merging, saving to one dataframe
scop<-read.csv('Data/ScopusOAData_20180214TT.csv')

### Need to subset to relevant climate + ecology journals

journals<-read.csv(file='Data/climate_journals.csv')

scop<-scop[scop$Source.title %in% journals$journal,]
dim(scop) ## 57610 rows

save(scop, file='Data/scopus_OA_climate_clean.Rdata')







