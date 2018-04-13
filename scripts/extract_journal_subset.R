## Read journal list and identify specific climate journals

rm(list=ls())
setwd("~/GitHub/open-climate-change/")
setwd('/Users/robins64/Documents/git_repos/open-climate-change')

j.dat<-read.csv("Data/JournalList_Scopus.csv",stringsAsFactors = F,na.strings=c(NA,""))

scop<-read.csv('Data/ScopusOAData_20180214TT.csv')

## creating cleaner version for analysis. Use big csv for paper referencing if necessary
scop$X...Authors <- NULL
scop$Title <- NULL
scop$Volume <- NULL
scop$Issue <- NULL
scop$Art..No. <- NULL
scop$Page.start <- NULL
scop$Page.end <- NULL
scop$Page.count <- NULL
scop$Link <- NULL
scop$Source <- NULL
scop$EID <- NULL
scop$Publisher.s.Country <- NULL

## add OA identifier
scop$OA<-ifelse(is.na(scop$Journal.Open.Access), FALSE, TRUE)

## remove inactive journals
# scop$active<-j.dat$Active.or.Inactive[match(scop$Source.title, j.dat$Source.Title)]
# scop<-scop[which(scop$active=='Active'),]

t<-aggregate(Authors ~ Source.title, scop, length)

## fair to just take the journals with highest number of papers?
journal.list<-t$Source.title[t$Authors>200] ## n = 53
journal.list<-data.frame(journal=journal.list)
## add ISSN for altmetric search
journal.list$ISSN<-j.dat$E.ISSN[match(journal.list$journal, j.dat$Source.Title)]
journal.list$ISSN[is.na(journal.list$ISSN)]<-j.dat$Print.ISSN[match(journal.list$journal[is.na(journal.list$ISSN)], j.dat$Source.Title)]

write.csv(journal.list, file='Data/climate_journals.csv')
