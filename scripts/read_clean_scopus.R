

setwd('/Users/robins64/Documents/git_repos/open-climate-change')
library(stringr)

## Reading in scopus data, cleaning, merging, saving to one dataframe

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


### Need to subset to relevant climate + ecology journals

journals<-read.csv(file='Data/climate_journals.csv')

scop<-scop[scop$Source.title %in% journals$journal,]
dim(scop)


save(scop, file='Data/scopus_OA_climate_clean.Rdata')
