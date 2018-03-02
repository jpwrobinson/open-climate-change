


## Script for reading in raw almetric, attaching OA status, removing extra columns

rm(list=ls())
setwd("~/GitHub/open-climate-change/")
setwd('/Users/robins64/Documents/git_repos/open-climate-change')


library(here); library(stringr)

alt<-read.csv('Data/altmetric/Altmetric_2018-02-26.csv')
## drop columns we probably won't use
alt$Authors.at.my.Institution<-NULL
alt$Departments<-NULL
alt$National.Clinical.Trial.ID<-NULL
alt$SSRN<-NULL
alt$URN<-NULL
alt$URI<-NULL
alt$PubMed.ID<-NULL
alt$PubMedCentral.ID<-NULL
alt$Handle.net.IDs<-NULL
alt$ADS.Bibcode<-NULL
alt$arXiv.ID<-NULL
alt$RePEc.ID<-NULL
alt$Peer.review.mentions<-NULL
alt$Weibo.mentions<-NULL
alt$LinkedIn.mentions<-NULL
alt$Pinterest.mentions<-NULL
alt$Facebook.metions<-NULL
alt$Wikipedia.mentions<-NULL
alt$Google..mentions<-NULL
alt$Reddit.mentions<-NULL
alt$F1000.mentions<-NULL
alt$Q.A.mentions<-NULL
alt$Video.mentions<-NULL
alt$Syllabi.mentions<-NULL
alt$Badge.URL<-NULL
alt$Number.of.Mendeley.readers<-NULL

## scopus entries
load("./Data/scopus_OA_climate_clean.Rdata")  ## scopus data filtered by Jimmy - journals with > 300 papers in last 10 years

## add OA info to altmetric dataframe
alt$OA<-scop$OA[match(alt$DOI,scop$DOI)]
dim(alt[is.na(alt$OA),])
## still missing 3000 papers

## add Year for modelling grouping/temporal plots
alt$year<-as.numeric(str_split_fixed(as.character(alt$Publication.Date), '-', 3)[,1])
## drop 2017 studies
alt <- alt[!alt$year == 2017,]



save(alt, file='Data/altmetric_OA_clean.Rdata')



