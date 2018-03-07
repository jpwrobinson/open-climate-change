


## Script for reading in raw almetric, attaching OA status, removing extra columns

rm(list=ls())
setwd("~/GitHub/open-climate-change/")
setwd('/Users/robins64/Documents/git_repos/open-climate-change')


library(here); library(stringr)

# alt<-read.csv('Data/altmetric/Altmetric_2018-02-26.csv')
alt<-read.csv('Data/altmetric/Altmetric_2018-03-04.csv')

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
alt$Facebook.mentions<-NULL
alt$Wikipedia.mentions<-NULL
alt$Google..mentions<-NULL
alt$Reddit.mentions<-NULL
alt$F1000.mentions<-NULL
alt$Q.A.mentions<-NULL
alt$Video.mentions<-NULL
alt$Syllabi.mentions<-NULL
alt$Badge.URL<-NULL
alt$Number.of.Mendeley.readers<-NULL
alt$Details.Page.URL<-NULL

## change some column names
colnames(alt)[colnames(alt)=='Journal.Collection.Title']<-'Journal'

## scopus entries
load("./Data/scopus_OA_climate_clean.Rdata")  ## scopus data filtered by Jimmy - journals with > 200 papers in last 10 years

## add OA info to altmetric dataframe
alt$OA<-scop$OA[match(alt$DOI,scop$DOI)]
dim(alt[is.na(alt$OA),])
## missing 3728 papers: these are commentaries ?

## drop missing papers
alt <- alt[!is.na(alt$OA),]

## add Year for modelling grouping/temporal plots
alt$year<-as.numeric(str_split_fixed(as.character(alt$Publication.Date), '-', 3)[,1])

## add journal impact factor
alt$SJR<-scop$X2016.SJR[match(alt$DOI, scop$DOI)]

## drop 1 FAO paper
alt<-alt[!alt$Journal=='',]
## drop economic paper
alt<-alt[!alt$Journal=='Latin American Economic Review',]

## fix journal names
alt$Journal<-str_replace_all(alt$Journal, '&', 'and')
alt$Journal[alt$Journal=='Proceedings of the National Academy of Sciences']<-'Proceedings of the National Academy of Sciences of the United States of America'
alt$Journal[alt$Journal=='Environmental Research Letters (ERL)']<-'Environmental Research Letters'
alt$Journal[alt$Journal=='Global Environmental Change Part A: Human and Policy Dimensions']<-'Global Environmental Change'
alt$Journal[alt$Journal=='Water (20734441)']<-'Water (Switzerland)'
alt$Journal[alt$Journal=='Atmospheric Environment (00046981)']<-'Atmospheric Environment'
alt$Journal[alt$Journal=='AMBIO - A Journal of the Human Environment']<-'Ambio'
alt$Journal[alt$Journal=='Deep-Sea Research Part II, Topical Studies in Oceanography']<-'Deep-Sea Research Part II: Topical Studies in Oceanography'
alt$Journal[alt$Journal=='CATENA']<-'Catena'
alt$Journal[alt$Journal=='PloS one']<-'PLoS ONE'
alt$Journal[alt$Journal=='Science of The Total Environment']<-'Science of the Total Environment'
alt$Journal[alt$Journal=='AMBIO']<-'Ambio'
alt$Journal[alt$Journal=='Estuarine Coastal and Shelf Science']<-'Estuarine, Coastal and Shelf Science'

## add impact bins
bins<-read.csv('Data/BinJournallist_fromScopus.csv')

alt$SJRfac<-bins$bin[match(alt$Journal, bins$Source.title)]


save(alt, file='Data/altmetric_OA_clean.Rdata')



