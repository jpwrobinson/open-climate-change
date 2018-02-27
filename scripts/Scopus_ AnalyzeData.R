

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
setwd('/Users/robins64/Documents/git_repos/open-climate-change')

## load data
load("./Data/scopus_OA_climate_clean.Rdata")  ## scopus data filtered by Jimmy - top 30 journals
dat<-read.csv("./Data/ScopusOAData_20180214TT.csv",na.strings=F)  ## cleaned data








