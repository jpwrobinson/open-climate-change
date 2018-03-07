

rm(list=ls())
setwd("~/GitHub/open-climate-change/")
setwd('/Users/robins64/Documents/git_repos/open-climate-change')
library(here); library(dplyr); library(tidyr); library(ggplot2); theme_set(theme_bw()); library(gridExtra); library(lme4)

load(file='Data/altmetric_OA_clean.Rdata')
names(alt)

## Summary stats for journals per bin
unique(alt$Journal[alt$SJRfac=='A']) ## n = 24
aggregate(Journal ~ OA, alt[alt$SJRfac=='A',], function(x)length(unique(x))) ## 12 open, 22 closed

unique(alt$Journal[alt$SJRfac=='B']) ## n = 29
aggregate(Journal ~ OA, alt[alt$SJRfac=='B',], function(x)length(unique(x))) ## 16 open, 24 closed

unique(alt$Journal[alt$SJRfac=='C']) ## n = 21
aggregate(Journal ~ OA, alt[alt$SJRfac=='C',], function(x)length(unique(x))) ## 14 open, 17 closed

unique(alt$Journal[alt$SJRfac=='D']) ## n = 23
aggregate(Journal ~ OA, alt[alt$SJRfac=='D',], function(x)length(unique(x))) ## 10 open, 20 closed




## get mean mentions by OA + bin
ratio<-alt %>% mutate(OA = ifelse(OA == TRUE, 'Open', 'Closed')) %>%
	group_by(SJRfac, OA) %>% 
	summarise(news = mean(News.mentions), policy=mean(Policy.mentions), twitter=mean(Twitter.mentions)) %>%
	gather(source, mentions, -SJRfac, -OA) %>% 
	spread(OA, mentions) 

## add zeroes for cases with no mentions in a given year * journal quantile
ratio$Open[is.na(ratio$Open)]<-0
ratio$ratio<-with(ratio, Open/Closed)

ggplot(ratio, aes(SJRfac, Closed, col=source))+geom_point() + facet_wrap (~source)