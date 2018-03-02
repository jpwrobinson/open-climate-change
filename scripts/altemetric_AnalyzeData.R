

rm(list=ls())
setwd("~/GitHub/open-climate-change/")
setwd('/Users/robins64/Documents/git_repos/open-climate-change')
library(here); library(dplyr); library(tidyr); library(ggplot2); theme_set(theme_bw())

load(file='Data/altmetric_OA_clean.Rdata')
names(alt)
means<-alt %>% select(Altmetric.Attention.Score, Journal.Collection.Title, Publication.Date, News.mentions, Blog.mentions, Policy.mentions, Twitter.mentions, OA) %>%
		filter(!Journal.Collection.Title == '') %>%
		group_by(Journal.Collection.Title, OA) %>% 
		summarise(news = mean(News.mentions),
				  blog=mean(Blog.mentions),
				  policy=mean(Policy.mentions),
				  twitter=mean(Twitter.mentions)) %>%
		gather(source, mentions, -Publication.Date, -Journal.Collection.Title, -OA) 

ggplot(means, aes(source, mentions, fill=OA)) + geom_boxplot() + facet_wrap(~source, scales='free')


