

rm(list=ls())
setwd("~/GitHub/open-climate-change/")
setwd('/Users/robins64/Documents/git_repos/open-climate-change')
library(here); library(dplyr); library(tidyr); library(ggplot2); theme_set(theme_bw()); library(gridExtra); library(lme4)

load(file='Data/altmetric_OA_clean.Rdata')
names(alt)

## Average mentions for each journal in each year
means<-alt %>% select(Altmetric.Attention.Score, Journal, year, SJR,News.mentions, Blog.mentions, Policy.mentions, Twitter.mentions, OA) %>%
		filter(!Journal == '') %>%
		group_by(Journal, OA, year) %>% 
		summarise(news = mean(News.mentions),
				  blog=mean(Blog.mentions),
				  policy=mean(Policy.mentions),
				  twitter=mean(Twitter.mentions),
				  SJR = mean(SJR)) 

# plot raw mean mentions
g1<-ggplot(means, aes(factor(year), blog, fill=OA)) + geom_boxplot() + 
		labs(y='Mentions', x='Year', title='blog') + guides(fill=FALSE)
g2<-ggplot(means, aes(factor(year), news, fill=OA)) + geom_boxplot() + 
		labs(y='Mentions', x='Year', title='news') + guides(fill=FALSE)
g3<-ggplot(means, aes(factor(year), policy, fill=OA)) + geom_boxplot() + 
		labs(y='Mentions', x='Year', title='policy') + guides(fill=FALSE)
g4<-ggplot(means, aes(factor(year), twitter, fill=OA)) + geom_boxplot() + 
		labs(y='Mentions', x='Year', title='twitter') + theme(legend.position='right')

pdf(file='figures/exploratory/altmetric/raw_mentions_years.pdf', height=7, width=11)

grid.arrange(g1,g2,g3,g4)

## truncate ylims to remove highly mentioned papers
g1<-g1+lims(y=c(0,3))
g2<-g2+lims(y=c(0,5))
g3<-g3+lims(y=c(0,1))
g4<-g4+lims(y=c(0,30))
grid.arrange(g1,g2,g3,g4)

dev.off()


## Mentions should be scaled and centered
## Outliers may be problematic = papers with very high media reporting tend to swamp mean trends

## scale + center
meansS<-means %>% mutate(blogS = scale(blog),
						 newsS = scale(news),
						 policyS = scale(policy),
						 twitterS = scale(twitter))

g1<-ggplot(meansS,aes(factor(year), blogS, fill=OA)) + geom_boxplot() + 
		labs(y='Mentions', x='Year', title='blog') + guides(fill=FALSE)
g2<-ggplot(meansS, aes(factor(year), newsS, fill=OA)) + geom_boxplot() + 
		labs(y='Mentions', x='Year', title='news') + guides(fill=FALSE)
g3<-ggplot(meansS, aes(factor(year), policyS, fill=OA)) + geom_boxplot() + 
		labs(y='Mentions', x='Year', title='policy') + guides(fill=FALSE)
g4<-ggplot(meansS, aes(factor(year), twitterS, fill=OA)) + geom_boxplot() + 
		labs(y='Mentions', x='Year', title='twitter') + theme(legend.position='right')

pdf(file='figures/exploratory/altmetric/scaled_mentions_years.pdf', height=7, width=11)
grid.arrange(g1,g2,g3,g4)
dev.off()


## Also need to consider impact factor effect. 
m1<-glmer(News.mentions ~ SJR * OA + (1 | year) + (1 | Journal), alt, family='poisson')
summary(m1)


