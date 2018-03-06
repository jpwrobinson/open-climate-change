

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
		summarise(attention = mean(Altmetric.Attention.Score),
				  news = mean(News.mentions),
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


## Mentions could be scaled and centered?
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
## Modelling raw mentions with year and journal as random effects
### ONLY PREDICT OPEN ACCESS EFFECT OVER OA JOURNAL IMPACT FACTOR RANGE

pdf(file='figures/exploratory/altmetric/predicted_mentions.pdf', height=7, width=11)
par(mfrow=c(2,2), mar=c(4,4,1,1))

## Total mentions
m1<-glmer(Altmetric.Attention.Score ~ SJR * OA + (1 | year) + (1 | Journal), alt, family='poisson')
summary(m1)

pred.dat<-expand.grid(SJR=seq(1,max(alt$SJR), 1), year=2011, OA=c('TRUE', 'FALSE'), Journal='Nature Climate Change')

## predict dropping ranefs
p<-predict(m1, newdata=pred.dat, re.form=NA, type='response', se=TRUE)
plot(1:6, p[1:6], type='l', col='darkblue', lwd=2, xlab='Impact factor', ylab='Altmetric Attention Score',ylim=c(0,100), xlim=c(0, 19))
lines(1:18, p[19:36], type='l', col='darkred', lwd=2)
legend('topleft', legend=c('Open', 'Closed'), lty=1, col=c('darkblue', 'darkred'), bty='n')


## News mentions
m1<-glmer(News.mentions ~ SJR * OA + (1 | year) + (1 | Journal), alt, family='poisson')
summary(m1)

pred.dat<-expand.grid(SJR=seq(1,max(alt$SJR), 1), year=2011, OA=c('TRUE', 'FALSE'), Journal='Nature Climate Change')

## predict dropping ranefs
p<-predict(m1, newdata=pred.dat, re.form=NA, type='response', se=TRUE)
plot(1:6, p[1:6], type='l', col='darkblue', lwd=2, xlab='Impact factor', ylab='News mentions', xlim=c(0, 19), ylim=c(0,1))
lines(1:18, p[19:36], type='l', col='darkred', lwd=2)
#legend('topleft', legend=c('Open', 'Closed'), lty=1, col=c('darkblue', 'darkred'), bty='n')


## Twitter mentions
m1<-glmer(Twitter.mentions ~ SJR * OA + (1 | year) + (1 | Journal), alt, family='poisson')
summary(m1)

pred.dat<-expand.grid(SJR=seq(1,max(alt$SJR), 1), year=2011, OA=c('TRUE', 'FALSE'), Journal='Nature Climate Change')

## predict dropping ranefs
p<-predict(m1, newdata=pred.dat, re.form=NA, type='response', se=TRUE)
plot(1:6, p[1:6], type='l', col='darkblue', lwd=2, xlab='Impact factor', ylab='Twitter mentions', ylim=c(0, 100), xlim=c(0, 19))
lines(1:18, p[19:36], type='l', col='darkred', lwd=2)
#legend('topleft', legend=c('Open', 'Closed'), lty=1, col=c('darkblue', 'darkred'), bty='n')

## Policy mentions
m1<-glmer(Policy.mentions ~ SJR * OA + (1 | year) + (1 | Journal), alt, family='poisson')
summary(m1)

pred.dat<-expand.grid(SJR=seq(1,max(alt$SJR), 1), year=2011, OA=c('TRUE', 'FALSE'), Journal='Nature Climate Change')

## predict dropping ranefs
p<-predict(m1, newdata=pred.dat, re.form=NA, type='response', se=TRUE)
plot(1:6, p[1:6], type='l', col='darkblue', lwd=2, xlab='Impact factor', ylab='Policy mentions', ylim=c(0,2), xlim=c(0, 19))
lines(1:18, p[19:36], type='l', col='darkred', lwd=2)
#legend('topleft', legend=c('Open', 'Closed'), lty=1, col=c('darkblue', 'darkred'), bty='n')


dev.off()

## Models are bad at predicting outliers (high media attention)

## Simplify outliers using mean mentions by year and impact bins
### estimate mentions ratio for closed:OA

## bin journals by impact factor
jour.dat<-alt[,c("Journal","SJR", 'OA')]
jour.dat<-jour.dat[-which(duplicated(jour.dat)),]


bins<-quantile(jour.dat$SJR,na.rm=T)  ##quantile bins
jour.dat$SJRfac<-cut(jour.dat$SJR,breaks = bins,labels = LETTERS[1:(length(bins)-1)])
jour.dat$SJRfac[which(is.na(jour.dat$SJRfac))]<-"A"
write.csv(jour.dat, 'Data/altmetric_binning.csv')

ratio<-alt %>% mutate(OA = ifelse(OA == TRUE, 'Open', 'Closed')) %>%
	group_by(year, SJRfac, OA) %>% 
	summarise(news = mean(News.mentions), policy=mean(Policy.mentions), twitter=mean(Twitter.mentions)) %>%
	gather(source, mentions, -year, -SJRfac, -OA) %>% 
	spread(OA, mentions) 

## add zeroes for cases with no mentions in a given year * journal quantile
ratio$Open[is.na(ratio$Open)]<-0
ratio$ratio<-with(ratio, Open/Closed)

## There are no open journals in the top impact bin
unique(alt$Journal[alt$SJRfac=='A']) ## n = 48
aggregate(Journal ~ OA, alt[alt$SJRfac=='A',], function(x)length(unique(x))) ## 23 open, 40 closed

unique(alt$Journal[alt$SJRfac=='B']) ## n = 14
aggregate(Journal ~ OA, alt[alt$SJRfac=='B',], function(x)length(unique(x))) ## 10 open, 13 closed

unique(alt$Journal[alt$SJRfac=='C']) ## n = 27
aggregate(Journal ~ OA, alt[alt$SJRfac=='C',], function(x)length(unique(x))) ## 18 open, 22 closed

unique(alt$Journal[alt$SJRfac=='D']) ## n = 15
aggregate(Journal ~ OA, alt[alt$SJRfac=='D',], function(x)length(unique(x))) ## 3 open, 13 closed

# drop top journals
# ratio <- ratio[!ratio$SJRfac=='D',]

pdf(file='figures/exploratory/altmetric/mean_mentions_impactbins.pdf', height=7, width=11)

ggplot(ratio, aes(year, ratio, col=SJRfac)) + geom_point() + 
		facet_wrap(~source, scales='free') + labs(y = 'Relative mentions (Open : Closed)')


ggplot(ratio, aes(year+0.1, Closed, col=SJRfac)) + geom_point() + 
		geom_point(data=ratio, aes(year, Open), shape=2) +
		facet_wrap(SJRfac~source, scales='free') + labs(y = 'Mean mentions')


dev.off()



# Reduce effect of outliers for better model fits.
## Modelling mean mentions by journal in each year. 
### ONLY PREDICT OPEN ACCESS EFFECT OVER OA JOURNAL IMPACT FACTOR RANGE

pdf(file='figures/exploratory/altmetric/predicted_mentions_means.pdf', height=7, width=11)
par(mfrow=c(2,2), mar=c(4,4,1,1))

## Total mentions
m1<-lmer(log10(attention+1) ~ SJR * OA + (1 | year) + (1 | Journal), means)
summary(m1)

pred.dat<-expand.grid(SJR=seq(1,max(means$SJR), 1), year=2011, OA=c('TRUE', 'FALSE'), Journal='Nature Climate Change')
pred.dat<-pred.dat[!(pred.dat$SJR > 6 & pred.dat$OA=='TRUE'),]

## predict dropping ranefs
p<-predict(m1, newdata=pred.dat, re.form=NA, type='response')
plot(1:6, 10^p[1:6], type='l', col='darkblue', lwd=2, xlab='Impact factor', ylab='Altmetric Attention Score',ylim=c(0,170), xlim=c(0, 19))
lines(1:18, 10^p[7:24], type='l', col='darkred', lwd=2)
legend('topleft', legend=c('Open', 'Closed'), lty=1, col=c('darkblue', 'darkred'), bty='n')


## News mentions
m1<-lmer(log10(news+1) ~ SJR * OA + (1 | year) + (1 | Journal), means)
summary(m1)

pred.dat<-expand.grid(SJR=seq(1,max(means$SJR), 1), year=2011, OA=c('TRUE', 'FALSE'), Journal='Nature Climate Change')
pred.dat<-pred.dat[!(pred.dat$SJR > 6 & pred.dat$OA=='TRUE'),]

## predict dropping ranefs
p<-predict(m1, newdata=pred.dat, re.form=NA, type='response')
plot(1:6, 10^p[1:6], type='l', col='darkblue', lwd=2, xlab='Impact factor', ylab='News mentions',ylim=c(0,10), xlim=c(0, 19))
lines(1:18, 10^p[7:24], type='l', col='darkred', lwd=2)
# legend('topleft', legend=c('Open', 'Closed'), lty=1, col=c('darkblue', 'darkred'), bty='n')


## Twitter mentions
m1<-lmer(log10(twitter+1) ~ SJR * OA + (1 | year) + (1 | Journal), means)
summary(m1)

pred.dat<-expand.grid(SJR=seq(1,max(means$SJR), 1), year=2011, OA=c('TRUE', 'FALSE'), Journal='Nature Climate Change')
pred.dat<-pred.dat[!(pred.dat$SJR > 6 & pred.dat$OA=='TRUE'),]

## predict dropping ranefs
p<-predict(m1, newdata=pred.dat, re.form=NA, type='response')
plot(1:6, 10^p[1:6], type='l', col='darkblue', lwd=2, xlab='Impact factor', ylab='Twitter mentions',ylim=c(0,40), xlim=c(0, 19))
lines(1:18, 10^p[7:24], type='l', col='darkred', lwd=2)
# legend('topleft', legend=c('Open', 'Closed'), lty=1, col=c('darkblue', 'darkred'), bty='n')

## Policy mentions
m1<-lmer(log10(policy+1) ~ SJR * OA + (1 | year) + (1 | Journal), means)
summary(m1)

pred.dat<-expand.grid(SJR=seq(1,max(means$SJR), 1), year=2011, OA=c('TRUE', 'FALSE'), Journal='Nature Climate Change')
pred.dat<-pred.dat[!(pred.dat$SJR > 6 & pred.dat$OA=='TRUE'),]

## predict dropping ranefs
p<-predict(m1, newdata=pred.dat, re.form=NA, type='response')
plot(1:6, 10^p[1:6], type='l', col='darkblue', lwd=2, xlab='Impact factor', ylab='Policy mentions',ylim=c(0,1.5), xlim=c(0, 19))
lines(1:18, 10^p[7:24], type='l', col='darkred', lwd=2)
# legend('topleft', legend=c('Open', 'Closed'), lty=1, col=c('darkblue', 'darkred'), bty='n')


dev.off()

