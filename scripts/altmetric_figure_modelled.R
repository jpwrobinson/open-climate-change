

rm(list=ls())
setwd("~/GitHub/open-climate-change/")
setwd('/Users/robins64/Documents/git_repos/open-climate-change')
se<-function(x) sd(x)/sqrt(length(x))
library(here); library(dplyr); library(tidyr); library(ggplot2); theme_set(theme_bw()); library(plotrix)

add_label <- function(xfrac, yfrac, label, pos = 4, ...){
  u <- par("usr")
  x <- u[1] + xfrac * (u[2] - u[1])
  y <- u[4] - yfrac * (u[4] - u[3])
  text(x, y, label, pos = pos, ...)
}


load(file='Data/altmetric_OA_clean.Rdata')
names(alt)

## Summary stats for journals per bin
unique(alt$Journal[alt$SJRfac=='A']) ## n = 45
aggregate(Journal ~ OA, alt[alt$SJRfac=='A',], function(x)length(unique(x))) ## 12 open, 40 closed

unique(alt$Journal[alt$SJRfac=='B']) ## n = 55
aggregate(Journal ~ OA, alt[alt$SJRfac=='B',], function(x)length(unique(x))) ## 20 open, 50 closed

unique(alt$Journal[alt$SJRfac=='C']) ## n = 51
aggregate(Journal ~ OA, alt[alt$SJRfac=='C',], function(x)length(unique(x))) ## 28 open, 44 closed

unique(alt$Journal[alt$SJRfac=='D']) ## n = 47
aggregate(Journal ~ OA, alt[alt$SJRfac=='D',], function(x)length(unique(x))) ## 36 open, 22 closed


t<-alt[alt$Policy.mentions>0,]
# ggplot(t, aes(year, Policy.mentions, col=OA))+ geom_point()

dim(t[t$OA=='TRUE',]) ## 140 OA policy mentions

## check number of pubs per journal per year
alt <- alt %>% group_by(year, Journal) %>% mutate(nNews = length(News.mentions[News.mentions > 0]),
												  nTwitter = length(Twitter.mentions[Twitter.mentions>0]), 
												  nPolicy = length(Policy.mentions[Policy.mentions>0]))




## get mean mentions by OA + bin
news.dat<-alt %>% mutate(OA = ifelse(OA == TRUE, 'Open', 'Closed')) %>%
	# filter(nNews > 2) %>%
	group_by(SJRfac, OA, Journal, year) %>% 
	# complete(Journal, nesting(source, OA, SJRfac, year), fill=list(News.mentions=0, )) %>% 
	summarise(news = mean(News.mentions), 
				policy=mean(Policy.mentions), 
				twitter=mean(Twitter.mentions)) %>%
	gather(source, mentions, -SJRfac, -OA, -Journal, -year) #%>%
	# mutate(mentions10 = log10(mentions+1))

## add zeroes for cases with no mentions in a given year * journal quantile
news.dat$mentions[is.na(news.dat$mentions)]<-0

## or drop all NAs
news.dat<-news.dat[news.dat$mentions!=0,]
news.dat$SJRfac.scaled<-scale(as.numeric(news.dat$SJRfac))
news.dat$mentions10<-log10(news.dat$mentions)


## lots of years dropped when n papers is low
# with(news.dat, table(Journal))
# data.frame(alt[alt$Journal == 'Ecology Letters',])

ggplot(news.dat, aes(SJRfac, mentions, fill=OA)) + geom_boxplot() + 
		facet_wrap(~source, scales='free') +
		scale_y_log10()

### gather data for base figure
# news.dat<-alt %>% mutate(OA = ifelse(OA == TRUE, 'Open', 'Closed')) %>%
# 	select(News.mentions, Policy.mentions, Twitter.mentions, OA, SJRfac, year, Journal) %>%
# 	gather(source, mentions, -SJRfac, -OA, -year, -Journal) 

library(lme4)

news<-lmer(mentions10 ~ OA * SJRfac.scaled + (1 | year) + (1 | Journal), 
			news.dat[news.dat$source=='news',], )

# ## trying dummy approach
# news.dat$Adummy<-ifelse(news.dat$SJRfac == 'A', 1, 0)
# news.dat$Bdummy<-ifelse(news.dat$SJRfac == 'B', 1, 0)
# news.dat$Cdummy<-ifelse(news.dat$SJRfac == 'C', 1, 0)
# news.dat$Ddummy<-ifelse(news.dat$SJRfac == 'D', 1, 0)


# news1<-lmer(mentions10 ~ OA * Adummy + OA*Bdummy + OA*Cdummy + OA*Ddummy + (1 | year) + (1 | Journal), 
# 			news.dat[news.dat$source=='news',], )

# news2<-lmer(mentions10 ~ OA * SJRfac + (1 | year) + (1 | Journal), 
# 			news.dat[news.dat$source=='news',], )


 # plot(fitted(news), fitted(news2))


summary(news)
hist(resid(news))
plot(fitted(news), news.dat$mentions10[news.dat$source=='news'], pch=21, 
				bg=news.dat$SJRfac[news.dat$source=='news'])
legend('bottomright', legend= c('A', 'B', 'C', 'D'), pt.bg = 1:4, pch = 21)


news.dat<-expand.grid(OA = unique(news.dat$OA), SJRfac.scaled=unique(news.dat$SJRfac.scaled), year = 2010, Journal='Nature')
news.dat$p<-predict(news, newdat=news.dat, re.form=NA, type='response')
news.dat$p10<-10^news.dat$p
news.dat$source<-'news'
ggplot(news.dat, aes(SJRfac.scaled, 10^p, col=OA)) + geom_point() + labs(y = 'news mentions')


## get mean mentions by OA + bin
twitter.dat<-alt %>% mutate(OA = ifelse(OA == TRUE, 'Open', 'Closed')) %>%
	# filter(nNews > 2) %>%
	group_by(SJRfac, OA, Journal, year) %>% 
	# complete(Journal, nesting(source, OA, SJRfac, year), fill=list(News.mentions=0, )) %>% 
	summarise(news = mean(News.mentions), 
				policy=mean(Policy.mentions), 
				twitter=mean(Twitter.mentions)) %>%
	gather(source, mentions, -SJRfac, -OA, -Journal, -year)# %>%
	# mutate(mentions10 = log10(mentions+1))

## add zeroes for cases with no mentions in a given year * journal quantile
twitter.dat$mentions[is.na(twitter.dat$mentions)]<-0

## or drop all NAs
twitter.dat<-twitter.dat[twitter.dat$mentions!=0,]
twitter.dat$SJRfac.scaled<-scale(as.numeric(twitter.dat$SJRfac))
twitter.dat$mentions10<-log10(twitter.dat$mentions)

twitter<-lmer(mentions10 ~ OA * SJRfac.scaled + (1 | year) + (1 | Journal),
			twitter.dat[twitter.dat$source=='twitter',])
summary(twitter)
hist(resid(twitter))
plot(fitted(twitter), twitter.dat$mentions10[twitter.dat$source=='twitter'], 
	pch=as.numeric(as.factor(twitter.dat$OA[twitter.dat$source=='twitter'])), 
				col=twitter.dat$SJRfac[twitter.dat$source=='twitter'])
legend('bottomright', legend= c('A', 'B', 'C', 'D', 'Open', 'Closed'), col = c(1:4, 1, 1), pch = c(1,1,1,1,2))
abline(0,1)


## trying dummy approach
twitter.dat$Adummy<-ifelse(twitter.dat$SJRfac == 'A', 1, 0)
twitter.dat$Bdummy<-ifelse(twitter.dat$SJRfac == 'B', 1, 0)
twitter.dat$Cdummy<-ifelse(twitter.dat$SJRfac == 'C', 1, 0)
twitter.dat$Ddummy<-ifelse(twitter.dat$SJRfac == 'D', 1, 0)


twitter1<-lmer(mentions10 ~ OA * Adummy + OA*Bdummy + OA*Cdummy + OA*Ddummy + (1 | year) + (1 | Journal), 
			twitter.dat[twitter.dat$source=='twitter',], )

twitter2<-lmer(mentions10 ~ OA * SJRfac + (1 | year) + (1 | Journal), 
			twitter.dat[twitter.dat$source=='twitter',], )

par(mfrow=c(1,2))
 plot(fitted(twitter), fitted(twitter1), ylab='Dummy approach', xlab='Continuous approach')
 plot(fitted(twitter), fitted(twitter2), ylab='Factor approach', xlab='Continuous approach')


twitter.dat<-expand.grid(OA = unique(twitter.dat$OA), SJRfac.scaled=unique(twitter.dat$SJRfac.scaled), year = 2010, Journal='Nature')
twitter.dat$p<-predict(twitter, newdat=twitter.dat, re.form=NA, type='response')
twitter.dat$p10<-10^twitter.dat$p
twitter.dat$source<-'twitter'
ggplot(twitter.dat, aes(SJRfac.scaled, 10^p, col=OA)) + geom_point() + labs(y = 'twitter mentions')


## get mean mentions by OA + bin
policy.dat<-alt %>% mutate(OA = ifelse(OA == TRUE, 'Open', 'Closed')) %>%
	# filter(nNews > 2) %>%
	group_by(SJRfac, OA, Journal, year) %>% 
	# complete(Journal, nesting(source, OA, SJRfac, year), fill=list(News.mentions=0, )) %>% 
	summarise(news = mean(News.mentions), 
				policy=mean(Policy.mentions), 
				twitter=mean(Twitter.mentions)) %>%
	gather(source, mentions, -SJRfac, -OA, -Journal, -year) #%>%
	# mutate(mentions10 = log10(mentions+1))

## add zeroes for cases with no mentions in a given year * journal quantile
policy.dat$mentions[is.na(policy.dat$mentions)]<-0
policy.dat$SJRfac.scaled<-scale(as.numeric(policy.dat$SJRfac))

## or drop all NAs
policy.dat<-policy.dat[policy.dat$mentions!=0,]

policy.dat$mentions10<-log10(policy.dat$mentions)

# ggplot(policy.dat, aes(year, log10(mentions), col=OA)) + geom_point() + facet_wrap(~SJRfac)

policy<-lmer(mentions10 ~ OA * SJRfac.scaled + (1 | year) + (1 | Journal),
			policy.dat[policy.dat$source=='policy',])
hist(resid(policy))
plot(fitted(policy), policy.dat$mentions10[policy.dat$source=='policy'])

policy.dat<-expand.grid(OA = unique(policy.dat$OA), SJRfac.scaled=unique(policy.dat$SJRfac.scaled), year = 2010, Journal='Nature')
policy.dat$p<-predict(policy, newdat=policy.dat, re.form=NA, type='response')
policy.dat$p10<-10^policy.dat$p
policy.dat$source<-'policy'
ggplot(policy.dat, aes(SJRfac.scaled, 10^p, col=OA)) + geom_point() + labs(y = 'policy mentions')

ratio.plot<-rbind(news.dat, twitter.dat, policy.dat)
ratio.plot$xlim<-ifelse(ratio.plot$OA=='Open', as.numeric(ratio.plot$SJRfac.scaled)+0.1, as.numeric(ratio.plot$SJRfac)-0.1)

save(ratio.plot, news, twitter, policy, alt, file='Data/altmetric_glmer_fit.Rdata')


## plot
pdf(file='figures/X_fig_altmetric_modelled.pdf', height=2, width=8)
layout(matrix(c(1,2,3), nrow=1))
cols<-c('#d8b365', '#5ab4ac')
cx.ax=0.9
par(mar=c(2.5,4,2,0))
## news
with(ratio.plot[ratio.plot$source=='news' & ratio.plot$OA=='Closed',], 
		plotCI(xlim, p, ui=p, li=p, pch=19,cex=1.5, sfrac=0, 
			axes=F, xlim=c(-1.5, 1.5), xlab='', ylab='', ylim=c(0, 0.7),
			scol=cols[1], col=cols[1]))
with(ratio.plot[ratio.plot$source=='news' & ratio.plot$OA=='Open',], 
		plotCI(xlim, p, ui=p, li=p, pch=19,cex=1.5, sfrac=0,
			scol=cols[2], col=cols[2], add=TRUE))
axis(1, at=unique(ratio.plot$SJRfac.scaled), labels=c('Low', 'Medium', 'High', 'Very high'), cex.axis=cx.ax)
axis(2, cex.axis=cx.ax)
add_label(0.01, 0.1, 'News', font=2, cex=1)
mtext(2, text='Mean mentions', line=2.5, cex=0.8)

par(mar=c(2.5,2,2,2))
## policy
with(ratio.plot[ratio.plot$source=='policy' & ratio.plot$OA=='Closed',], 
		plotCI(xlim, p, ui=p, li=p, pch=19,cex=1.5, sfrac=0, 
			axes=F, xlim=c(-1.5, 1.5), xlab='', ylab='', ylim=c(0,0.7),
			scol=cols[1], col=cols[1]))
with(ratio.plot[ratio.plot$source=='policy' & ratio.plot$OA=='Open',], 
		plotCI(xlim, p, ui=p, li=p, pch=19,cex=1.5, sfrac=0,
			scol=cols[2], col=cols[2], add=TRUE))
axis(1, at=unique(ratio.plot$SJRfac.scaled), labels=c('Low', 'Medium', 'High', 'Very high'), cex.axis=cx.ax)
axis(2, cex.axis=cx.ax)
add_label(0.01, 0.1, 'Policy', font=2, cex=1)

par(mar=c(2.5,0,2,4))
## twitter
with(ratio.plot[ratio.plot$source=='twitter' & ratio.plot$OA=='Closed',], 
		plotCI(xlim, p, ui=p, li=p, pch=19,cex=1.5, sfrac=0, 
			axes=F, xlim=c(-1.5, 1.5), xlab='', ylab='', ylim=c(0,10),
			scol=cols[1], col=cols[1]))
with(ratio.plot[ratio.plot$source=='twitter' & ratio.plot$OA=='Open',], 
		plotCI(xlim, p, ui=p, li=p, pch=19,cex=1.5, sfrac=0,
			scol=cols[2], col=cols[2], add=TRUE))
axis(1, at=unique(ratio.plot$SJRfac.scaled), labels=c('Low', 'Medium', 'High', 'Very high'), cex.axis=cx.ax)
axis(2, cex.axis=cx.ax)
add_label(0.01, 0.1, 'Twitter', font=2, cex=1)

par(xpd=T)
legend('topright', legend=c('Open', 'Closed'), 
	col=rev(cols), pch=19, bty='n', inset=c(-0.2, 0), cex=0.9)

dev.off()