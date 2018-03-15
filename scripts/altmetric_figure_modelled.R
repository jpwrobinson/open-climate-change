

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
unique(alt$Journal[alt$SJRfac=='A']) ## n = 24
aggregate(Journal ~ OA, alt[alt$SJRfac=='A',], function(x)length(unique(x))) ## 12 open, 22 closed

unique(alt$Journal[alt$SJRfac=='B']) ## n = 29
aggregate(Journal ~ OA, alt[alt$SJRfac=='B',], function(x)length(unique(x))) ## 16 open, 24 closed

unique(alt$Journal[alt$SJRfac=='C']) ## n = 21
aggregate(Journal ~ OA, alt[alt$SJRfac=='C',], function(x)length(unique(x))) ## 14 open, 17 closed

unique(alt$Journal[alt$SJRfac=='D']) ## n = 23
aggregate(Journal ~ OA, alt[alt$SJRfac=='D',], function(x)length(unique(x))) ## 10 open, 20 closed


t<-alt[alt$Policy.mentions>0,]
ggplot(t, aes(year, Policy.mentions, col=OA))+ geom_point()

dim(t[t$OA=='TRUE',]) ## 112 OA policy mentions

## get mean mentions by OA + bin
ratio<-alt %>% mutate(OA = ifelse(OA == TRUE, 'Open', 'Closed')) %>%
	group_by(SJRfac, OA) %>% 
	summarise(news = mean(News.mentions), policy=mean(Policy.mentions), twitter=mean(Twitter.mentions)) %>%
	gather(source, mentions, -SJRfac, -OA) %>% 
	spread(OA, mentions) 

## add zeroes for cases with no mentions in a given year * journal quantile
ratio$Open[is.na(ratio$Open)]<-0
ratio$ratio<-with(ratio, Open/Closed)

# ggplot(ratio, aes(SJRfac, Closed, col=source))+geom_point() + facet_wrap (~source, scales='free') + 
# 		geom_point(aes(SJRfac, Open, col=source), shape=2)


### gather data for base figure
ratio<-alt %>% mutate(OA = ifelse(OA == TRUE, 'Open', 'Closed')) %>%
	select(News.mentions, Policy.mentions, Twitter.mentions, OA, SJRfac, year, Journal) %>%
	gather(source, mentions, -SJRfac, -OA, -year, -Journal) 

library(lme4)
news<-glmer(log10(mentions+1) ~ OA * SJRfac + (1 | year) + (1 | Journal),family='Gamma', 
			ratio[ratio$source=='News.mentions',])
twitter<-glmer(log10(mentions+1) ~ OA * SJRfac + (1 | year) + (1 | Journal),family='Gamma', 
			ratio[ratio$source=='Twitter.mentions',])
policy<-glmer(log10(mentions+1) ~ OA * SJRfac + (1 | year) + (1 | Journal),family='Gamma', 
			ratio[ratio$source=='Policy.mentions',])


ratio$xlim<-ifelse(ratio$OA=='Open', as.numeric(ratio$SJRfac)+0.1, as.numeric(ratio$SJRfac)-0.1)

## plot
pdf(file='figures/X_fig_altmetric.pdf', height=2, width=8)
layout(matrix(c(1,2,3), nrow=1))
cols<-c('#d8b365', '#5ab4ac')
cx.ax=0.9
par(mar=c(2.5,4,2,0))
## news
with(ratio[ratio$source=='News.mentions' & ratio$OA=='Closed',], 
		plotCI(xlim, mean, ui=mean+2*se, li=mean-2*se, pch=19,cex=1.5, sfrac=0, 
			axes=F, xlim=c(0.75, 4.25), xlab='', ylab='',
			scol=cols[1], col=cols[1]))
with(ratio[ratio$source=='News.mentions' & ratio$OA=='Open',], 
		plotCI(xlim, mean, ui=mean+2*se, li=mean-2*se, pch=19,cex=1.5, sfrac=0,
			scol=cols[2], col=cols[2], add=TRUE))
axis(1, at=c(1:4), labels=c('Low', 'Medium', 'High', 'Very high'), cex.axis=cx.ax)
axis(2, cex.axis=cx.ax)
add_label(0.01, 0.1, 'News', font=2, cex=1)
mtext(2, text='Mean mentions', line=2.5, cex=0.8)

par(mar=c(2.5,2,2,2))
## policy
with(ratio[ratio$source=='Policy.mentions' & ratio$OA=='Closed',], 
		plotCI(xlim, mean, ui=mean+2*se, li=mean-2*se, pch=19,cex=1.5, sfrac=0, 
			axes=F, xlim=c(0.75, 4.25), xlab='', ylab='', ylim=c(0,0.7),
			scol=cols[1], col=cols[1]))
with(ratio[ratio$source=='Policy.mentions' & ratio$OA=='Open',], 
		plotCI(xlim, mean, ui=mean+2*se, li=mean-2*se, pch=19,cex=1.5, sfrac=0,
			scol=cols[2], col=cols[2], add=TRUE))
axis(1, at=c(1:4), labels=c('Low', 'Medium', 'High', 'Very high'), cex.axis=cx.ax)
axis(2, cex.axis=cx.ax)
add_label(0.01, 0.1, 'Policy', font=2, cex=1)

par(mar=c(2.5,0,2,4))
## twitter
with(ratio[ratio$source=='Twitter.mentions' & ratio$OA=='Closed',], 
		plotCI(xlim, mean, ui=mean+2*se, li=mean-2*se, pch=19,cex=1.5, sfrac=0, 
			axes=F, xlim=c(0.75, 4.25), xlab='', ylab='', ylim=c(0,30),
			scol=cols[1], col=cols[1]))
with(ratio[ratio$source=='Twitter.mentions' & ratio$OA=='Open',], 
		plotCI(xlim, mean, ui=mean+2*se, li=mean-2*se, pch=19,cex=1.5, sfrac=0,
			scol=cols[2], col=cols[2], add=TRUE))
axis(1, at=c(1:4), labels=c('Low', 'Medium', 'High', 'Very high'), cex.axis=cx.ax)
axis(2, cex.axis=cx.ax)
add_label(0.01, 0.1, 'Twitter', font=2, cex=1)

par(xpd=T)
legend('bottomright', legend=c('Open', 'Closed'), 
	col=rev(cols), pch=19, bty='n', inset=c(-0.2, 0), cex=0.9)

dev.off()