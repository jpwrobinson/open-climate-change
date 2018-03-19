

rm(list=ls())
setwd("~/GitHub/open-climate-change/")
setwd('/Users/robins64/Documents/git_repos/open-climate-change')

se<-function(x) sd(x)/sqrt(length(x))
library(here); library(dplyr); library(tidyr); library(plotrix)
library(lme4)

add_label <- function(xfrac, yfrac, label, pos = 4, ...){
  u <- par("usr")
  x <- u[1] + xfrac * (u[2] - u[1])
  y <- u[4] - yfrac * (u[4] - u[3])
  text(x, y, label, pos = pos, ...)
}



## load scopus data
load('Data/scopus_glmerfit.Rdata')
pred<-expand.grid(OA =unique(mod.dat$OA), jour.bin=unique(mod.dat$jour.bin), Year = 2007, Source.title='Nature')
pred$p<-predict(fit5a, newdata=pred, re.form = NA)
pred$xlim<-ifelse(pred$OA==TRUE, as.numeric(pred$jour.bin)+0.1, as.numeric(pred$jour.bin)-0.1)

## load altmetric data
load('Data/altmetric_glmer_fit.Rdata')



## plot
pdf(file='figures/Figure1.pdf', height=2, width=8)
layout(matrix(c(1,2,3,4), nrow=1))
cols<-c('#d8b365', '#5ab4ac')
cx.ax=0.9
par(mar=c(2.5,4,2,0))

## scopus
with(pred[pred$OA==FALSE,], 
		plotCI(xlim, 10^p, ui=10^p, li=10^p, pch=19,cex=1.5, sfrac=0, 
			axes=F, xlim=c(0.75, 4.25), xlab='', ylab='', ylim=c(0, 60),
			scol=cols[1], col=cols[1]))
with(pred[pred$OA==TRUE,], 
		plotCI(xlim, 10^p, ui=10^p, li=10^p, pch=19,cex=1.5, sfrac=0,
			scol=cols[2], col=cols[2], add=TRUE))
axis(1, at=c(1:4), labels=c('Low', 'Medium', 'High', 'Very high'), cex.axis=cx.ax)
axis(2, cex.axis=cx.ax)
add_label(0.01, 0.1, 'Scopus citations', font=2, cex=1)
mtext(2, text='Mean citations/mentions', line=2.5, cex=0.8)

par(mar=c(2.5,2,2,2))
## news
with(ratio.plot[ratio.plot$source=='news' & ratio.plot$OA=='Closed',], 
		plotCI(xlim, p, ui=p, li=p, pch=19,cex=1.5, sfrac=0, 
			axes=F, xlim=c(0.75, 4.25), xlab='', ylab='', ylim=c(0, 0.7),
			scol=cols[1], col=cols[1]))
with(ratio.plot[ratio.plot$source=='news' & ratio.plot$OA=='Open',], 
		plotCI(xlim, p, ui=p, li=p, pch=19,cex=1.5, sfrac=0,
			scol=cols[2], col=cols[2], add=TRUE))
axis(1, at=c(1:4), labels=c('Low', 'Medium', 'High', 'Very high'), cex.axis=cx.ax)
axis(2, cex.axis=cx.ax)
add_label(0.01, 0.1, 'News', font=2, cex=1)
# mtext(2, text='Mean mentions', line=2.5, cex=0.8)

par(mar=c(2.5,2,2,2))
## policy
with(ratio.plot[ratio.plot$source=='policy' & ratio.plot$OA=='Closed',], 
		plotCI(xlim, p, ui=p, li=p, pch=19,cex=1.5, sfrac=0, 
			axes=F, xlim=c(0.75, 4.25), xlab='', ylab='', ylim=c(0,0.7),
			scol=cols[1], col=cols[1]))
with(ratio.plot[ratio.plot$source=='policy' & ratio.plot$OA=='Open',], 
		plotCI(xlim, p, ui=p, li=p, pch=19,cex=1.5, sfrac=0,
			scol=cols[2], col=cols[2], add=TRUE))
axis(1, at=c(1:4), labels=c('Low', 'Medium', 'High', 'Very high'), cex.axis=cx.ax)
axis(2, cex.axis=cx.ax)
add_label(0.01, 0.1, 'Policy', font=2, cex=1)

par(mar=c(2.5,1,2,3))
## twitter
with(ratio.plot[ratio.plot$source=='twitter' & ratio.plot$OA=='Closed',], 
		plotCI(xlim, p, ui=p, li=p, pch=19,cex=1.5, sfrac=0, 
			axes=F, xlim=c(0.75, 4.25), xlab='', ylab='', ylim=c(0,10),
			scol=cols[1], col=cols[1]))
with(ratio.plot[ratio.plot$source=='twitter' & ratio.plot$OA=='Open',], 
		plotCI(xlim, p, ui=p, li=p, pch=19,cex=1.5, sfrac=0,
			scol=cols[2], col=cols[2], add=TRUE))
axis(1, at=c(1:4), labels=c('Low', 'Medium', 'High', 'Very high'), cex.axis=cx.ax)
axis(2, cex.axis=cx.ax)
add_label(0.01, 0.1, 'Twitter', font=2, cex=1)

par(xpd=T)
legend('topright', legend=c('Open', 'Closed'), 
	col=rev(cols), pch=19, bty='n', inset=c(-0.2, 0), cex=0.9)

dev.off()