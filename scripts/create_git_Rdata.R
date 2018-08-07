rm(list=ls())
setwd("~/GitHub/open-climate-change/")
setwd('/Users/robins64/Documents/git_repos/open-climate-change')

bin.labs<-c('Low', 'Medium', 'High', 'Very high')

load('Data/scopus_glmerfit.Rdata')

m.citations<-fit5a


pred<-expand.grid(OA =unique(mod.dat$OA), jour.bin.scaled=unique(mod.dat$jour.bin.scaled), Year = 2007, Source.title='Nature')
pred$log10predicted.citation<-predict(fit5a, newdata=pred, re.form = NA)
pred$predicted.citation<-10^pred$log10predicted.citation

pred$Source.title<-NULL
pred$Year<-NULL
# pred$jour.bin.scaled<-NULmL
pred$SJRbin<-rep(bin.labs, each =2)

save(m.citations, pred, file='Data/scopus_lm_forgit.Rdata')


rm(list=ls())
load('Data/altmetric_glmer_fit.Rdata')

## fix xlim for policy
ratio.plot$xlim[ratio.plot$source=='policy']<-ratio.plot$xlim[ratio.plot$source=='news']
ratio.plot$SJRfac.scaled[ratio.plot$source=='policy']<-ratio.plot$SJRfac.scaled[ratio.plot$source=='news']


## rename to useful names
m.news<-news
m.twitter<-twitter
m.policy<-policy
predicted.mentions<-ratio.plot


bin.labs<-c('Low', 'Medium', 'High', 'Very high')

predicted.mentions$p<-NULL
# predicted.mentions$xlim<-pNULL
predicted.mentions$Journal<-NULL
predicted.mentions$year<-NULL
predicted.mentions$SJRbin<-rep(rep(bin.labs, each =2), times = 3)
predicted.mentions$log10predicted.mention<-predicted.mentions$p10
predicted.mentions$predicted.mention<-10^(predicted.mentions$log10predicted.mention)
predicted.mentions$p10<-NULL
# predicted.mentions$SJRfac.scaled<-NULL

save(m.news, m.twitter, m.policy, predicted.mentions, file = 'Data/altmetric_glm_forgit.Rdata')