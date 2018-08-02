rm(list=ls())
setwd("~/GitHub/open-climate-change/")
setwd('/Users/robins64/Documents/git_repos/open-climate-change')



load('Data/scopus_glmerfit.Rdata')
save(mod.dat, file='Data/scopus_glm_forgit.Rdata')


rm(list=ls())
load('Data/altmetric_glmer_fit.Rdata')

save(ratio.plot, file = 'Data/altmetric_glm_forgit.Rdata')