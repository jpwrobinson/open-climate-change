

setwd('/Users/robins64/Documents/git_repos/open-climate-change')
library(stringr)

## Reading in scopus data, cleaning, merging, saving to one dataframe

scop<-read.csv('Data/ScopusOAData_20180214TT.csv')
head(scop)

# ## Trav already done this - ignore
# ### reading in raw csvs
# file.list<-list.files('Data/ScopusCiteRawData_2007-2016')
# names.list<-str_replace_all(file.list, '.csv', '')
# df<-numeric()

# for(i in 1:length(file.list)){
# 	dat<-read.csv(paste0('Data/ScopusCiteRawData_2007-2016/', file.list[i]))
# 	## strip unnecessary columns
# 	dat$X...Authors <- NULL
# 	dat$Title <- NULL
# 	dat$Volume <- NULL
# 	dat$Issue <- NULL
# 	dat$Art..No. <- NULL
# 	dat$Page.start <- NULL
# 	dat$Page.end <- NULL
# 	dat$Page.count <- NULL
# 	dat$Link <- NULL
# 	dat$Source <- NULL
# 	dat$EID <- NULL
# 	## assign csv name to data frame
# 	assign(names.list[i],dat)
# }

## creating cleaner version for analysis. Use big csv for paper referencing if necessary
scop$X...Authors <- NULL
scop$Title <- NULL
scop$Volume <- NULL
scop$Issue <- NULL
scop$Art..No. <- NULL
scop$Page.start <- NULL
scop$Page.end <- NULL
scop$Page.count <- NULL
scop$Link <- NULL
scop$Source <- NULL
scop$EID <- NULL
scop$Publisher.s.Country <- NULL

## add OA identifier
scop$OA<-ifelse(is.na(scop$Journal.Open.Access), FALSE, TRUE)


### Need to subset to relevant climate + ecology journals

journals<-c('....')

scop<-scop[scop$Source.title %in% journals,]
dim(scop)


save(scop, file='Data/scopus_OA_climate_clean.Rdata')



