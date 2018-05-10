
########################
## CC Open science
#
## Figure 2
#
## OA articles over time 


library(plyr)
library(ggplot2)
library(lme4)

rm(list=ls())
setwd("~/GitHub/open-climate-change/")
setwd('/Users/robins64/Documents/git_repos/open-climate-change')

## load data
load("./Data/scopus_OA_climate_clean.Rdata")  ## scopus data filtered by Jimmy - journals with > 200 papers in last 10 years


j.dat<-read.csv("./Data/BinJournallist_fromScopus.csv",stringsAsFactors = F)

dat<-scop
dat$Cited.by[which(is.na(dat$Cited.by))]<-0
dat<-merge(dat,j.dat[,c("Source.title","bin")],by.x="Source.title",by.y="Source.title",all.x=T)
names(dat)

## summary of open access over time
names(dat)

sum.dat<-ddply(dat,.(Year,bin),summarize,
               OpenArt = length(OA[which(OA==T)]),
               ClosArt = length(OA[which(OA==F)]))
sum.dat$Ratio<-with(sum.dat, OpenArt/ClosArt)

sum.dat$OpenPer<-with(sum.dat, OpenArt/(OpenArt+ClosArt)*100)
sum.dat$ClosPer<-with(sum.dat, ClosArt/(OpenArt+ClosArt)*100)
sum.dat$ClosDif<-with(sum.dat, ClosPer-100)

sum.totdat<-ddply(dat,.(Year),summarize,
                  OpenArt = length(OA[which(OA==T)]),
                  ClosArt = length(OA[which(OA==F)]))
sum.totdat$Tot<-with(sum.totdat,OpenArt + ClosArt)

line1<-sum.dat %>% filter(Year == 2016 & bin == 'A')
line2<-sum.dat %>% filter(Year == 2016 & bin == 'B')
line3<-sum.dat %>% filter(Year == 2016 & bin == 'C')
line4<-sum.dat %>% filter(Year == 2016 & bin == 'D')
quant.labs<-c('0.1-1.2', '1.2-1.7', '1.7-2.7', '2.7-18.1')
bin.labs<-c('Low', 'Medium', 'High', 'Very high')
tot.lab<-sum.totdat[10,2]/sum.totdat[10,4]*100

## cols
library(RColorBrewer)
mycols <- brewer.pal(n = 9, "Greens")[c(3,5,6,8)]

p1<-ggplot(sum.dat)
p1f<-p1 + theme_classic() + 
  theme(text = element_text(size=6),
        legend.text = element_text(size=4),
        legend.title = element_text(size=4),
        legend.key.height = unit(6,units="points")) +
  lims(y=c(0, 45)) +
  geom_line(aes(x=Year,y=OpenPer,colour=bin),lwd=0.5) + 
  geom_line(aes(x=Year,y=(OpenArt/Tot*100)),data=sum.totdat,
            lwd=0.9) +
  ylab("OA publications (%)") + 
  coord_cartesian(expand=F) + 
  scale_x_continuous(breaks=seq(2008, 2016, 2), labels=seq(2008, 2016, 2), limits= c(2007, 2017)) +
  scale_colour_manual(values=mycols) + 
  # scale_color_discrete(labels = c("Low","Medium","High","Very high"),name="Journal impact")
  theme(legend.position='none') +
  annotate('text', x = 2016.1, y = line1$OpenPer-0.5, size=1, hjust=0, label = bin.labs[1]) +
  annotate('text', x = 2016.1, y = line2$OpenPer, size=1, hjust=0, label = bin.labs[2]) +
  annotate('text', x = 2016.1, y = line3$OpenPer, size=1, hjust=0, label = bin.labs[3]) +
  annotate('text', x = 2016.1, y = line4$OpenPer+0.5, size=1, hjust=0, label = bin.labs[4]) +
  annotate('text', x = 2016.1, y = tot.lab, size=1.25, fontface=2, hjust=0, label = 'Total')




pdf(file="./figures/Figure1.pdf", height=2, width=3)
#quartz(height=2,width=3)
p1f
dev.off()






# ## Not use below ##

# ## graph of total number of publications for each journal bin across time
# ## Journal information plot
# names(dat)
# sum.j<-ddply(dat[dat$Year==2016,],.(Source.title,Article.Open.Access,Journal.Open.Access,bin),summarize,
#              SJR = mean(X2016.SJR),
#              NoArt = length(SJR))
# name.ind<-sum.j$Source.title[duplicated(sum.j$Source.title)]

# jour.dat$OAtype<-"Closed"
# jour.dat$OAtype[jour.dat$Source.title %in% name.ind]<-"Hybrid"
# jour.dat$OAtype[jour.dat$Journal.Open.Access=="DOAJ/ROAD Open Access"]<-"Open"

# sum.totdat<-ddply(dat,.(Year,bin),summarize,
#                   TotArt = length(bin))

# p2<-ggplot(sum.totdat)
# p2f<-p2 + theme_classic() +
#   geom_area(aes(x=Year,y=TotArt,fill=bin,colour=bin),alpha=0.6) + 
#   ylab("Total publications") + 
#   coord_cartesian(expand=F)
# p2f












