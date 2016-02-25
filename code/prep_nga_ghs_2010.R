# prep data from Kenya 



rm(list=ls())
library(foreign)
library(readstata13)

# load data
setwd('C:/Users/royburst/Google Drive/choice')
ind     <- read.dta13('./data/ghs/data/post-harvest/sect4a_harvestW1.dta')
  

# keep only those who were ill or injured
ind <- ind[grep("YES",ind$s4aq3),]
#ind <- ind[,]
  
  # make a new data frame for resulting
  d <- data.frame(na=rep(NA,nrow(ind)))
  d$pid <- paste0(ind$hhid,ind$indiv)  
  d$hhid  <- ind$hhid    
  d$indiv <- ind$indiv
  
# rename some variables 
  # (HEALTH)
  d$util <- ind$s4aq1==1
  d$reg_activities_stopped <- ind$s4aq4=='yes'
  d$reg_activities_stopped[is.na(d$reg_activities_stopped)]=T

  # few obs, so need to classify facility types
  f <- o <- rep("",length(d$util))
  f[ind$s4aq7%in%c(1)]       <- "Hosp"
  f[ind$s4aq7%in%c(2,5,6,7)] <- "PHC"
  
  o[ind$s4aq8%in%c(1,2,3,4)] <- "Pu"
  o[ind$s4aq8%in%c(5,6,7)]   <- "Pr"
  
  d$facility <- paste0(o,f)
  
  d$facility[ind$s4aq7%in%c(8,9)]   <- "Home"
  d$facility[ind$s4aq7%in%c(10)]    <- "NoCare"
  d$facility[ind$s4aq7%in%c(3,4)]   <- "PPMV"
  d$facility[d$util==FALSE] <- "NoCare"
  
    # keep only obs that fall in the categories I can use
    d<-d[d$facility%in%c('PuHosp','PrHosp','PPMV','Home','NoCare','PrPHC','PuPHC'),]
    d$na<-NULL

  # NO INFORMATION ON HEALTH CONDITIONS..   
    
# Section 1. - demographic and HH variables. 
  ind   <- read.dta13('./data/ghs/data/post-harvest/sect1_harvestW1.dta')
  dd    <- data.frame(na=rep(NA,nrow(ind)))
  dd$hhid  <- ind$hhid    
  dd$indiv <- ind$indiv
  
  # religions
  dd$rel.christian = (ind$s1q22%in%c("christianity"))
  dd$rel.muslim    = (ind$s1q22%in%c("islam"))
    
  
  # age
  dd$age = ind$s1q4
  
  # male
  dd$male = ind$s1q2=="male"

  # hhsize
  ind$hh_size = 1
  x <- aggregate(hh_size~hhid,FUN=sum,data=ind)
  dd <- merge(dd,x,by='hhid')
  
  
  
  # years of education
  ind   <- read.dta13('./data/ghs/data/post-harvest/sect2a_harvestW1.dta')
  ### LOOKING LIKE ALMOST ALL ARE MISSING.... (See pre-planting survey?)
  
  
  ## ASSET INDEX
  

  # rural - from hh info
  hh       <- read.dta13("./data/ghs/data/post-harvest/secta_harvestW1.dta")
  hh$rural <- hh$sector == "rural"
  hh <- hh[c('hhid','rural')]
  
  # Merge things
  d <- merge(d,dd,by=c('hhid','indiv'))
  d <- merge(d,hh,by='hhid')
  d$na<-NULL
  
  # sanity check logistic model
  d=na.omit(d)
  summary(glm(util~age+male+reg_activities_stopped+rel.muslim+rural+hh_size,data=d,family='binomial'))
  
  