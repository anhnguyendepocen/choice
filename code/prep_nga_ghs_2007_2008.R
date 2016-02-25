


rm(list=ls())
library(foreign)
library(readstata13)
library(gdata)


  ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # health info
  setwd('C:/Users/royburst/Google Drive/choice')
  ind     <- read.dta13('./data/GENERAL_HOUSEHOLD_SURVEY/2007_2008/NGA_GHS_2007_2008_PARTI_HEALTH.DTA')
    
    
  # get rid of duplicate rows
  ind<-unique(ind)
  
  # HEALTH
  ill.4wks     <- ind$sick==1

  ill.fever     <- ind$fevermal == "2"
  ill.diarrhea  <- ind$dirrhea  == "2"
  ill.msk       <- ind$backpain == "2"
  ill.cough     <- ind$cough    == "2"
  ill.ent       <- ind$ernostrt == "2"
  
  reg_activities_stopped <- ind$daysmiss%in%c('2','3','4')
  
  # util
  util = ind$consult == "1"
  util[ind$provider==5] = FALSE #traditional healer
  
  # provider
  # unfortunately PHC and Hosp are mixed together.
  facility = rep("",nrow(ind))
  facility[ind$provider%in%c(1,4,6)]="Private"
  facility[ind$provider%in%c(2,3)]  ="Public"
  facility[ind$provider%in%c(7)]    ="PPMV"
  facility[ind$provider%in%c(5)]  ="NoCare"
  util[facility!=""]=TRUE
  util[facility=="NoCare"]=FALSE
  facility[util==FALSE] = "NoCare"
  
  # ids
  pid <- paste0(ind$state,ind$lga,ind$ric,ind$eacode,ind$sector,ind$hunum,ind$memberno)
  
  # cbind what I have currently
  rm(ind)
  obj=ls()
  d<-data.frame(pid=pid)
  for(o in obj) d[,o]=get(o)
  keep(d, sure = TRUE) 
  
  
  
  ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # demographics
  ind <- read.dta13('./data/GENERAL_HOUSEHOLD_SURVEY/2007_2008/NGA_GHS_2007_2008_PARTB_PERSONS_IN_THE_HOUSEHOLD.DTA')
  ind <- unique(ind)
  ind$sector = 1
  ind$sector[ind$Sector=="Rural"]=2
  
  # 
  age   <- ind$age
  male  <- ind$sex=="Male"
  urban <- ind$Sector=="Urban"
  
  # max edu in hh
  eduyrs <- rep(NA,nrow(ind))
    eduyrs[ind$attendan=="1"]=0
    eduyrs[ind$hgrderch%in%c(0,1,2,3)]=0
    eduyrs[ind$hgrderch%in%c(4:15)]   =ind$hgrderch[ind$hgrderch%in%c(4:15)]-3
    eduyrs[ind$hgrderch%in%c(16:18)]  = 16
    eduyrs[is.na(eduyrs)&ind$hlevelrc=='1'] = 0   
    eduyrs[is.na(eduyrs)&ind$hlevelrc=='2'] = 3
    eduyrs[is.na(eduyrs)&ind$hlevelrc=='3'] = 9
    eduyrs[is.na(eduyrs)&ind$hlevelrc=='4'] = 14
    ind$max_hh_yedu=eduyrs
    tmp <- aggregate(max_hh_yedu~State+lga+ric+eacode+sector+hu,
                     FUN=max,data=ind)  
    ind<-merge(ind,tmp,by=c('State','lga','ric','eacode','sector','hu'),all.x=T)
    
    max_hh_yedu = ind$max_hh_yedu.y
    
  # hh size
  ind$hh_size = 1
  tmp <- aggregate(hh_size~State+lga+ric+eacode+sector+hu,
                     FUN=sum,data=ind)      
  ind<-merge(ind,tmp,by=c('State','lga','ric','eacode','sector','hu'),all.x=T)
  hh_size = ind$hh_size.y
    
  # assets (max count in HH - quantile)
  radio       <- ind$oradio    == "1"
  tv          <- ind$otelev    == "1"
  cell.phone  <- ind$ombphn    == "1"
  fixed.phone <- ind$ofixphn   == "1"
  pc          <- ind$opc       == "1"
  internet    <- ind$ointernt  == "1"
  tmp <- aggregate(cbind(radio,tv,cell.phone,fixed.phone,pc,internet)~
                     State+lga+ric+eacode+sector+hu,data=ind,FUN=sum)

  for(v in c('tv','cell.phone','fixed.phone','pc','internet')) # 'radio',
    tmp[,v] = tmp[,v] > 0
  
  tmp$assets <- rowSums(cbind(tmp$tv,tmp$cell.phone, # tmp$radio,
                            tmp$fixed.phone,tmp$pc,tmp$internet))   
  ind<-merge(ind,tmp,by=c('State','lga','ric','eacode','sector','hu'),all.x=T)
  # NOT ANYMORE split into 0/1  (any)
  assets=ind$assets >0
  
    
  # income group (make numeric)
  ## TOO MANY MISSING TO USE   
    
  
  # pid
  pid <- paste0(ind$State,ind$lga,ind$ric,ind$eacode,ind$sector,ind$hu,ind$b0)


  # merge back in
  dd<-data.frame(pid,age,male,urban,max_hh_yedu,eduyrs,hh_size,assets,
                 radio,tv,cell.phone,fixed.phone,pc,internet)
  
  
  d<-merge(d,dd,by='pid',all=F)
  
  # sanity check
  summary(glm(util~reg_activities_stopped+max_hh_yedu+male+assets+
              hh_size+urban+ill.fever+ill.cough+ill.diarrhea,
              data=d[d$ill.4wks==T,],family='binomial',na.action='na.omit'))
  
  # save
  ghs<-d
  save(ghs,file='./data/nga_ghs_2007_2008_prepped.RData')
  
  
  
  
  
  
  
  
  
  