# prep data from Kenya 



rm(list=ls())
library(foreign)
library(readstata13)

# load data
dataloc <- 'J:/DATA/KEN/HH_HEALTH_EXPENDITURE_UTILIZATION_SURVEY/2007/'
#hh      <- read.dta13(paste0(dataloc,'KEN_HHEUS_2007_HH_Y2014M10D14.DTA'))
ind     <- read.dta13(paste0(dataloc,'KEN_HHEUS_2007_IND_Y2014M10D14.DTA'))
  
  
# rename some variables

# years of education
    #table(ind$Q08_01,ind$Q09_01)
    # becuase its phrased acheived, then added on to it is question, we do years when entering that level. then add on number acheived.
  yrs.edu <- rep(NA,nrow(ind))
    yrs.edu[ind$Q08_01=='College (middle level)'   ]=12
    yrs.edu[ind$Q08_01=='Secondary'                ]=8
    yrs.edu[ind$Q08_01=='Primary'                  ]=0
    yrs.edu[ind$Q08_01=='Not Stated'               ]=0
    yrs.edu[ind$Q08_01=='Nursery'                  ]=0
    yrs.edu[ind$Q08_01=='University'               ]=12
    yrs.edu[ind$Q08_01=='Post primary/ vocational' ]=8
    
   # set maximums for years completed, as there are some funky numbers there
  tmp<- ind$Q09_01
    tmp[ind$Q08_01=='College (middle level)'  & ind$Q09_01>=6 ]=6
    tmp[ind$Q08_01=='Secondary'               & ind$Q09_01>=5 ]=5
    tmp[ind$Q08_01=='Primary'                 & ind$Q09_01>=8 ]=8
    tmp[ind$Q08_01=='Not Stated'              & ind$Q09_01>=0 ]=0
    tmp[ind$Q08_01=='Nursery'                 & ind$Q09_01>=0 ]=0
    tmp[ind$Q08_01=='University'              & ind$Q09_01>=6 ]=6
    tmp[ind$Q08_01=='Post primary/ vocational'& ind$Q09_01>=6 ]=6
  tmp[ind$Q09_01==98]=0
   # add them together to get total edu years
  # many 'not stated makes it difficult - lost of NA
  yrs.edu = yrs.edu+tmp
  # hist(yrs.edu)
  
  # max edu by HH
  ind$hhid = paste0(ind$cluster,ind$HOUSEHOLD_NUMBER)
  ind$max_hh_yedu<-yrs.edu
  x<-aggregate(max_hh_yedu~hhid,FUN=max,data=ind)
  ind<-merge(ind,x,by='hhid')
  
  max_hh_yedu=ind$max_hh_yedu.y
  
  
  
# age
  age = ind$age

# male
  male = ind$Q03_01=="Male"
  male[ind$Q03_01=='Not Stated']=NA
  
# rural
  rural = ind$CLUSTER_TYPE == "Rural"
  
# religions
  rel.christian = (ind$Q04_01%in%c("Catholic","Protestant"))
  rel.muslim    = (ind$Q04_01%in%c("Muslim"))

# illness
  # chronic - Hypertension, Diabetes, Cardiac, Arthritis, HIV AIDS, Ulcers, Gout, Other Chronic
  trim <- function (x) gsub("^\\s+|\\s+$", "", x)
  ill.chronic =     rowSums(cbind(trim(ind$Q15A_01)=='Yes',
                                  trim(ind$Q15B_01)=='Yes',
                                  trim(ind$Q15C_01)=='Yes',
                                  trim(ind$Q15D_01)=='Yes',
                                  trim(ind$Q15E_01)=='Yes',
                                  trim(ind$Q15F_01)=='Yes',
                                  trim(ind$Q15G_01)=='Yes'),na.rm=T)>=1

# was ill in past 4 weeks
  ill.4wks <- ind$Q16_01=="People with some sickness reported"
  
  
# facility utilized, either for illness or preventative/promotative
  util = trim(ind$Q17_01) == "Yes"| trim(ind$Q21_01) == "Yes"
  util[is.na(util)]=FALSE
  
  
# utilization
  # illness for those who utilized
  # based on Q25 questions (using only first visit, for now)
  ill.malaria    = ind$Q25_1_11 == 1          
  ill.pnuemonia  = ind$Q25_1_11 == 2           
  ill.skin       = ind$Q25_1_11 == 3    
  ill.tb         = ind$Q25_1_11 == 4 
  ill.hiv        = ind$Q25_1_11 == 5  
  ill.diabetes   = ind$Q25_1_11 == 6     
  ill.diarrhea   = ind$Q25_1_11 == 7    
  ill.worm       = ind$Q25_1_11 == 8
  ill.injury     = ind$Q25_1_11 == 9 
  ill.std        = ind$Q25_1_11 == 10
  ill.eye        = ind$Q25_1_11 == 11
  
  # replace NAs
  for(illness in ls()[grep('ill.',ls())]) {
    x<-get(illness)
    x[is.na(x)]=F
    assign(illness,x)
  }
  
  # facility types
  facility = ""
    facility[ind$Q27_1_11=="Govt. Hospital"                      ] =  "Hosp"                                
    facility[ind$Q27_1_11=="Private hospital"                    ] =  "Hosp"                         
    facility[ind$Q27_1_11=="Mission hospital"                    ] =  "Hosp"                         
    facility[ind$Q27_1_11=="Govt. Health Centre"                 ] =  "PHC"                         
    facility[ind$Q27_1_11=="Mission health centre"               ] =  "PHC"                          
    facility[ind$Q27_1_11=="Govt.Dispensary"                     ] =  "PHC"                          
    facility[ind$Q27_1_11=="Mission Dispensary"                  ] =  "PHC"                          
    facility[ind$Q27_1_11=="Nursing/Maternity Home"              ] =  ""                                
    facility[ind$Q27_1_11=="Private Clinic"                      ] =  "PHC"                               
    facility[ind$Q27_1_11=="NGO Clinic"                          ] =  "PHC"                          
    facility[ind$Q27_1_11=="Company/parastatal clinic"           ] =  "PHC"                        
    facility[ind$Q27_1_11=="Communitypharmacies(Bamako)"         ] =  "Pharm"                            
    facility[ind$Q27_1_11=="Chemist/pharmacy/shop"               ] =  "Pharm"                                
    facility[ind$Q27_1_11=="Traditional healer"                  ] =  ""                                
    facility[ind$Q27_1_11=="Village health  Worker (TBA, CHW)"   ] =  "PHC" 
    facility[ind$Q27_1_11=="Other (specify)……………."               ] =  ""    
    facility[grep('clinic',tolower(ind$Q27_OTHERS_SPECIFY_1_11) )  ] = "PHC"
    facility[grep('dispens',tolower(ind$Q27_OTHERS_SPECIFY_1_11) ) ] = "PHC"
    facility[grep('hosp',tolower(ind$Q27_OTHERS_SPECIFY_1_11) )    ] = "Hosp"   
    facility[grep('shop',tolower(ind$Q27_OTHERS_SPECIFY_1_11) )    ] = "Pharm"   
    
  facility[ill.4wks&facility==""]="NoCare"
  
  
  
  rm(x,tmp,trim,ind,illness,dataloc,yrs.edu)


  # cbind the rest of them now
  obj=ls()
  res<-data.frame(id=1:length(male))
  for(o in obj) res[,o]=get(o)

  
  # save it
  save(res,file='./data/ken_hheus_2007_prepped.RData')
  
  