# Roy Burstein
# Generic Code for the Random Parameters Logit Model
# Functions to run model and also 

# Set up
rm(list=ls())

library(nnet)
library(MASS)
library(mlogit)

#######################################################################################################
#######################################################################################################
## 1.) FUNCTIONS FOR MODELLING


# make variables dummies where needed
make.dummies <- function(dat=d,
                         var,
                         nameto,
                         intercept=FALSE,
                         append=TRUE) {
  
  if(class(var)!='character')     stop('Var expects character')
  if(class(nameto) !='character') stop('nameto expects character')

  assign(nameto,factor(dat[,var]))
  
  form=as.formula(paste0('~',nameto))
  
  if(intercept)
    res <- model.matrix(form)
  else
    res <- model.matrix(form)[,-1]  
  
  if(append)
    res <- cbind(dat,res)
  
  return(res)
}
  


mnl.model <- function(d=data,
                alternatives='facility',
                id.variable='pid',
                choiceindicator='chosen',
                coefficients=c('male','age_gr'),
                dummycoeffs=c('max_hh_edu'),
                rpl=F){
 
  # Throw exceptions
  if(class(dummycoeffs)!='character' & !is.null(dummycoeffs))    
    stop('dummycoeffs expects character')
  if(class(coefficients)!='character' & !is.null(coefficients))    
    stop('coefficients expects character')
  if(class(choiceindicator)!='character' ) 
    stop('choiceindicator expects character')

  
  # data must be formatted to one row per alternative/attribute.. throw error if not the case
  
  
  # format alternatives as intercept
  d= make.dummies(var=alternatives,nameto='int',intercept=T,append=T)
  
  # start a list of coefficients
  
  if(!is.null(coefficients)){
    form = paste0(coefficients,collapse='+')
  } else{ form = c() }
  
  # format dummy coeffs and save all possible values in coeff list
  if(!is.null(dummycoeffs))
    for(dum in dummycoeffs){
      d= make.dummies(var=dum,nameto=dum,intercept=F,append=T)
      form = paste0(form,'+',
               paste(colnames(make.dummies(var=dum,nameto=dum,intercept=F,append=F))[-1],
               collapse="+"))
    }
    
  
  # get the model formula (add in intercepts for the alternatives)
  form<-mFormula(as.formula(paste0(choiceindicator,'~',form,'-1 |',
    paste(colnames(make.dummies(var=alternatives,nameto='int',intercept=F,append=F))[-1],
                                                collapse='+'))))
  
  # format the data to run model using mlogit
  md<-mlogit.data(d, choice = choiceindicator,
                 shape="long", alt.var=alternatives, id.var=id.variable)

  # clean data frame 
  dd<-model.frame(form,data=md)
  
  # first run a straightforward multinomial logit
  m <- mlogit(form,data=dd)
  
  # if rpl is selected, run that as well
  m<-mlogit(form, dd, 
            rpar=c(intNocare_='n', intPPMV_='n',   intPrHosp_='n',
                       intPrPHC_='n', intPuHosp_='n', intPuPHC_='n'), 
                R=100, halton=NA, print.level=1, 
                panel=FALSE,correlation=TRUE) 
  
  
  return(m)
}
  




