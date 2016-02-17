
rm(list=ls())
source('./code/model.r')

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
## test this all out with Nigeria LSS data


  data<-read.csv("J:/Project/phc/nga/choice/data/1f_fully_prepped.csv")
  # sample 20% of people so it runs faster!
  # data<-data[data$pid%in%sample(unique(data$pid),nrow(data)/7*.2),]
  
  
  # rename util vals to make them nice
  #data$u='no'; data$u[data$util==1]='yes'
  
  # format the data long by my chosen alt variable
  dat=mnl.data.format(dat=data,
                      id.variable='pid',
                      choiceindicator='chosen',
                      alternatives='util')
  
  #coef list
  clist=c('male','age_gr','max_hh_yedu','ill_chronic' ,'ill_malaria', 'ill_headache' ,'ill_diarrhea', 'ill_cough','reg_activities_stopped' )
  
  # run the model
  mnl <- mnl.model(d=dat,
                   alternatives='alts',
                   id.variable='pid',
                   choiceindicator='chosen',
                   coefficients=clist,
                   dummycoeffs=NULL)
  
  
  
  # simulate betas from the model object
  simulatedbetas<- sim.betas(m=mnl$model,sims=1000)
  
  
  # try with low edu young female
  xhyp <- make.hyp.data(modeldata=mnl$modeldata)
  
  xhyp1 <- make.hyp.data(modeldata=mnl$modeldata,
                         customhypname=clist,
                         customhypval= c(0,0,3,0,0,0,0,0,0))
  
  xhyp2 <- make.hyp.data(modeldata=mnl$modeldata,
                         customhypname=clist,
                         customhypval= c(0,0,33,0,0,0,0,0,0))
  
  
  # simulate probabilities
  base <- mlogitsim(x=xhyp)
  res1 <- mlogitsim(x=xhyp1)  
  res2 <- mlogitsim(x=xhyp2)  
  
  
  
  # graph  
  forestplot(d=list(cbind(data.frame('name'=colnames(res1)),t(res1),id=rep("xhyp1",nrow(t(res1)))),
                    cbind(data.frame('name'=colnames(res2)),t(res2),id=rep("xhyp2",nrow(t(res2))))
  ))

  
  
  ## ADD GRAPHIC AND TABULAR OUTPUTS!!
  
  