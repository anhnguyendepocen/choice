# Roy Burstein
# Generic Code for functions to run the MNL and by extension RPL Models




library(nnet)
library(MASS)
library(mlogit)


## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Format data long by chosen alt


## ~~~~~~~~~~~~~~~~~~~~~~~~~~~
## make variables dummies where needed
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
    

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~
## wrap mnl modelling function
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
                       paste(colnames(make.dummies(var=dum,nameto=dum,intercept=F,append=F)), #[-1],
                       collapse="+"))
            }
            
          
          # get the model formula (add in intercepts for the alternatives)
        #  form<-mFormula(as.formula(paste0(choiceindicator,'~', 
        #              paste(colnames(make.dummies(var=alternatives,nameto='int',intercept=F,append=F)), #[-1],
        #               collapse='+'),'-1|',form)))
          form<-mFormula(as.formula(paste0(choiceindicator,'~', 
                      '1|',form)))
          
          # format the data to run model using mlogit
          md<-mlogit.data(d, choice = choiceindicator,
                         shape="long", alt.var=alternatives, id.var=id.variable)
        
          # clean data frame 
          dd<-model.frame(form,data=md)
          
          # first run a straightforward multinomial logit
          m <- mlogit(form,data=dd)
          
          # if rpl is selected, run that as well
          if(rpl){
            stop('Have not yet built in, choose rpl==FALSE')
            #m<-mlogit(form, dd, 
            #          rpar=c(intNocare_='n', intPPMV_='n',   intPrHosp_='n',
            #                     intPrPHC_='n', intPuHosp_='n', intPuPHC_='n'), 
            #             R=100, halton=NA, print.level=1, 
            #             panel=FALSE,correlation=TRUE) 
          }
          
          return(list(model=m,modeldata=dd))
        }
    
  

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~
## extract predictions (assuming no alt-specific variables included)
# in-sample is easy: call probabilities from the model object, the following is for hypotheticals


  # first simulate betas from mvnorm from model results
  sim.betas <- function(m=mnl$model,sims=100){
    # first extract the information from the model object
    pe <- summary(m)$coefficients 
    ll <- m$logLik 
    vc <- solve(m$hessian)*-1
   
    # how many coefficients do I have?
    totcoefs<-length(pe)
    
    # how many alternatives were there?
    altnames<-names(predict(m))
    alts<-length(altnames)
    
    # what was our reference category?
    ref <- names(predict(m)[1])
    
    # how many coefficients do I have per alternative?
    coefs<-totcoefs/(alts-1) # -1 to remove reference which has no coeffs by construction
    
    # simulate <<sims>> betas from the var-covar matrix
    simbetas <- mvrnorm(sims,pe,vc)
    
    # construct a 3d array for each alternate 
    #(make a reference one as well with zeros)
    simb <- array(NA, dim = c(sims,coefs,alts)) 
    simb[,,1] <- c(rep(0,sims*(coefs)))    # reference 
    for(i in 2:alts){
      alt<-names(predict(m)[i])
      simb[,,i] <- simbetas[,c(grep(alt,names(data.frame(simbetas))))]           
    }
    
    return(list(totcoefs=totcoefs,
                altnames=altnames,
                alts=alts,
                ref=ref,
                coefs=coefs,
                simb=simb,
                sims=sims))
  }  
  
  # make frame for hypotheticals (like cfMake sort of)
  # by default take the means
  make.hyp.data <- function(modeldata=dd,
                            customhypname=NULL,
                            customhypval=NULL){
    # by default use means
    xhyp<-aggregate(modeldata,FUN=mean,by=list(index(modeldata)$alt))
    xhyp<-xhyp[,!colnames(xhyp)%in%c("Group.1",choiceindicator)] # remove first 2 vars 
    
    # if there are custom settings for the hyps, apply those here
    if(class(customhypname)!='character'&!is.null(customhypname))
      stop('customhypname argument expects either NULL \n
           or a vector of characters matchnig custom value length')
    if(class(customhypval)!='numeric'&!is.null(customhypval))
      stop('customhypval argument expects either NULL \n
           or a vector of numeric matching custom name length')
    if(!is.null(customhypname)&!is.null(customhypval)&length(customhypval)!=length(customhypname))
      stop('customhypval and name need to be equal lengths')
    
    if(!is.null(customhypname)&length(customhypval)==length(customhypname)){
      for(var in customhypname){
        if(is.null(xhyp[,var])) stop(sprintf('%s does not exist',var))
        xhyp[,var]=customhypval[customhypname==var]
      }
    }
    
    return(xhyp)
  }
  
  
  # Function to Simulate results given hypothetical data and betas from mvnorm
  mlogitsim <- function(x,b=simulatedbetas,ci=0.95,summary=T) {
    
      # decompose everything I got from running sim.betas
      for(i in 1:length(b)) assign(names(b)[i], b[[i]])
    
      x <-  as.vector(t(as.matrix(x)))
      x <- array(x, dim = c(1, coefs, dim(simb)[3]))
      
      esims <- nrow(as.matrix(simb))
      res <- list(lower = array(0, dim = c(1,alts, length(ci))), 
                  upper = array(0, dim = c(1,alts, length(ci))),
                  pe    = array(0, dim = c(1,alts, length(ci))))
      
      simdenom <- 0
      for (icat in 1:(dim(simb)[3]))  {
        newdenom <- exp(simb[, , icat] %*% x[1, , icat])
        simdenom <- simdenom + newdenom
      }
      simy <- matrix(NA, nrow = dim(simb)[1], ncol = alts)
      for (icat in 1:dim(x)[3]) {
        simy[, icat] <- exp(simb[, , icat] %*% x[1, , icat])/simdenom
      }         
      
      simysorted <- apply(simy, 2, sort)
      res$pe <- apply(simy, 2, mean)
      length.simy <- nrow(simy)
      low <- up <- NULL
      
      if(summary){
        for(alt in 1:alts){
          res$lower[1,alt, 1] <- quantile(simysorted[,alt],  probs = (1-ci)/2)
          res$upper[1,alt, 1] <- quantile(simysorted[,alt],  probs = ci+(1-ci)/2)
        }  
        res<-list(
          lci=round(as.numeric(res$lower),5),
          mean=round(as.numeric(res$pe),5),
          uci=round(as.numeric(res$upper),5))
        res<-do.call(rbind.data.frame, res)
        colnames(res)<-altnames
        return(res)
      }
      if(summary==F) return(simy)
    }
  
    
  
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~
## plot extracted predictions
  
  forestplot <- function(d=list(), xlab="Probability of Choosing Utilization Alternative",ylab="Utilized"){
    d <- do.call("rbind", d) 
    require(ggplot2)
    p <- ggplot(d, aes(x=name, y=mean, ymin=lci, ymax=uci,colour=id)) + 
      geom_pointrange(data=subset(d,as.numeric(d$id)==1),size=1.2) +  
      geom_pointrange(data=subset(d,as.numeric(d$id)==2),size=1.2,aes(x=as.numeric(name)+.2))  +  
      coord_flip() +
      ylab(xlab) +
      xlab(ylab) + theme_bw() 
    return(p)
  }

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
## test this all out with Nigeria LSS data
test=F
if(test){
  data<-read.csv("J:/Project/phc/nga/choice/data/1f_fully_prepped.csv")
  
  # sample 20% of people so it runs faster!
  # data<-data[data$pid%in%sample(unique(data$pid),nrow(data)/7*.2),]
  
  # make it binary for utilized only
    # keep one row per pid
    data = data[data$chosen==1,]
    row.names(data)=NULL
    
    # rename util vals to make them nice
    data$u='no'; data$u[data$util==1]='yes'
    
    # append and duplicate data so have it long by pid-alternate, and identified with chosen
      # make list of non-selected alternative for each individual
      alterns = t(matrix(rep(unique(data$u),nrow(data)),ncol=nrow(data)))
      alterns = unique(data$u)
        
      
      dat=data[order(data$pid),][rep(1:nrow(data),each=2),] 
      dat$outcome = (rep(alterns,nrow(dat)/length(alterns)))
      dat$chosen  = (dat$u==dat$outcome)
      row.names(dat)=NULL
      
      dat<-mlogit.data(dat, choice = 'chosen',
                      shape="long", alt.var='outcome', id.var='pid')
      
  
      
  # run the model
  mnl <- mnl.model(d=dat,
                 alternatives='outcome',
                 id.variable='pid',
                 choiceindicator='chosen',
                 coefficients=c('male','age_gr'),
                 dummycoeffs=c('max_hh_edu'))
  
  
  
  # simulate betas from the model object
  simulatedbetas<- sim.betas(m=mnl$model,sims=1000)
  
  # try with low edu young female
  xhyp <- make.hyp.data(modeldata=mnl$modeldata)
                        
  xhyp1 <- make.hyp.data(modeldata=mnl$modeldata,
                        customhypname=c('male','age_gr','max_hh_edu3','max_hh_edu4','max_hh_edu5'),
                        customhypval= c(0,0,0,0,1))

  xhyp2 <- make.hyp.data(modeldata=mnl$modeldata,
                         customhypname=c('male','age_gr','max_hh_edu3','max_hh_edu4','max_hh_edu5'),
                         customhypval= c(0,0,1,0,0))
  
  
  # simulate probabilities
  base <- mlogitsim(x=xhyp)
  res1 <- mlogitsim(x=xhyp1)  
  res2 <- mlogitsim(x=xhyp2)  
  
  # graph  
  forestplot(d=list(cbind(data.frame('name'=colnames(res1)),t(res1),id=rep("Female u5 with Edu=3",nrow(t(res1)))),
                    cbind(data.frame('name'=colnames(res2)),t(res2),id=rep("Female u5 with Edu=5",nrow(t(res2))))
                    ))
}

