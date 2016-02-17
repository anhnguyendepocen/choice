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
    

  
## ~~~~~~~~~~~~~~~~
## Format Dataset to run via specified alternatives (long)
  
mnl.data.format <- function(dat=data,
                            id.variable='pid',
                            choiceindicator='chosen',
                            alternatives='fac_type1'){
  
  
  # keep unique by PID
  dat <- subset(dat, !duplicated(get(id.variable)))
  row.names(dat)=NULL
  
  # rename util vals to make them nice
  dat$outcome = as.character(dat[,alternatives])
  
  # append and duplicate data so have it long by pid-alternate, and identified with chosen
  # make list of non-selected alternative for each individual
  alterns = t(matrix(rep(unique(dat$outcome),nrow(dat)),ncol=nrow(dat)))
  alterns = unique(dat$outcome)
  
  
  dat=dat[order(dat[,id.variable]),][rep(1:nrow(dat),each=length(alterns)),] 
  dat$alts    = (rep(alterns,nrow(dat)/length(alterns)))
  dat$chosen  = (dat$outcome==dat$alts)
  row.names(dat)=NULL
  
  return(mlogit.data(dat, choice = choiceindicator,
                     shape="long", alt.var='alts', id.var=id.variable))
}  
  
  
  
  
  
  
  
  
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~
## wrap mnl modelling function
  mnl.model <- function(d=dat,
                        alternatives='alts',
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
          d= make.dummies(dat=d,var=alternatives,nameto='int',intercept=T,append=T)
          
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
    altnames<-colnames(m$probabilities)
    alts<-length(altnames)
    
    # what was our reference category?
    ref <- altnames[1]
    
    # how many coefficients do I have per alternative?
    coefs<-totcoefs/(alts-1) # -1 to remove reference which has no coeffs by construction
    
    # simulate <<sims>> betas from the var-covar matrix
    simbetas <- mvrnorm(sims,pe,vc)
    
    # construct a 3d array for each alternate 
    #(make a reference one as well with zeros)
    simb <- array(NA, dim = c(sims,coefs,alts)) 
    simb[,,1] <- c(rep(0,sims*(coefs)))    # reference 
    for(i in 2:alts){
      alt<-colnames(m$probabilities)[i]
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
                            customhypval=NULL,
                            choiceindicator='chosen'){
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
    
    # add intercept
    xhyp<-cbind(data.frame(int=rep(1,nrow(xhyp))),xhyp)
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
      
      # loop through categories of outcomes to get denominator
      simdenom <- 0
      for (icat in 1:(dim(simb)[3]))  {
        newdenom <- exp(simb[, , icat] %*% x[1, , icat]) # exp(Bx)), simb for reference is always 0
        simdenom <- simdenom + newdenom # sum up all the probs for the denom Sigma(exp(Bx))
      }
      # simulate the results
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
  
  
  # function takes two unsummarized mlogitsims and gives summarized first differences
  firstdiffs<- function(obj1,obj2,ci=0.95,alterns=simulatedbetas$altnames){
    x<-obj1-obj2
    
    res <- list(lower = array(0, dim = c(1,length(alterns), length(ci))), 
                upper = array(0, dim = c(1,length(alterns), length(ci))),
                pe    = array(0, dim = c(1,length(alterns), length(ci)))) 
    
    res$pe <- apply(x, 2, mean)
    simysorted <- apply(x, 2, sort) 
    
    for(alt in 1:length(alterns)){
      res$lower[1,alt, 1] <- quantile(simysorted[,alt],  probs = (1-ci)/2)
      res$upper[1,alt, 1] <- quantile(simysorted[,alt],  probs = ci+(1-ci)/2)
    }  
    res<-list(
      lci=round(as.numeric(res$lower),5),
      mean=round(as.numeric(res$pe),5),
      uci=round(as.numeric(res$upper),5))
    res<-do.call(rbind.data.frame, res)
    # same order as simb
    colnames(res)<-alterns
    return(res)
  }
  
  
  
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~
## plot extracted predictions
  
  # forest plot
  forestplot <- function(d=list(), xlab="Probability of Choosing Utilization Alternative",ylab="Utilized",fdiff=F){
    d <- data.frame(do.call("rbind", d) )
    require(ggplot2)
    p <- ggplot(d, aes(x=name, y=mean, ymin=lci, ymax=uci,colour=id)) + 
      geom_pointrange(data=subset(d,as.numeric(d$id)==1),size=1.2) +  
      coord_flip() +
      theme_bw() 
    
      if(fdiff){
        p <- p + geom_hline(xintercept = 0,linetype="longdash")+
        theme(panel.grid.minor.x = element_line(linetype = "solid"))
        if (xlab=="Probability of Choosing Utilization Alternative")
          xlab="Difference in Probability of Choosing Utilization Alternative"
      } else {
        p<-p+geom_pointrange(data=subset(d,as.numeric(d$id)==2),size=1.2,aes(x=as.numeric(name)+.2)) 
      }
      p<-p+
        theme(axis.title.y = element_text(face="bold",size=20))+
        theme(axis.text.y  = element_text(face="bold",size=16))+
        theme(axis.text.x  = element_text(face="bold",size=12))+ 
        theme(legend.key = element_blank(),legend.title=element_blank(),legend.position="bottom")+
        theme(panel.border = element_blank())+
        ylab(xlab) +
        xlab(ylab) 
      return(p)
    }
  

  
  # stacked bar
  
  
 
  # table grob of results (predicted probs as well as model outputs)
  library(gridExtra)
  library(grid)
  library(xtable)
  model.results.grob <- function(m=mnl$model,xtable=F){
    res<-data.frame(round(summary(m)$CoefTable[,c(1:2,4)],3))
    r <- data.frame(round(exp(res[,1]),3),res[,1],res[,2],res[,3])
    rownames(r)=rownames(res)
    if(xtable==F) {
      colnames(r)=c("e^hat(beta)","hat(beta)","hat(se)","p-value")
      tt <- ttheme_default(colhead=list(fg_params = list(parse=TRUE)))
      grid.table(r,theme=tt)
    }else{ 
      colnames(r)=c("OR","beta","se","p-value")
      #xtable(r) 
      return(r)
    }
  }

  # predicted probabilities
  model.pred.grob <- function(r=res1,xtable=F){
    r=t(round(r,3))
    if(xtable==F) {
      tt <- ttheme_default(colhead=list(fg_params = list(parse=TRUE)))
      grid.table(r,theme=tt)
    }else{ 
      #xtable(r) 
      return(r)
    }
  }
  
  
  
  
  
  

## ~~~~~~~~~~~~ Fully format and run in one function


  
  


