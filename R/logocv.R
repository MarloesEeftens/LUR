#################################
##### logocv                #####
##### By: Marloes Eeftens   #####
##### Last edit: 11/04/2018 #####
#################################

logocv<-function(x,dependent,fixed,random=c("intercept"),group,export_estimates=FALSE){

  #0) Check settings, set defaults:
  if(missing(x)){stop("No dataframe specified for x")}
  if(missing(dependent)){stop("No dependent variable specified")}
  if(missing(fixed)){stop("Please specify the selected fixed predictors as a vector of strings.")}

  #1) Create dummies for factors, and get all the variable names as they will show up in the model:
  fixed_varnames<-c()
  dummy_varnames<-c()
  for(i in 1:length(fixed)){
    if(!is.factor(x[,which(names(x)==fixed[i])])){fixed_varnames_i<-fixed[i]}
    if(is.factor(x[,which(names(x)==fixed[i])])){
      factor_levels<-levels(x[,which(names(x)==fixed[i])])
      fixed_varnames_i<-paste0(fixed[i],factor_levels)[-1]
      #Create dummy variables for the factors
      dummy_vars<-as.data.frame(lme4::dummy(x[,which(names(x)==fixed[i])]))
      names(dummy_vars)<-fixed_varnames_i
      x<-cbind.data.frame(x,dummy_vars)
      dummy_varnames<-c(dummy_varnames,fixed_varnames_i)
    }
    fixed_varnames<-c(fixed_varnames,fixed_varnames_i)
  }

  #2) Construct the right side of the modelling equation:
  fixed_part<-paste(c(fixed_varnames),collapse="+")
  random_part<-gsub("intercept","1",paste(c(random),collapse="+"))

  #3) Calculate model coefficients for each group left out:
  #Define the different groups of observations:
  groups<-as.vector(unique(x[[group]]))
  estimates<-list()
  for(i in 1:length(groups)){
    #Make a training dataset without groups[i]
    x_train<-x[x[[group]]!=groups[i],]
    #Train the model on the data set from which i was removed and save the estimates:
    eval(parse(text=paste0("model_train<-lmer(data=x_train,",dependent,"~",fixed_part,"+(",random_part,"|",group,"),REML=F)")))
    estimates[[i]]<-c(i,as.data.frame(coefficients(summary(model_train)))$Estimate)
  }

  #4) Construct an overview of the coefficients for each group left out:
  estimates<-rbindlist(lapply(estimates,as.data.frame.list))
  names(estimates)<-c(group,"intercept",paste0(fixed_varnames,"_coeff"))

  #5) Merge the coefficients into the original dataframe x by group:
  x<-merge(x=x,y=estimates,by=group,all=TRUE)

  #6) Apply the model (only the fixed part!) to each of the test datasets:
  equation<-paste0(c("x$logocv<-x$intercept",
                     rbind(c(rep("+x$",length(fixed_varnames))),
                           fixed_varnames,
                           c(rep("*x$",length(fixed_varnames))),
                           fixed_varnames,
                           c(rep("_coeff",length(fixed_varnames)))
                          )
                     ),collapse="")
  eval(parse(text=equation))

  #7) Delete dummy variables, and delete coefficients if needed
  x<-x[,-which(names(x) %in% dummy_varnames)]
  if(export_estimates==FALSE){x<-x[,-which(names(x) %in% c("intercept",paste0(fixed_varnames,"_coeff")))]}

  #8) Return the new dataframe with extra column "logocv":
  return(x)
}
