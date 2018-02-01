#################################
##### loocv                 #####
##### By: Marloes Eeftens   #####
##### Last edit: 01/02/2018 #####
#################################

loocv<-function(x,dependent,predictors){

  #0) Check settings, set defaults:
  if(missing(x)){stop("No dataframe specified for x")}
  if(missing(dependent)){stop("No dependent variable specified")}
  if(missing(predictors)){stop("Please specify the selected predictors as a vector of strings.")}

  #1) Create a new variable in x called "loocv":
  x$loocv<-c(rep(NA,dim(x)[1]))

  #2) Construct the right side of the modelling equation:
  right_eqn_side<-paste(c(predictors),collapse="+")

  #3) Leave out one observation:
  for(i in 1:dim(x)[1]){
    x_train<-x[-i,]
    x_test<-x[i,]
    #Apply the model to each of the data sets from which i was removed:
    eval(parse(text=paste0("model_train<-lm(data=x_train,",dependent,"~",right_eqn_side,")")))
    x$loocv[i]<-predict(model_train,x_test)
  }

  #4) Return the new dataframe with extra column "loocv":
  return(x)
}
