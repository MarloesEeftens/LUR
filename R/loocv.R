#################################
##### loocv                 #####
##### By: Marloes Eeftens   #####
##### Last edit: 01/02/2018 #####
#################################

loocv<-function(x,dependent,predictors,export_coefficients=FALSE){

  #0) Check settings, set defaults:
  if(missing(x)){stop("No dataframe specified for x")}
  if(missing(dependent)){stop("No dependent variable specified")}
  if(missing(predictors)){stop("Please specify the selected predictors as a vector of strings.")}

  #1) Create a new list to store results of each iteration:
  loocv_i<-list()

  #2) Construct the right side of the modelling equation:
  right_eqn_side<-paste(c(predictors),collapse="+")

  #3) Leave out one observation:
  for(i in 1:dim(x)[1]){
    eval(parse(text=paste0("model_train<-lm(data=x[-i,],",dependent,"~",right_eqn_side,")")))
    #Apply the model to each of the data sets from which i was removed:
    loocv_i[[i]]<-c(predict(model_train,x[i,])[[1]],unname(model_train$coefficients))
  }

  #4) Collapse list to a dataframe:
  loocv<-as.data.frame(rbindlist(lapply(loocv_i,as.data.frame.list)))
  names(loocv)<-c("loocv","intercept",paste0(predictors,"_coeff"))

  #5) Merge back in to the original dataframe x:
  x<-cbind.data.frame(x,loocv)

  #6) Return the new dataframe with extra column "loocv":
  return(x)
}
