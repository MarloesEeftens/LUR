#################################
##### screen_predictors     #####
##### By: Marloes Eeftens   #####
##### Last edit: 26/01/2018 #####
#################################

screen_predictors<-function(x,predictors,details){

  #0) Check inputs
  if(!is.data.frame(x)){stop("Specify a dataframe x containing the predictors you want to evaluate")}
  if(missing(predictors)){names(x)}
  if(missing(details)){details=FALSE}

  #1) Initialize a place to put the high-risk predictors:
  overview_predictors<-setNames(data.frame(matrix(ncol=3,nrow=0)),c("predictor","flag","note"))

  #2) Loop through the list of predictors to flag the high risk ones:
  for(i in predictors){
    note1<-c()
    note2<-c()
    note3<-c()
    flag<-FALSE
    dat_sel<-subset(x,select=i)[,1]
    same_value<-max(summary(as.factor(dat_sel)))
    if(length(dat_sel)-same_value<=5){
      flag<-TRUE
      note1<-paste0(same_value," sites have the same value")
    }
    if(min(dat_sel)<quantile(dat_sel,0.1)-3*(quantile(dat_sel,0.9)-quantile(dat_sel,0.1))){
      flag<-TRUE
      note2<-"Outlier at the lower end of the distribution"
    }
    if(max(dat_sel)>quantile(dat_sel,0.9)+3*(quantile(dat_sel,0.9)-quantile(dat_sel,0.1))){
      flag<-TRUE
      note3<-"Outlier at the higher end of the distribution"
    }
    predictor_i<-c(predictor=i,flagged=flag,note=paste(c(note1,note2,note3),collapse="; "))
    overview_predictors[nrow(overview_predictors)+1,]<-predictor_i
  }

  #3) Return the predictors with a high risk:
  flagged_predictors<-c(overview_predictors$predictor[overview_predictors$flag==TRUE])
  if(details==TRUE){flagged_predictors<-list(flagged_predictors,overview_predictors)}
  return(flagged_predictors)
}
