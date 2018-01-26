#################################
##### good_old_stepwise     #####
##### By: Marloes Eeftens   #####
##### Last edit: 26/01/2018 #####
#################################

good_old_stepwise<-function(x,dependent,predictors,p){

  #0) Set initial variables: model Rsq is 0 and there are no predictors selected
  model_adj_rsq<-0
  model_adj_rsq_1<-model_adj_rsq
  predictors_sel<-c()
  options_exhausted<-FALSE

  #1) While the model_adj_rsq of the new model is at least 0.01 higher than the model_adj_rsq of
  #   the old model (or model_adj_rsq_1): keep evaluating predictors.
  while(options_exhausted==FALSE){
    adj_r2<-c()
    r2<-c()
    coeff<-c()
    pvalue<-c()
    new_predictor<-c()
    #Compile a list of all the predictors that should be evaluated in the model
    right_eqn_side<-paste(c("var_new",predictors_sel),collapse="+")
    #Loop through all the potential predictors, saving adj_r2, coeff and pvalue
    for(i in predictors){
      x$var_new<-subset(x,select=i)[,1]
      eval(parse(text=paste0("model_fit<-glm(",dependent,"~",right_eqn_side,",data=x)")))
      adj_r2<-c(adj_r2,rsq(model_fit,adj=TRUE))
      r2<-c(r2,rsq(model_fit))
      coeff<-c(coeff,model_fit$coeff[[2]])
      pvalue<-c(pvalue,coef(summary(model_fit))[,4][2])
    }
    #Make an overview and select only those predictors which meet our criteria:
    #- A pvalue below the threshold p
    #- A coefficient that is positive
    #- An improvement upon the previous iteration of the model
    overview<-cbind.data.frame(predictors,adj_r2,r2,coeff,pvalue)[order(-adj_r2),]
    overview<-subset(overview,coeff>0&pvalue<=p&r2>0&adj_r2>model_adj_rsq+0.01)
    if(dim(overview)[1]==0){
      options_exhausted<-TRUE
    }
    if(dim(overview)[1]!=0){
      #The previous adj_r2 is now saved as model_adj_rsq_1
      model_adj_rsq_1<-model_adj_rsq
      #The new model's adj_r2 is saved
      model_adj_rsq<-head(overview$adj_r2,1)
      #The variable is added to the list of selected predictors
      new_predictor<-as.character(head(overview$predictors,1))
      predictors_sel<-c(predictors_sel,new_predictor)
      #These are the ones we continue to evaluate:
      predictors<-predictors[-which(predictors==new_predictor)]
    }
  }

  #2) Whether there were no more variables which met the criteria ("") or whether the last one selected
  #   did not result in an increased adj_r2, the last variable clearly did not improve the model any
  #   further and should be removed:
  if(length(predictors_sel)>0){
    right_eqn_side_sel<-paste(predictors_sel,collapse="+")
    eval(parse(text=paste0("model_fit_sel<-glm(",dependent,"~",right_eqn_side_sel,",data=x)")))
  }
  if(length(predictors_sel)==0){
    model_fit_sel<-"No predictors were found which explain the dependent variable..."
  }

  #3) These are the predictors which we ultimately select for our model!
  return(model_fit_sel)
}
