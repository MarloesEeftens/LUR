#################################
##### good_old_stepwise     #####
##### By: Marloes Eeftens   #####
##### Last edit: 29/01/2018 #####
#################################

good_old_stepwise<-function(x,dependent,predictors,adj_rsq_improvement=0.01,coeff_sign=c(1),pvalue_max=0.05,VIF_max=3,cooksD_max=1){

  #0) Check inputs, set defaults:
  if(is.data.frame(x)==FALSE){stop("No data frame specified for x.")}
  if(missing(dependent)|!is.vector(dependent)|!is.character(dependent)){stop("Specify dependent as a character vector")}
  if(missing(predictors)|!is.vector(predictors)|!is.character(predictors)){stop("Specify predictors as a character vector")}

  #1) Set initial variables: model Rsq is 0 and there are no predictors selected
  model_adj_rsq<-0
  model_adj_rsq_1<-model_adj_rsq
  predictors_sel<-c()
  options_exhausted<-FALSE

  #2) While the model_adj_rsq of the new model is at least 0.01 higher than the model_adj_rsq of
  #   the old model (or model_adj_rsq_1): keep evaluating predictors.
  while(options_exhausted==FALSE){
    adj_r2<-c()
    r2<-c()
    new_coeff<-c()
    lowest_coeff<-c()
    pvalue<-c()
    max_VIF<-c()
    max_cooksD<-c()
    new_predictor<-c()
    #Compile a list of all the predictors that should be evaluated in the model
    right_eqn_side<-paste(c("var_new",predictors_sel),collapse="+")
    #Loop through all the potential predictors, saving adj_r2, new_coeff and pvalue
    for(i in predictors){
      x$var_new<-subset(x,select=i)[,1]
      eval(parse(text=paste0("model_fit<-lm(",dependent,"~",right_eqn_side,",data=x)")))
      adj_r2<-c(adj_r2,rsq(model_fit,adj=TRUE))
      r2<-c(r2,rsq(model_fit))
      new_coeff<-c(new_coeff,model_fit$coeff[[2]])
      lowest_coeff<-c(lowest_coeff,min(model_fit$coefficients[-1]))
      pvalue<-c(pvalue,coef(summary(model_fit))[,4][2])
      max_VIF<-c(max_VIF,ifelse(length(predictors_sel)==0,1,max(car::vif(model_fit))))
      max_cooksD<-c(max_cooksD,max(cooks.distance(model_fit)))
    }
    #Make an overview of all the models:
    overview<-cbind.data.frame(predictors,adj_r2,r2,new_coeff,lowest_coeff,pvalue,max_VIF,max_cooksD)
    overview<-overview[order(overview$adj_r2,decreasing=TRUE),]#Select only those predictors which meet our criteria:
    overview<-subset(overview,
      adj_r2>model_adj_rsq+adj_rsq_improvement& #New model should improve upon previous model
      sign(new_coeff) %in% coeff_sign&          #Coefficient should be in coeff_sign
      sign(lowest_coeff) %in% coeff_sign&       #The sign of the lowest coefficient should be in coeff_sign
      pvalue<=pvalue_max&                       #Pvalue should be below the threshold p
      max_VIF<VIF_max&                          #Maximum VIF should be lower than VIF_max
      max_cooksD<cooksD_max)                    #Maximum cooksD should be lower than cooksD_max
    #Now we see if there are any suitable variables left to select:
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

  #3) Once there are no more variables which improve the model any further, run the final model:
  if(length(predictors_sel)>0){
    right_eqn_side_sel<-paste(predictors_sel,collapse="+")
    eval(parse(text=paste0("model_fit_sel<-lm(",dependent,"~",right_eqn_side_sel,",data=x)")))
  }
  #  Or return a message that no relevant predictors could be identified:
  if(length(predictors_sel)==0){
    model_fit_sel<-"No predictors were found which explain the dependent variable..."
  }

  #4) Return the model with the selected variables included!
  return(model_fit_sel)
}
