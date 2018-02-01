#################################
##### LUR_random_simulation #####
##### By: Marloes Eeftens   #####
##### Last edit: 26/01/2018 #####
#################################

#Say we have done k monitoring campaigns, with i sites each. We wish to explain their contrasts with j predictors.
LUR_random_simulation<-function(dependent,nr_sites,nr_predictors,nr_repeats,plot_name){

  #0) Check settings:
  if(missing(dependent)){
    if(missing(nr_sites)){stop("Specify a either 'dependent' (if you want to use your own real data) or 'nr_sites' (if you want to randomly generate dependent data)")}
  }
  if(!missing(dependent)){
    if(!is.numeric(dependent)){stop("Dependent should be a numeric vector")}
    if(missing(nr_sites)){nr_sites<-length(dependent)}
    if(!missing(nr_sites)&length(dependent)!=nr_sites){
      message("Lenght of dependent vector does noet match nr_sites. Omit or change nr_sites to avoid this warning")
      nr_sites<-length(dependent)
    }
  }
  if(missing(nr_predictors)){stop("Specify a number or numeric vector for nr_predictors")}
  if(missing(nr_repeats)){nr_repeats<-1}

  #1) Initialize the dataframe where results should be stored
  overview<-setNames(data.frame(matrix(ncol=5,nrow=0)),c("sites","predictors","repeats","r2","n_pred"))

  #2) Make some datasets with i observations and j predictors, repeat k times
  #The dependent1 and the predictor variables are indepently generated, and normally distributed:
  for(i in nr_sites){
    for(j in nr_predictors){
      for(k in seq(1:nr_repeats)){
        if(missing(dependent)){dependent1<-rnorm(n=i,m=0,sd=1)}
        if(!missing(dependent)){dependent1<-dependent}
        predictors<-as.data.frame(matrix(rnorm(n=i*j,m=0,sd=1),nrow=i))
        names(predictors)<-paste0("var_",seq(1:j))
        data_i<-cbind(dependent1,predictors)
        fit_i<-good_old_stepwise(x=data_i,dependent="dependent1",predictors=paste0("var_",seq(1:j)),p=0.05)
        if(is.character(fit_i)){
          stats_ijk<-c(nr_sites=i,nr_predictors=j,nr_repeats=k,r2=0,nr_predictors_sel=0)}
        if(!is.character(fit_i)){
          stats_ijk<-c(nr_sites=i,nr_predictors=j,nr_repeats=k,r2=rsq(fit_i),nr_predictors_sel=length(fit_i$coefficients)-1)}
        overview[nrow(overview)+1,]<-stats_ijk
      }
    }
  }

  #3) Output a plot if required:
  if(!missing(plot_name)){
    #Summarize by sites and predictors:
    overview_summ<-overview %>% group_by(sites,predictors) %>% summarise(n_pred=mean(n_pred),r2=mean(r2))
    if(missing(dependent)){
      #Plot the average rsquared purely based on chance, as a function of nr_sites and nr_predictors:
      plot_title<-paste0("Average R squared obtained based on randomly generated predictors\nfor different combinations of #monitors and #predictors (",nr_repeats," repeated simulations)")
      p<-ggplot(overview_summ,aes(sites,predictors))+
        geom_tile(aes(fill=r2),colour="white")+
        theme(plot.title=element_text(size=10))+
        scale_fill_gradient(low="white",high="black")+
        theme_bw()+
        xlab("Number of monitoring sites")+
        ylab("Number of predictors evaluated")+
        ggtitle(plot_title)+
        labs(fill="Average R squared")
    }
    if(!missing(dependent)){
      #Plot the average rsquared and number of predictors selected based on chance, as a function nr_predictors:
      overview_melt<-melt(overview,id.vars=c("sites","predictors","repeats"))
      overview_summ_melt<-melt(overview_summ,id.vars=c("sites","predictors"))
      overview_melt$variable<-ifelse(overview_melt$variable=="r2","R squared","Number of predictors")
      overview_summ_melt$variable<-ifelse(overview_summ_melt$variable=="r2","R squared","Number of predictors")
      plot_title<-paste0("Number of predictors selected and R squared obtained based on randomly generated\npredictors (",nr_repeats," repeated simulations)")
      p<-ggplot(data=overview_melt,aes(x=predictors,y=value))+
        geom_boxplot(aes(group=predictors))+
        theme(plot.title=element_text(size=10))+
        facet_wrap(~variable,nrow=1,scales="free")+
        theme_bw()+
        xlab("Number of predictors evaluated")+
        ylab("")+
        ggtitle(plot_title)+
        geom_point(data=overview_melt,aes(x=jitter(predictors),y=value))+
        geom_line(data=overview_summ_melt,aes(x=predictors,y=value),col="red",size=2)
    }
    ggsave(plot_name,plot=p,units="cm",dpi=600,width=20,height=12)
  }

  #4) Return the resulting overview of model performance statistics:
  return(overview)
}
