\name{loocv}
\alias{loocv}
\title{Leave One Out Cross Validation (LOOCV))}
\description{This function takes a dataframe with n sites and the predictors you specified, and adds a column called "loocv" containing the predicted value for i, had the model been fit on the other n-1 sites and applied to site i.}
\usage{loocv(x,dependent,predictors)}
\arguments{
  \item{x}{An R dataframe which includes a dependent variable and predictors.}
  \item{dependent}{The name of the variable which you would like to create a model for.}
  \item{predictors}{A vector of strings, including the names of all the predictor variables which should be considered for inclusion in your LUR model.}
}
\details{}
\value{The function returns the dataframe x, with an additional column names "loocv" which contains the predicted value for i, fitted on the entire dataset EXCEPT observation i.}
\references{}
\author{Marloes Eeftens, marloes.eeftens@swisstph.ch}
\note{While validation of any model on an independent, held-out  test set is always preferable, this is not always realistic if the number of training sites is small.}
\seealso{}
\examples{
#Let's assume we did a monitoring campaign with 20 sites and 80 predictors
nr_sites<-20
nr_predictors<-80

#Generate some normally distributed air pollution data:
NO2_conc<-rnorm(n=nr_sites,m=25,sd=8)
#And (independently) some normally distributed predictor data:
predictors<-as.data.frame(matrix(rnorm(n=nr_sites*nr_predictors,m=0,sd=1),nrow=nr_sites))
names(predictors)<-paste0("var_",seq(1:nr_predictors))
my_LUR_data<-cbind(NO2_conc,predictors)
#Find the best predictors for your model:
LUR_fit<-good_old_stepwise(x=my_LUR_data,dependent="NO2_conc",predictors=paste0("var_",seq(1:nr_predictors)))
#Sometimes LUR_fit will give you a model, sometimes not...
LUR_fit
#Did you get one? Great!
#Not so lucky? Not to worry! This is random data, not real life. Simply go up to the blank line, generate some more air pollution data, some new predictors, and re-run.

#The following only works if a model was actually possible...
#Extract the selected predictors from the model:
selected_preds<-c(names(LUR_fit$coefficients)[-1])
#Cross-validate:
LUR_data_loocv<-loocv(x=my_LUR_data,dependent="NO2_conc",predictors=selected_preds)
#Calculate the cross-validated R squared:
cor(LUR_data_loocv$NO2_conc,LUR_data_loocv$loocv)^2
#Plot measured concentrations against loocv predictions:
plot(x=LUR_data_loocv$NO2_conc,y=LUR_data_loocv$loocv)
}
\keyword{}
