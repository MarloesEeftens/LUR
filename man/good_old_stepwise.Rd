\name{good_old_stepwise}
\alias{good_old_stepwise}
\title{Fit a good old stepwise LUR model!}
\description{Given a dataframe with a dependent variable and a number of predictors to be evaluated in a stepwise manner, this function returns those predictors which give you the optimal adjusted Rsquared, assuming a model of the structure: dependent = intercept + beta1*predictor1 + beta2*predictor2 +...}
\usage{good_old_stepwise(x,dependent,predictors,p)}
\arguments{
  \item{x}{An R dataframe which includes a dependent variable and predictors.}
  \item{dependent}{The name of the variable which you would like to create a model for.}
  \item{predictors}{A vector of strings, including the names of all the predictor variables which should be considered for inclusion in your LUR model.}
  \item{p}{A threshold for the P value: this allows you to only include additional variables if they yield a level of significance which is greater than p.}
}
\details{Assumptions: 1) We maximize adj_rsq; 2) Observations are independent; 3) All predictors are equally eligible for inclusion; 4) There is no maximum to the number of variables which can be included by the model; 5) All variables are assumed to be linearly related to the dependent.}
\value{The result of this function is the result of a glm() with all selected predictors included.}
\references{Eeftens, Marloes, et al. "Development of land use regression models for PM2.5, PM2.5 absorbance, PM10 and PMcoarse in 20 European study areas; results of the ESCAPE project." Environmental science & technology 46.20 (2012): 11195-11205.}
\author{Marloes Eeftens, marloes.eeftens@swisstph.ch}
\note{This version is quite preliminary. Please suggest edits! :-)}
\seealso{}
\examples{
#Let's assume we did a monitoring campaign with 20 sites and 60 predictors
nr_sites<-20
nr_predictors<-60
#Generate some normally distributed air pollution data:
dependent<-rnorm(n=nr_sites,m=0,sd=1)
#And (independently) some normally distributed predictor data:
predictors<-as.data.frame(matrix(rnorm(n=nr_sites*nr_predictors,m=0,sd=1),nrow=nr_sites))
names(predictors)<-paste0("var_",seq(1:nr_predictors))
data_ij<-cbind(dependent,predictors)
LUR_fit<-good_old_stepwise(x=data_ij,dependent="dependent",predictors=paste0("var_",seq(1:nr_predictors)),p=0.05)
LUR_fit
#The following only works if a model was actually possible...
summary(LUR_fit)
#This is how much contrast we could explain in the dependent data.
rsq(LUR_fit)
}
\keyword{}
