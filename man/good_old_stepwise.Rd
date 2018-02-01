\name{good_old_stepwise}
\alias{good_old_stepwise}
\title{Fit a good old stepwise LUR model!}
\description{Given a dataframe with a dependent variable and a number of predictors to be evaluated in a stepwise manner, this function returns the output of a linear model ("lm") including those predictors which give you the optimal adjusted Rsquared, within the statistical constraints below, assuming a model of the structure: dependent = intercept + beta1*predictor1 + beta2*predictor2 +...}
\usage{good_old_stepwise(x,dependent,predictors,p)}
\arguments{
  \item{x}{An R dataframe which includes a dependent variable and predictors.}
  \item{dependent}{The name of the variable which you would like to create a model for.}
  \item{predictors}{A vector of strings, including the names of all the predictor variables which should be considered for inclusion in your LUR model.}
  \item{adj_rsq_improvement}{(Optional, defaults to 0.01.) The minimal improvement in adj_rsq that is needed to increase model complexity by including an additional variable.}
  \item{coeff_sign}{(Optional, defaults to c(1).) If coeff_sign=c(1), we a priori define a direction of effect for all predictors: as their value increases, so does the level of the dependent. (Make sure to flip the sign of predictors for which you want to define a negative impact on the dependent, e.g. distance from a source, presence of pollution sinks). If you do not want to specify a direction of effect, set coeff_sign=c(-1,1). This way you accept both negative and positive effects. (It makes no sense to include 0, since we don't want variables without any effect.) have been flipped prior to evaluation, so that these also have positive coefficients.}
  \item{pvalue_max}{(Optional, defaults to 0.05.) A threshold for the P value: this allows you to only include additional variables if they yield a level of significance which is greater than p.}
  \item{VIF_max}{(Optional, defaults to 3.) A high variance inflation factor (of VIF) indicates that two predictors are collinear, which can lead to unstable coefficients and wobbly models.}
  \item{cooksD_max}{(Optional, defaults to 1.) A high Cook's D value for one or several points indicates that these points have a high influence on the model coefficients. If these depend heavily on a single or a few sites only, the model coefficients are less stable.}
}
\details{Assumptions: 1) We maximize adj_rsq; 2) Observations are independent; 3) All predictors are equally eligible for inclusion; 4) There is no maximum to the number of variables which can be included by the model; 5) All variables are assumed to be linearly related to the dependent.}
\value{The result of this function is the result of a lm() with all selected predictors included.}
\references{Eeftens, Marloes, et al. "Development of land use regression models for PM2.5, PM2.5 absorbance, PM10 and PMcoarse in 20 European study areas; results of the ESCAPE project." Environmental science & technology 46.20 (2012): 11195-11205.}
\author{Marloes Eeftens, marloes.eeftens@swisstph.ch}
\note{This function is preliminary and has many settings and defaults. I hope all makes sense, otherwise please suggest edits! :-)}
\seealso{}
\examples{
#Let's assume we did a monitoring campaign with 20 sites and 80 predictors
nr_sites<-20
nr_predictors<-80

#Generate some normally distributed air pollution data:
NO2_measurements<-rnorm(n=nr_sites,m=25,sd=8)
#And (independently) some normally distributed predictor data:
predictors<-as.data.frame(matrix(rnorm(n=nr_sites*nr_predictors,m=0,sd=1),nrow=nr_sites))
names(predictors)<-paste0("var_",seq(1:nr_predictors))
my_LUR_data<-cbind(NO2_measurements,predictors)
LUR_fit<-good_old_stepwise(x=my_LUR_data,dependent="NO2_measurements",predictors=paste0("var_",seq(1:nr_predictors)))
#Sometimes LUR_fit will give you a model, sometimes not...
LUR_fit
#Did you get one? Great!
#Not so lucky? Not to worry! This is random data, not real life. Simply go up to the blank line, generate some more air pollution data, some new predictors, and re-run.

#The following only works if a model was actually possible...
summary(LUR_fit)
LUR_fit has the structure of a typical model output of the lm() function.
#This is how much contrast we could explain in the dependent data.
rsq(LUR_fit)
}
\keyword{}
