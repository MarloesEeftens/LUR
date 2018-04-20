\name{logocv}
\alias{logocv}
\title{Leave One Group Out Cross Validation (LOGOCV) for the fixed part of a mixed model.}
\description{This function takes a dataframe with n sites and the predictors you specified, and adds a column called "logocv" containing the predicted value for i, had the model been fit on the other n-1 sites and applied to site i.}
\usage{logocv(x,dependent,fixed,random=c("intercept"),group,export_estimates=FALSE)}
\arguments{
  \item{x}{An R dataframe which includes a dependent variable and all of the mentioned fixed and random effects.}
  \item{dependent}{The name of the variable which the model aims to estimate.}
  \item{fixed}{A vector of strings, including the names of all predictors which are included as "fixed effects" in the LUR model. These can be numeric or factors. Interactions can be included as well, in the same way as you typically do in a lmer model, like so: fixed = c("a","b","a*b")}.
  \item{random}{Defaults to c("intercept"): A vector of strings, including the names of all predictors which are to be included as "random effects" in the model.}
  \item{group}{The grouping variable responsible for the random component(s) of the model. In each iteration, the logocv function leaves out all observations which have the same value for the variable "group". For the estimation, only the fixed effects are used, which are calculated based on a model of the same structure, fit to the remaining groups.}
  \item{export_estimates}{Defaults to FALSE: If set to TRUE, this option will add one column to the output dataframe for each fixed predictor that was included. For a given predictor "ABC", the column will be named "ABC_coeff" and will have the value of its fixed effect as calculated for all groups in the dataset except the one that particular observation belongs to.}
}
\details{}
\value{The function returns the dataframe x, with an additional column name "logocv" which contains the predicted value for each observation, fitted on the entire dataset EXCEPT the group which that particular observation belongs to.}
\references{}
\author{Marloes Eeftens, marloes.eeftens@swisstph.ch}
\note{}
\seealso{}
\examples{
#Let's assume we did a monitoring campaign with 20 sites in 4 seasons
nr_sites<-20
nr_seasons<-4
#A unique id for each site:
site_id<-rep(1:nr_sites,nr_seasons)
season<-sort(rep(c("autumn","spring","summer","winter"),nr_sites))
#Let's say NO2 concentration varies by season in the following way:
season_no2<-c(rep(31,nr_sites),rep(28,nr_sites),rep(26,nr_sites),rep(35,nr_sites))
#And (independently) some normally distributed GIS predictor data (the same value for each unique site_id):
var1<-rep(rnorm(n=nr_sites,m=0,sd=1),nr_seasons)
var2<-rep(rnorm(n=nr_sites,m=0,sd=1),nr_seasons)
#Generate some normally distributed air pollution data:
NO2_conc<-season_no2+
  var1*3+var2*2+var1*rep(rnorm(n=nr_sites,m=0,sd=2))+
  rep(rnorm(n=nr_sites,m=0,sd=2),nr_seasons)+
  rnorm(n=nr_sites*nr_seasons,m=0,sd=2)
#Put all columns in a dataframe:
my_data<-cbind.data.frame(NO2_conc,season,site_id,var1,var2)

#Let's fit a model with a random intercept and slope:
lmer_fit<-lmer(data=my_data,NO2_conc~var1+var2+season+(1+var1|site_id),REML=F)
summary(lmer_fit)
library(MuMIn)
r.squaredGLMM(lmer_fit)

#Cross-validate the model, leaving out one group of observations at a time:
logocv_fit<-logocv(x=my_data,dependent="NO2_conc",fixed=c("var1","var2","season"),random=c("intercept","var1"),group="site_id")
plot(logocv_fit$NO2_conc,logocv_fit$logocv)
rsq(lm(data=logocv_fit,NO2_conc~logocv))
}
\keyword{}
