\name{LUR_random_simulation}
\alias{LUR_random_simulation}
\title{Simulate your LUR modelling process using randomly generated (instead of real) predictor variables.}
\description{This simulation takes dependent (pollution) data, and predicts these values using a user-specified number of randomly generated predictor variables. If you find that the models generated from evaluating simulated predictors are just as good as your actual models, it is time to consider if you are maybe evaluating too many predictors for the number of sites whose variance you are trying to explain. If the simulated predictors perform worse than your real ones, it is more likely that you are looking at an actual relationship between predictors and dependent.}
\usage{LUR_random_simulation(dependent,nr_sites,nr_predictors,nr_repeats,plot_folder)}
\arguments{
  \item{dependent}{(Optional, no default. Specify a dependent if you want to see how well random variables do in explaining your "actual" concentrations measured.) Dependent should be a numeric vector.}
  \item{nr_sites}{(Optional, defaults to length(dependent). Specifying nr_sites only makes sense if you don't have an "actual" dependent whose variability you're trying to explain.) Specify nr_sites as a numeric vector (e.g. 'nr_sites=c(20,25,30)') to generate the pollutant concentrations for the number(s) of sites you want to predict for.}
  \item{nr_predictors}{A numeric vector, specifying the number(s) of random predictors you want to generate to evaluate their explatory power for your dependent. E.g. 'nr_predictors=40', or nr_predictors=c(10,15,20,25)'}
  \item{nr_repeats}{(Optional, defaults to 1.) Specify how many times you want to repreat each simulation for a combination of nr_sites and nr_predictors. Repeated simulations are a good idea, because (obviously) not every single set of random predictors explains the data equally well. Sometimes a model will not be possible.}
  \item{plot_folder}{(Optional, no default.) Specify a plot folder if you want. If you evaluated a range of values for nr_predictors, the plot will show R squared and the number of selected predictors, as a function of the number of evaluated predictors. If you evaluated a range of values for nr_predictors AND nr_sites, the plot will be a heatmap, showing the average R2 by nr_predictors and nr_sites.}
}
\details{This function is inspired on a series of papers (see references) which showed that model explanatory ability does not equal predictive ability.}
\value{The output is a dataframe containing one row for each simulation, so for each nr_sites, each nr_predictors and each repeat that was evaluated. It is possible to summarize this output table in a graph. If no model was found, the nr_}
\references{LUR model fitting with randomly generated variables was previously done by Basagaña et al. (2012): Basagaña, Xavier, et al. "Effect of the number of measurement sites on land use regression models in estimating local air pollution." Atmospheric environment 54 (2012): 634-642.}
\author{Marloes Eeftens, marloes.eeftens@swisstph.ch}
\note{Obviously, the actual simulated models are useless for predicting any variability in independent datasets. They just serve to calculate for any nr_predictors, how likely it is that a LUR model with similar explanatory power is accepted (and subsequently used for predictions), despite being based on a coincidental linear combination of random numbers, or, in other words: nothing at all...}
\seealso{}
\examples{
#Let's see how R squared increases with nr_predictors and decreases with nr_sites:
#This simulation ran for about 2 minutes
sim1<-LUR_random_simulation(nr_sites=c(20,25,30,35,40,45,50),
        nr_predictors=c(20,30,40,50,60,70,80),
        nr_repeats=5,
        plot_name="V:/EEH/R_functions/LUR/sim1.png")

#2) Now let's take some actual NO2 measurements and see what the chance is to predict those as well with random numbers as with real predictors:
my_real_NO2_conc<-c(12.5, 22.2, 23.1, 35.2, 34.4, 22.9, 35.1, 26.7, 16.4, 23.2,
                    24, 24.2, 37, 21, 39.6, 24.7, 39.4, 20.5, 19.7, 18.4, 17.1, 19.9,
                    39.6)
#Let's say we evaluate between 20 and 80 predictors again:
#This simulation ran for about 32 seconds
sim2<-LUR_random_simulation(dependent=my_real_NO2_conc,
        nr_predictors=c(20,30,40,50,60,70,80),
        nr_repeats=10,
        plot_name="V:/EEH/R_functions/LUR/sim2.png")
sim2_summ<-sim2 %>%
  group_by(sites,predictors) %>%
  summarise(n_pred=mean(n_pred),r2=mean(r2))
#Both from the plot, and from the summary table, it is clear that the number of selected predictors and the R squared increase, as we increase the nr_predictors.
sim2_summ

#3) Now let's say we evaluated 40 predictors in our real model, let's see how well random variables do:
#This simulation ran for about 25 seconds
sim3<-LUR_random_simulation(dependent=my_real_NO2_conc,
        nr_predictors=c(40),
        nr_repeats=100)
#Let's see how many models explained as much variance as our real model, which yielded and R squared of
real_rsq<-0.76
#These are the random models which explained more variance than our real model:
sim3[sim3$r2>=real_rsq,]
#These are ones which also just needed <=2 predictors to do that:
sim3[sim3$r2>=real_rsq&sim3$n_pred<=2,]
}
\keyword{}
