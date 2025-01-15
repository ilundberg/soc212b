
clear all

* Import the data
clear all
import delimited "https://ilundberg.github.io/soc212b/data/baseball_population.csv"

* Convert team from string to categorical for regression
encode team, gen(teamFactor)
drop team
rename teamFactor team

* Save the population
save baseball_population, replace

* Sample cases per team for learning, without replacement
gsample 5, strata(team) wor
* Save the learning sample
save baseball_sample, replace

* Estimate a ridge regression model.
* alpha(0) says to estimate ridge regression
* We will learn about selection(cv) in a future week on model selection
* The grid() option tells it to consider lower values of lambda than it ordinarily
* considers, because in this example I found it was always choosing lambda too large
lasso linear salary i.team, selection(cv) grid(,min(.1))
estimates save lasso_estimates

* Side step: Compare to OLS
predict yhat
reg salary i.team
predict ols
twoway (scatter yhat ols) (line ols ols)

* Load prediction data
clear
use baseball_population

* Make predictions
estimates use lasso_estimates
predict yhat

* Summarize predicted values within teams
bysort team: egen estimate = mean(yhat)
bysort team: keep if _n == 1
keep team estimate

* Sort by estimate value and look at results
sort estimate
browse
