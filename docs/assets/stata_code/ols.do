
clear all

* Import the data
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

* Estimate an OLS model
reg salary i.team

* Load prediction data
clear
use baseball_population

* Make predictions
predict yhat

* Summarize predicted values within teams
bysort team: egen estimate = mean(yhat)
bysort team: keep if _n == 1
keep team estimate

* Sort by estimate value and look at results
sort estimate
browse
