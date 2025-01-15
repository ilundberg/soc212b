
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

* Estimate a multilevel model with fixed slope on team past record
* and random intercept for each team
use baseball_sample, clear
mixed salary team_past_record || team:
* With multilevel models, Stata doesn't like when you predict in a new dataset.
* Instead, we will predict within this dataset.
predict estimate, fitted
bysort team: keep if _n == 1
gen estimator = "multilevel"
keep team team_past_record estimate estimator
save multilevel_estimates, replace

* For comparison compare to OLS
use baseball_sample, clear
reg salary i.team
predict estimate
bysort team: keep if _n == 1
gen estimator = "ols"
keep team team_past_record estimate estimator

append using multilevel_estimates
encode estimator, gen(estimator_name)

scatter estimate team_past_record, colorvar(estimator_name) colordiscrete zlabel(, valuelabel) coloruseplegend 

