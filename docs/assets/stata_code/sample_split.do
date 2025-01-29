
clear all

import delimited "https://ilundberg.github.io/soc212b/data/baseball_population.csv"

* Convert team from string to categorical for regression
encode team, gen(teamFactor)
drop team
rename teamFactor team

* Draw a sample from the population
* 10 players per team without replacement
gsample 10, wor strata(team)

* Conduct sample split stratified by team
splitsample, generate(sample_var) balance(team)

* Save that sample split dataset
save sample_split, replace

* Work with a training set
keep if sample_var == 1

* Fit a regression
reg salary team_past_salary

* Switch to the test set
use sample_split, clear
keep if sample_var == 2

* Make predictions
predict yhat

* Create squared error
gen squared_error = (salary - yhat) ^ 2

* Summarize mean squared error
mean squared_error

