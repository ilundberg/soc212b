
* Note: first install the package
* ssc install crtrees

* Load data
use https://ilundberg.github.io/soc212b/data/simulated_data_for_trees.dta

* Regression tree
crtrees y x z, gen(yhat_tree) tree
scatter yhat_tree x, colorvar(z)

* Regression forest
crtrees y x z, rforests generate(yhat_forest) bootstraps(50)
* Visualize predictions
scatter yhat_forest x, colorvar(z)

rforest y x z, type(regression)
predict yhat_rforest
scatter yhat_rforest x, colorvar(z)

* Forest notes:
* 1) Each run of code creates a file matatree in your working directory.
*    You must delete this file to run again.
* 2) Ideally you use more than 50 bootstraps. If you omit the option,
*    defaults to 1,000. This may just be slow.
