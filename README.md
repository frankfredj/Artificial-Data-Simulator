# Artificial-Data-Simulator
Simulating artificial data based on a provided sample

# Inputs

* data: the sample on which the artificial data will be based.
* n: the number of observations to be simulated.
* unique.num: a numerical predictor with fewer unique values than unique.num will be converted to a categorical one.
* outliers.remove: if TRUE, a multivariate 0.95 CI will be constructed based on PCA and outliers will be removed.

# Methodology

The data's missing values will be imputed via random forest if needed. The resulting output will be converted to a matrix containing
numerical values, which will be used to find the PCA rotation matrix. Independent PCs vectors of observations within the PCA space will 
then be simulated before being mapped back to our original space.

Categorical predictors will be retrieved based on the simulated numerical values, and numerical values falling off the original's data 
range will be removed. 

This process is iterated until we obtain a sample of size >= n.

If outliers.remove is set to TRUE, the sum of the squared components of the scaled / centered simulated PC vectors will be used to fit a
pdf via mle (or mme if convergence fails). Said pdf will be used to form a two-sided 0.95 CI, and outliers will be removed accordingly.

# Efficiency

A dataset pertaining to kidney cancer (binary regression) of size (100, 25) was used to simulated an artificial dataset of size (500, 25).
The methodology described in the Binary-Classification-workshop repository (model stacking via majority voting) was used to predict the original dataset's outcome variable
based on a model trained on the artificial data. 

The following results were obtained:

![](https://i.imgur.com/jc6sJVq.png)

Thus indicating that the function works as intended.

