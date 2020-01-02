[![lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![Travis Build Status](https://travis-ci.org/nredell/shapFlex.svg?branch=master)](https://travis-ci.org/nredell/shapFlex)
                                                                               
# package::shapFlex <img src="./tools/shapFlex_logo.png" alt="shapFlex logo" align="right" height="138.5" style="display: inline-block;">
                                                                               
The purpose of `shapFlex`, short for Shapley flexibility, is to compute stochastic feature-level Shapley values which 
can be used to (a) interpret and/or (b) assess the fairness of any machine learning model while 
**incorporating causal constraints into the model's feature space**. **[Shapley values](https://christophm.github.io/interpretable-ml-book/shapley.html)** 
are an intuitive and theoretically sound model-agnostic diagnostic tool to understand both **global feature importance** across all instances in a data set 
and instance/row-level **local feature importance** in black-box machine learning models.

This package implements the algorithm described in 
[Štrumbelj and Kononenko's (2014) sampling-based Shapley approximation algorithm](https://link.springer.com/article/10.1007%2Fs10115-013-0679-x) 
to compute the stochastic Shapley values for a given model feature and the algorithm described in 
[Frye, Feige, & Rowat's (2019) Asymmetric Shapley values: incorporating causal knowledge into model-agnostic explainability](https://arxiv.org/pdf/1910.06358.pdf) 
to incorporate prior knowledge into the Shapley value calculation. Asymmetric Shapley values can be tuned by the researcher to avoid splitting 
the Shapley feature effects uniformly across related/correlated features--as is done in the symmetric case--and focus on the unique effect of a target 
feature after having conditioned on other pre-specified "causal" feature effects.

* **Flexibility**: 
    + Shapley values can be estimated for <u>any machine learning model</u> using a simple user-defined 
    `predict()` wrapper function.
    + Shapley values can be estimated by incorporating prior knowledge about causaility in the feature space; this is especially 
    useful for interpreting time series models with a temporal dependence.

* **Speed**:
    + The code itself hasn't necessarily been optimized for speed. The speed advantage of `shapFlex` comes in the form of giving the user the ability 
 to <u>select 1 or more target features of interest</u> and avoid having to compute Shapley values for all model features. This is especially 
 useful in high-dimensional models as the computation of a Shapley value is exponential in the number of features.
 
 The main function in this package is `shapFlex::shapFlex()`.
 
 ***
   
   ## Install
   
   ``` r
 devtools::install_github("nredell/shapFlex")
 library(shapFlex)
 ```
 
 ## Getting Started
 
 Detailed **[shapFlex overview vignette](https://nredell.github.io/data_science_blog/shapFlex/)**.
 
 **[shapFlex runtime/speed vignette](https://nredell.github.io/data_science_blog/shapFlex/speed/)**.
 
 **[shapFlex consistency vignette](https://nredell.github.io/data_science_blog/shapFlex/consistency/)**: A look at how the 
 stochastic Shapley values of `shapFlex` correlate with the exact Shapley values produced by `catboost`'s implementation of 
**[shap](https://github.com/slundberg/shap)**.

## Example

* Below is an example of how `shapFlex` can be used to compute Shapley values for measuring the effect of 
~20 model features used in an ensemble of 2 machine learning models on the price of cars in the `imports85` 
dataset from the `randomForest` package.

``` r
library(dplyr)
library(shapFlex)
library(glmnet)
library(randomForest)
library(future)

data("imports85", package = "randomForest")
data <- imports85

data <- data[, -2]  # this column has excessive missing data.
data <- data[complete.cases(data), ]
row.names(data) <- 1:nrow(data)  # re-index for simplicity.
#------------------------------------------------------------------------------
# Train machine learning models.

outcome_col <- which(names(data) == "price")
outcome_name <- names(data)[outcome_col]

model_formula <- formula(paste0(outcome_name,  "~ ."))

set.seed(224)
model_lasso <- glmnet::cv.glmnet(x = model.matrix(model_formula, data),
                                 y = as.matrix(data[, outcome_col, drop = FALSE], ncol = 1))

set.seed(224)
model_rf <- randomForest::randomForest(formula = model_formula, data = data, ntree = 200)
#------------------------------------------------------------------------------
# A user-defined prediction function that takes 2 positional arguments and returns
# a 1-column data.frame (see the package overview vignette for more info).

predict_function <- function(models, data) {

  y_pred_lasso <- data.frame(predict(models[[1]], model.matrix(~ ., data)))  # LASSO
  y_pred_rf <- data.frame(predict(models[[2]], data))  # Random Forest

  data_pred <- dplyr::bind_cols(y_pred_lasso, y_pred_rf)

  data_pred <- data.frame("y_pred" = rowMeans(data_pred, na.rm = TRUE))

  return(data_pred)
}
#------------------------------------------------------------------------------
# Setting key shapFlex::shapFlex() parameters.

# A list of data.frame(s) of model features suitable for the user-defined predict function(s).
data_list <- list(data[, -(outcome_col), drop = FALSE])
# Dataset row numbers or indicies.
explain_instances <- 1:5
# Are the instances to explain row numbers/indices or row names ('row_name') in the input data?
explain_instance_id <- "row_index"
# A list of of model objects. Nested lists of length(data_list) are needed if length(data_list) > 1.
models <- list(model_lasso, model_rf)
# A list of length 1 vectors with length(prediction_functions) == length(data_list).
predict_functions <- list(predict_function)
#nThe number of randomly selected comparison rows or instances used to compute the feature-level Shapley value.
sample_size <- 60

#------------------------------------------------------------------------------
# Explaining 1 instance at a time-of 5 total-with shapFlex::shapFlex().

# Setup multi-core or multi-session parallel processing.
future::plan(future::multiprocess)

explained_instances <- vector("list", length(explain_instances))

set.seed(224)
for (i in seq_along(explain_instances)) {

  explained_instances[[i]] <- shapFlex::shapFlex(data = data_list,
                                                 explain_instance = explain_instances[i],  # loop
                                                 explain_instance_id = explain_instance_id,
                                                 models = models,
                                                 predict_functions = predict_functions,
                                                 sample_size = sample_size,
                                                 shuffle = 1,
                                                 use_future = TRUE)
}
#------------------------------------------------------------------------------
# Return the list of instance-level results as a data.frame.

data_shap <- dplyr::bind_rows(explained_instances)

DT::datatable(data_shap)
```
![](./tools/shapFlex_output.png)

***

## Cite

At the moment, the best citation for this package is related to the `shapFlex::r2()` function.

Redell, N. (2019). [Shapley decomposition of R^2 in machine learning models](https://arxiv.org/abs/1908.09718). arXiv preprint arXiv:1908.09718.

## References

Štrumbelj, E. & Kononenko, I. (2014) Explaining prediction models and individual predictions with feature contributions. Knowl Inf Syst (2014) 41: 647. https://doi.org/10.1007/s10115-013-0679-x
