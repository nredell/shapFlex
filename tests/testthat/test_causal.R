#------------------------------------------------------------------------------
# Test that causal specifications are calculated correctly.
library(shapFlex)
library(randomForest)

test_that("a feature that is both a causal target and causal effect is computed correctly", {

  data("data_adult", package = "shapFlex")

  data <- data_adult[1:300, ]

  outcome_name <- "income"
  outcome_col <- which(names(data) == outcome_name)

  model_formula <- formula(paste0(outcome_name,  "~ ."))

  set.seed(1)
  model <- randomForest::randomForest(model_formula, data = data, ntree = 300)
  #------------------------------------------------------------------------------
  predict_function <- function(model, data) {
    data_pred <- data.frame("y_pred" = predict(model, data, type = "prob")[, 2])
    return(data_pred)
  }
  #------------------------------------------------------------------------------
  explain <- data[1:10, -outcome_col]
  reference <- data[, -outcome_col]
  sample_size <- 60
  target_features <- c("marital_status", "education")
  # Features are both causes and effects so
  causal <- list(marital_status ~ education,
                 education ~ marital_status)
  #------------------------------------------------------------------------------
  set.seed(1)
  explained_full <- shapFlex::shapFlex(explain = explain,
                                       reference = reference,
                                       model = model,
                                       predict_function = predict_function,
                                       target_features = target_features,
                                       causal = causal,
                                       causal_weights = rep(1, length(causal)),  # Pure causal weights
                                       sample_size = sample_size)
  #------------------------------------------------------------------------------
  set.seed(1)
  explained_half <- shapFlex::shapFlex(explain = explain,
                                       reference = reference,
                                       model = model,
                                       predict_function = predict_function,
                                       target_features = target_features,
                                       causal = causal,
                                       causal_weights = rep(.5, length(causal)),  # Approximates symmetric calc.
                                       sample_size = sample_size)
  #------------------------------------------------------------------------------
  identical(explained_full, explained_half)
})
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
