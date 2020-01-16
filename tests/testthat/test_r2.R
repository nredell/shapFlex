#------------------------------------------------------------------------------
# Test that r2() is working correctly.
library(shapFlex)

test_that("r2() sigma unique is working correctly", {

  seed <- 1

  n_instances <- 10
  n_features <- 5

  set.seed(seed)
  X <- matrix(round(runif(n_instances * n_features, -5, 5), 0), ncol = n_features)

  set.seed(seed)
  y <- runif(10, 10, 20)

  data <- data.frame(y, X)
  #----------------------------------------------------------------------------
  model <- stats::lm(y ~ ., data)
  #----------------------------------------------------------------------------
  predict_function <- function(model, data) {

    data_pred <- data.frame("y_pred" = predict(model, data))
    return(data_pred)
  }
  #----------------------------------------------------------------------------
  explain <- data[, -1]

  sample_size <- 60  # Number of Monte Carlo samples.
  #----------------------------------------------------------------------------
  set.seed(seed)
  data_shap <- shapFlex::shapFlex(explain = explain,
                                  model = model,
                                  predict_function = predict_function,
                                  sample_size = sample_size)
  #----------------------------------------------------------------------------
  data_shap_wide <- tidyr::pivot_wider(data_shap, id_cols = "index",
                                       names_from = "feature_name", values_from = "shap_effect")

  data_shap_wide$index <- NULL
  #----------------------------------------------------------------------------

  intercept <- unique(data_shap$intercept)

  scale <- "r2"

  data_r2 <- shapFlex::r2(data_shap_wide, y, intercept, scale)
  #----------------------------------------------------------------------------
  unique(data_r2$sigma_unique) - .998 < .0001  # Hardcoded tolerance for this seed.
})
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
