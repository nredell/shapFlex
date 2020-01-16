#' Compute feature importances using a Shapley variance decomposition of R^{2}
#'
#' This function uses pre-computed Shapley values to decompose the overall model R^{2} into feature-level
#' attributions of variance explained using the formulation of Redell (2019) arXiv:1908.09718.
#'
#' @param shap A matrix or data.frame of Shapley values. The dimensions should be 'number of instances' by
#' 'number of features'. The predicted outcome for each instance, used in the R^{2} caluclation, is the row
#' sum of Shapley values across columns + the user-supplied \code{intercept}.
#' @param y A \code{length(nrow(shap)} numeric vector or 1-column matrix or data.frame with the outcome being predicted.
#' @param intercept A length-1 numeric vector giving the model's average prediction. The \code{intercept}
#' is returned in \code{shapFlex()}.
#' @param scale The scaling of the feature importances. \code{r2} (default) scales feature-level importances to the
#' overall model R^{2} while \code{1} scales feature importances along a 0 to 1 scale.
#' @return A data.frame of the feature importances with the model's global R^{2} ('r2'), the feature-level
#' importances or attribution of variance explained ('r2_shap'), and the proportion of variance between the
#' baseline or intercept-only model and the final model that can be uniquely ascribed to a given feature
#' ('sigma_unique').
#' @export
r2 <- function(shap, y, intercept, scale = c("r2", "1")) {

  shap <- as.data.frame(shap, drop = FALSE)

  # The default feature scaling is the overall model R2.
  scale <- scale[1]

  # Coerce 1-column data.frames and matrices into vectors.
  y <- as.vector(y)

  # The predicted value for each instance is the sum of all the instance's feature-level Shapley values.
  y_pred <- base::rowSums(shap, na.rm = TRUE) + intercept

  # Calculate the components of the baseline r2.
  y_pred_var <- stats::var(y_pred)
  error_var <- stats::var(y - y_pred)
  r2 <- y_pred_var / (y_pred_var + error_var)  # Gelman et. al. 2018 formula 1.

  # Melt the data.frame into an nrow by n_feature data.frame for calculating instance-feature-level Shapley-modified predictions.
  data <- tidyr::gather(shap, "feature", "shap_effect")

  # Add the outcome to the data.frame. This vector is recycled.
  data$y <- as.vector(y)

  # Add the Shapley-predicted outcome to the data.frame. This vector is recycled.
  data$y_pred <- y_pred

  # Prediction error or, more precisely, residual.
  data$error <- data$y - data$y_pred

  # Eqn. 5.
  y_pred_shap <- data$y_pred - data$shap_effect
  data$y_pred_shap <- y_pred_shap

  # Add the global scalar R2 components to the data.frame.
  data$error_var <- error_var
  data$r2 <- r2

  data_r2 <- data %>%
    dplyr::group_by(.data$feature) %>%  # feature-level R2 calculations.
    dplyr::mutate("error_var_shap" = stats::var(.data$y - .data$y_pred_shap),
                  "error_ratio" = base::min(.data$error_var / .data$error_var_shap, 1, na.rm = TRUE)) %>%
    dplyr::distinct(.data$feature, .keep_all = TRUE)  # with variances calculated, reduce to a feature-level data.frame.

  # Compute the unscaled feature-level Shapley R2 values.
  data_r2$r2_shap <- ((data_r2$r2 - data_r2$error_ratio * data_r2$r2) / (base::sum(data_r2$r2 - data_r2$error_ratio * data_r2$r2)))
  #----------------------------------------------------------------------------
  # Sigma calculation from Eqn. 7.
  data_sigma <- data %>%
    dplyr::group_by(.data$feature) %>%  # feature-level sigma calculations.
    dplyr::summarize("error_var_shap" = stats::var(.data$y - .data$y_pred_shap))

  error_var_shap <- sum(data_sigma$error_var_shap, na.rm = TRUE)

  # SHAP increase in error variance from removing each feature / variance diff from full model to intercept-only model.
  var_ratio <- sum(error_var_shap - error_var, na.rm = TRUE) / (stats::var(y - intercept, na.rm = TRUE) - error_var)
  #----------------------------------------------------------------------------
  # Optional: Scale the feature-level R2 values to the model's global R2 value.
  if (scale == "r2") {
    data_r2$r2_shap <- data_r2$r2_shap * data_r2$r2
  }

  data_r2$sigma_unique <- var_ratio

  data <- as.data.frame(dplyr::select(data_r2, .data$feature, .data$r2, .data$r2_shap, .data$sigma_unique))

  return(data)
}
