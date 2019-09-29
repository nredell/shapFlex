#' Compute feature importances using a Shapley variance decomposition of R^{2}
#'
#' This function uses pre-computed Shapley values to decompose the overall model R^{2} into feature-level
#' attributions of variance explained using the formulation of Redell (2019) arXiv:1908.09718.
#'
#' @param shap A matrix or data.frame of Shapley values. The dimensions should be 'number of instances' by
#' 'number of features'. The predicted outcome for each instance, used in the R^{2} caluclation, is the row
#' sum of Shapley values across columns.
#' @param y A vector or 1-column matrix or data.frame with the outcome being predicted.
#' @param scale The scaling of the feature importances. \code{r2} (default) scales feature-level importances to the
#' overall model R^{2} while \code{1} scales feature importances along a 0 to 1 scale.
#' @return A data.frame of the feature importances with the model's global R^{2} ('r2') and the feature-level
#' importances or attribution of variance explained ('r2_shap').
#' @export
r2 <- function(shap, y, scale = c("r2", "1")) {

  shap <- as.data.frame(shap, drop = FALSE)

  # The default feature scaling is the overall model R2.
  scale <- scale[1]

  # Coerce 1-column data.frames and matrices into vectors.
  y <- as.vector(y)

  # The predicted value for each instance is the sum of all the instance's feature-level Shapley values.
  y_pred <- base::rowSums(shap, na.rm = TRUE)

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

  # Eqn. (3) in [cite]
  data$y_pred_shap <- data$y_pred - data$shap_effect

  # Add the global scalar R2 components to the data.frame.
  data$error_var <- error_var
  data$r2 <- r2

  data <- data %>%
    dplyr::group_by(.data$feature) %>%  # feature-level R2 calculations
    dplyr::mutate("error_var_shap" = stats::var(.data$y - .data$y_pred_shap),
                  "error_ratio" = base::min(.data$error_var / .data$error_var_shap, 1, na.rm = TRUE)) %>%
    dplyr::distinct(.data$feature, .keep_all = TRUE)  # with variances calculated, reduce to a feature-level data.frame.

  # Compute the unscaled feature-level Shapley R2 values.
  data$r2_shap <- ((data$r2 - data$error_ratio * data$r2) / (base::sum(data$r2 - data$error_ratio * data$r2)))

  # Optional: Scale the feature-level R2 values to the model's global R2 value.
  if (scale == "r2") {
    data$r2_shap <- data$r2_shap * data$r2
  }

  data <- as.data.frame(dplyr::select(data, .data$feature, .data$r2, .data$r2_shap))

  return(data)
}
