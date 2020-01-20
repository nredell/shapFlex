
# Internal prediction function. Used at the end of shapFlex::shapFlex().
# Arguments are matched by position.
predict_shapFlex <- function(reference, data_predict, model, predict_function,
                             n_features, causal, causal_weights) {

  data_model <- data_predict[, 1:n_features, drop = FALSE]
  data_meta <- data_predict[, (n_features + 1):ncol(data_predict), drop = FALSE]

  # User-defined predict() function
  data_predicted <- predict_function(model, data_model)

  data_predicted <- dplyr::bind_cols(data_meta, data_predicted)

  # Returns a length 1 numeric vector of the average prediction--i.e., intercept--from the reference group.
  intercept <- mean(predict_function(model, reference)[, 1], na.rm = TRUE)
  #--------------------------------------------------------------------------
  # Cast the data.frame to, for each random sample, take the difference between the Frankenstein
  # instances which are now in six columns: two for symmetric and four for asymmetric.
  user_fun_y_pred_name <- names(data_predicted)[ncol(data_predicted)]

  data_predicted <- tidyr::spread(data_predicted, key = .data$feature_group, value = !!user_fun_y_pred_name)

  data_non_causal <- dplyr::filter(data_predicted, causal == 0)  # Symmetric Shapley.

  # Shapley value for each Monte Carlo sample for each instance.
  data_non_causal$shap_effect <- data_non_causal$real_target - data_non_causal$fake_target
  #--------------------------------------------------------------------------
  data_causal <- dplyr::filter(data_predicted, causal == 1)  # Asymmetric Shapley.

  if (!is.null(causal)) {

    # Eqn. 16 from https://arxiv.org/pdf/1910.06358.pdf.

    data_target_is_a_cause <- dplyr::filter(data_causal, .data$causal_type == "target_is_a_cause")
    # The effect data.frame will be empty when (a) no causal targets appear as causal effects and (b) there
    # are no causal effects in 'target_features'.
    data_target_is_an_effect <- dplyr::filter(data_causal, .data$causal_type == "target_is_an_effect")

    # Top left.
    data_target_is_a_cause$shap_u_1_12 <- with(data_target_is_a_cause, real_causes_fake_effects_real_target - real_causes_fake_effects_fake_target)
    # Top right.
    data_target_is_a_cause$shap_u_1_21 <- with(data_target_is_a_cause, fake_causes_real_effects_real_target_cause - fake_causes_real_effects_fake_target_cause)
    # Bottom left.
    data_target_is_an_effect$shap_u_2_12 <- with(data_target_is_an_effect, real_causes_fake_effects_real_target_effect - real_causes_fake_effects_fake_target_effect)
    # Bottom right.
    data_target_is_an_effect$shap_u_2_21 <- with(data_target_is_an_effect, fake_causes_real_effects_real_target - fake_causes_real_effects_fake_target)

    data_weights <- cbind(causal, causal_weights)
    names(data_weights) <- c("target_is_a_cause", "target_is_an_effect", "weight")

    data_weights <- tidyr::pivot_longer(data_weights, cols = c("target_is_a_cause", "target_is_an_effect"))
    names(data_weights)[2:3] <- c("causal_type", "feature_name")
    data_weights[, 2:3] <- lapply(data_weights[, 2:3], as.character)

    data_weights <- data_weights %>%
      dplyr::group_by(.data$causal_type, .data$feature_name) %>%
      dplyr::summarize(weight = mean(.data$weight))

    data_target_is_a_cause <- dplyr::left_join(data_target_is_a_cause, data_weights, by = c("feature_name", "causal_type"))
    data_target_is_an_effect <- dplyr::left_join(data_target_is_an_effect, data_weights, by = c("feature_name", "causal_type"))
    #--------------------------------------------------------------------------
    # Eqn. 17 from https://arxiv.org/pdf/1910.06358.pdf.

    # Computing the weighted averages by row because of changing weights within a column.
    shap_u_1 <- unlist(lapply(1:nrow(data_target_is_a_cause), function(i) {
      stats::weighted.mean(unlist(data_target_is_a_cause[i, c("shap_u_1_12", "shap_u_1_21")]), c(data_target_is_a_cause$weight[i], 1 - data_target_is_a_cause$weight[i]), na.rm = TRUE)
    }))

    # Causal effects where Shapley values have been requested.
    if (nrow(data_target_is_an_effect) > 0) {

      shap_u_2 <- unlist(lapply(1:nrow(data_target_is_an_effect), function(i) {
        stats::weighted.mean(unlist(data_target_is_an_effect[i, c("shap_u_2_12", "shap_u_2_21")]), c(data_target_is_an_effect$weight[i], 1 - data_target_is_an_effect$weight[i]), na.rm = TRUE)
      }))
    }
    #--------------------------------------------------------------------------
    # Shapley value for each Monte Carlo sample for each instance.
    data_target_is_a_cause$shap_effect <- shap_u_1

    if (nrow(data_target_is_an_effect) > 0) {
      data_target_is_an_effect$shap_effect <- shap_u_2
    }

    data_causal <- dplyr::bind_rows(data_target_is_a_cause, data_target_is_an_effect)

    data_causal <- data_causal %>%
      dplyr::group_by(.data$index, .data$sample, .data$feature_name) %>%
      dplyr::summarize("shap_effect" = mean(.data$shap_effect, na.rm = TRUE))

  }  # End asymmetric causal Shapley value calculations per Monte Carlo sample.
  #--------------------------------------------------------------------------

  data_predicted <- dplyr::bind_rows(data_non_causal, data_causal)

  data_predicted <- dplyr::select(data_predicted, .data$index, .data$sample, .data$feature_name, .data$shap_effect)

  # Final Shapley value calculation collapsed across Monte Carlo samples.
  data_predicted <- data_predicted %>%
    dplyr::group_by(.data$index, .data$feature_name) %>%  # Average across samples.
    dplyr::summarize("shap_effect_sd" = try(stats::sd(.data$shap_effect, na.rm = TRUE)),  # try() in case of 0 sd.
                     "shap_effect" = base::mean(.data$shap_effect, na.rm = TRUE))  # The feature-level Shapley value.

  data_predicted$intercept <- intercept

  return(data_predicted)
}
