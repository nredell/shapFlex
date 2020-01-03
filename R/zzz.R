
# Internal prediction function. Used at the end of shapFlex::shapFlex().
# Arguments are matched by position.
predict_shapFlex <- function(reference, data_predict, model, predict_function,
                             n_features, causal, causal_weights, causal_target) {

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
  data_non_causal$shap_effect <- data_non_causal$feature_static - data_non_causal$feature_random
  #--------------------------------------------------------------------------
  data_causal <- dplyr::filter(data_predicted, causal == 1)  # Asymmetric Shapley.

  if (!is.null(causal)) {

    # Eqn. 16 from https://arxiv.org/pdf/1910.06358.pdf.

    # shap_u_1_12
    shap_u_1_12 <- data_causal$feature_random_real_effects - data_causal$feature_random_fake_effects

    # shap_u_1_21
    shap_u_1_21 <- data_causal$feature_static_real_effects - data_causal$feature_static_fake_effects

    # shap_u_2_12
    shap_u_2_12 <- data_causal$feature_static_real_effects - data_causal$feature_random_real_effects

    # shap_u_2_21
    shap_u_2_21 <- data_causal$feature_static_fake_effects - data_causal$feature_random_fake_effects

    # weight_12 = 1 represents conditioning on the real effect when estimating the Shapley value
    # for the causal target i.e., shap_u_2_12. weight_12 = .5 is, in the limit, equivalentto the
    # symmetric Shapley value. However, eve with a weight of .5, the symmetric and asymmetric Shapley
    # values for the same target feature will likely differ because, in the symmetric case, the causal
    # effects used in the asymmetric equivalent will appear to the left (real) and right (fake) of
    # the target feature pivot in the Frankenstein instance stochastically which only approaches
    # 50/50 or .5 in the limit of Monte Carlo samples.
    weight_12 <- causal_weights
    weight_21 <- 1 - causal_weights

    data_shap_asym <- data.frame(shap_u_1_12, shap_u_1_21, shap_u_2_12, shap_u_2_21)

    # Joining a dictionary of causal weights for the asymmetric Shapley value calculations to
    # the combination of the four Frankenstein instances into two because the data have been sorted
    # along the way and this approach is more robust for computing weighted averages of the Shapley values.
    data_shap_asym$feature_name <- data_causal$feature_name

    data_shap_asym <- dplyr::left_join(data_shap_asym,
                                       data.frame("feature_name" = causal_target, weight_12, weight_21, stringsAsFactors = FALSE),
                                       by = "feature_name")

    # Eqn. 17 from https://arxiv.org/pdf/1910.06358.pdf.

    # Computing the weighted averages by row because of changing weights within a column.
    shap_u_1 <- unlist(lapply(1:nrow(data_shap_asym), function(i) {
      stats::weighted.mean(data_shap_asym[i, c("shap_u_1_12", "shap_u_1_21")], c(data_shap_asym$weight_12[i], data_shap_asym$weight_21[i]), na.rm = TRUE)
    }))

    shap_u_2 <- unlist(lapply(1:nrow(data_shap_asym), function(i) {
      stats::weighted.mean(data_shap_asym[i, c("shap_u_2_12", "shap_u_2_21")], c(data_shap_asym$weight_12[i], data_shap_asym$weight_21[i]), na.rm = TRUE)
    }))

    #data_shap_asym$shap_effect <- shap_u_2
    # Shapley value for each Monte Carlo sample for each instance.
    data_causal$shap_effect <- shap_u_2
  }  # End asymmetric causal Shapley value calculations per Monte Carlo sample.
  #--------------------------------------------------------------------------

  data_predicted <- dplyr::bind_rows(data_non_causal, data_causal)

  data_predicted <- dplyr::select(data_predicted, .data$index, .data$sample, .data$feature_name, .data$causal, .data$shap_effect)

  # Final Shapley value calculation collapsed across Monte Carlo samples.
  data_predicted <- data_predicted %>%
    dplyr::group_by(.data$index, .data$feature_name) %>%  # Average across samples.
    dplyr::summarize("shap_effect_sd" = try(stats::sd(.data$shap_effect, na.rm = TRUE)),  # try() in case of 0 sd.
                     "shap_effect" = base::mean(.data$shap_effect, na.rm = TRUE))  # The feature-level Shapley value.

  data_predicted$intercept <- intercept

  return(data_predicted)
}
