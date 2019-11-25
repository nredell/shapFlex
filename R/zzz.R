# Internal prediction function. Used at the end of shapFlex::shapFlex().
predict_shapFlex <- function(models, data, data_feature_effects, predict_functions,
                             dataset_weights = NULL, keep_samples) {

  # Set the default weights for combining predictions across datasets to a uniform weighting scheme.
  if (is.null(dataset_weights)) {dataset_weights <- rep(1 / length(models), length(models))}

  datasets_with_a_target_feature <- which(unlist(lapply(data_feature_effects, function(x) {!is.null(x)})))

  # If one input dataset, nest the models in a list for indexing. This is a convenience for the user in the most
  # common problem setting--1 dataset with multiple models--so they don't have to create nested lists for the
  # shapFlex() input.
  if (length(data) == 1) {

    models <- list(models)
  }

  #----------------------------------------------------------------------------
  # Apply predict() function(s) across models and datasets.
  i <- 1
  data_pred_by_dataset <- lapply(datasets_with_a_target_feature, function(i) {

    # Select the dataset.
    data_shap_instances <- data_feature_effects[[i]]  # the Shapley-sampled dataset from shapFlex::shapFlex().

    # Select the Shapley-sampled dataset for the user-defined prediction function(s). The meta-information columns are
    # hardcoded so if the shapFlex::shapFlex function changes to add new meta info, this needs to change or become
    # dynamic.
    data_model <- data_shap_instances[, 1:(ncol(data_shap_instances) - 6), drop = FALSE]
    data_meta <- data_shap_instances[, (ncol(data_shap_instances) - 5):ncol(data_shap_instances), drop = FALSE]

    # length(predict_functions[[i]]) == 1 for each input dataset.
    dataset_predictions <- predict_functions[[i]](models[[i]], as.data.frame(data_model))

    # Return the sampling predictions if requested by the user.
    if (isTRUE(keep_samples)) {

      dataset_predictions <- dplyr::bind_cols(data_model, data_meta, dataset_predictions)

    } else {

      dataset_predictions <- dplyr::bind_cols(data_meta, dataset_predictions)
    }

    dataset_predictions
  })
  #----------------------------------------------------------------------------
  # Returns a length 1 numeric vector of the average prediction--i.e., intercept--per input dataset (not per Shapley-sampled dataset).
  dataset_intercept <- lapply(datasets_with_a_target_feature, function(i) {
    # Predict on the original data to get the baseline prediction--i.e., intercept--against which the Shapley
    # values deviate.
    dataset_intercept <- predict_functions[[i]](models[[i]], data[[i]])
    dataset_intercept <- mean(dataset_intercept[, 1], na.rm = TRUE)  # hardcoded to handle univariate-outcome models.
    dataset_intercept
  })
  #----------------------------------------------------------------------------
  # Take the difference between each target and comparison instance pair to get the prediction delta for
  # a given target feature.
  data_shap_by_dataset <- lapply(seq_along(data_pred_by_dataset), function(i) {

    if (isTRUE(keep_samples)) {
      # Keep only the meta-data for analysis. If keep_samples == FALSE, only the meta-data is available and no
      # action needs to be taken.
      data_pred_by_dataset[[i]] <- data_pred_by_dataset[[i]][, (ncol(data_pred_by_dataset[[i]]) - 6):ncol(data_pred_by_dataset[[i]]), drop = FALSE]
    }

    dataset_number <- as.vector(data_pred_by_dataset[[i]]$dataset[1])

    # The model predictions are the last column in the sampling dataset.
    user_fun_y_pred_name <- names(data_pred_by_dataset[[i]])[ncol(data_pred_by_dataset[[i]])]

    # Cast the dataframe to, for each random sample, take the difference between the column-shuffled row with
    # the fixed target feature and the dynamic target feature. This is the sample-level Shapley value for a given
    # feature and instance.
    data_pred_by_dataset[[i]] <- tidyr::spread(data_pred_by_dataset[[i]], key = .data$feature_group, value = !!user_fun_y_pred_name)

    # Calculate the sample-feature-level Shapley value.
    data_pred_by_dataset[[i]]$shap_effect <- data_pred_by_dataset[[i]]$feature_static - data_pred_by_dataset[[i]]$feature_random

    data_pred_by_dataset[[i]] <- data_pred_by_dataset[[i]] %>%
      dplyr::group_by(.data$explained_instance, .data$feature_name) %>%
      dplyr::mutate("shap_effect_sd" = stats::sd(.data$shap_effect, na.rm = TRUE)) %>%  # sd() not working with summarize().
      dplyr::summarize("dataset" = dataset_number,  # defined outside of the dataframe.
                       "shap_effect_sd" = .data$shap_effect_sd[1],  # Shapley stamdard deviation.
                       "shap_effect" = base::mean(.data$shap_effect, na.rm = TRUE),  # the feature-level Shapley value.
                       "dataset_weight" = dataset_weights[dataset_number],  # from the 'dataset_weights' input vector.
                       "intercept" = dataset_intercept[[i]])

    data_pred_by_dataset[[i]]
  })
  #----------------------------------------------------------------------------
  # Return the sampling predictions at the end of this function if requested by the user.
  if (isTRUE(keep_samples)) {

    data_samples <- lapply(seq_along(data_pred_by_dataset), function(i) {

      data_meta <- data_pred_by_dataset[[i]][, (ncol(data_pred_by_dataset[[i]]) - 6):ncol(data_pred_by_dataset[[i]]), drop = FALSE]

      dataset_number <- as.vector(data_pred_by_dataset[[i]]$dataset[1])

      # The model predictions are the last column in the sampling dataset.
      user_fun_y_pred_name <- names(data_pred_by_dataset[[i]])[ncol(data_pred_by_dataset[[i]])]

      # Cast the dataframe to, for each random sample, take the difference between the column-shuffled row with
      # the fixed target feature and the dynamic target feature. This is the sample-level Shapley value for a given
      # feature and instance.
      data_meta <- tidyr::spread(data_meta, key = .data$feature_group, value = !!user_fun_y_pred_name)

      # Calculate the sample-feature-level Shapley value.
      data_meta$shap_effect <- data_meta$feature_static - data_meta$feature_random
      data_meta <- dplyr::select(data_meta, -feature_static, -feature_random)

      data_features <- dplyr::filter(data_pred_by_dataset[[i]], .data$feature_group == "feature_random")

      data_model <- data_features %>%
        dplyr::group_split(.data$feature_name)

      #x <- data_model[[1]]
      data_model <- lapply(data_model, function(x) {

        x <- as.data.frame(x)
        #x <- x[, c("explained_instance", "dataset", "sample", "shuffle", eval(unique(x$feature_name)))]
        x$feature_name <- eval(unique(x$feature_name))
        #names(x)[ncol(x) - 1] <- "feature_value"
        #x$feature_value <- as.numeric(x$feature_value)
        x$feature_value <- x[, names(x) == unique(x$feature_name)]
        x[, names(x) == unique(x$feature_name)] <- NULL
        x
      })

      data_interaction <- lapply(data_model, function(x) {

        x <- dplyr::left_join(x, data_meta, by = c("explained_instance", "dataset", "sample", "shuffle", "feature_name"))
      })
      })

  }
  #----------------------------------------------------------------------------

  data_shap <- dplyr::bind_rows(data_shap_by_dataset)

  data_shap_pred_intercept <- data_shap %>%
    dplyr::group_by(.data$explained_instance, .data$dataset) %>%  # group to get intercept.
    dplyr::summarize("intercept" = .data$intercept[1],
                     "pred_shap_dataset" = base::sum(.data$shap_effect, na.rm = TRUE),
                     "dataset_weight" = .data$dataset_weight[1]) %>%
    dplyr::group_by(.data$explained_instance) %>%  # group to get one instance-level intercept.
    dplyr::summarize("feature_name" = "intercept",
                     "shap_effect" = stats::weighted.mean(.data$intercept, .data$dataset_weight))  # name columns to match the dataset below for dplyr::bind_rows().

  # Weighting Shapley effects across datasets.
  # To-do: a weighted standard deviation is needed for multiple datasets.
  #"pred_shap_overall" = weighted.mean(intercept, dataset_weight) + weighted.mean(pred_shap_dataset, dataset_weight)
  if (length(data) > 1) {

    # Feature-level Shapley effects across datasets
    data_shap_pred_feature <- data_shap %>%
      dplyr::group_by(.data$explained_instance, .data$feature_name) %>%
      dplyr::summarize("shap_effect" = base::sum(.data$shap_effect * .data$dataset_weight, na.rm = TRUE))

  # Placehodler code.
  #   data_shap_pred_feature_var <- data_shap %>%
  #     dplyr::group_by(explained_instance, feature_name) %>%
  #     dplyr::summarize("shap_effect_var" = var(shap_effect, na.rm = TRUE)) %>%
  #     dplyr::ungroup() %>%
  #     dplyr::select(shap_effect_var)
  #
  #   data_shap_pred_feature <- suppressWarnings(dplyr::bind_cols(data_shap_pred_feature, data_shap_pred_feature_var))

    data_shap <- dplyr::bind_rows(data_shap_pred_intercept, data_shap_pred_feature)

  } else {

    data_shap <- dplyr::bind_rows(data_shap_pred_intercept, data_shap)
  }

  if (isTRUE(keep_samples)) {

    attr(data_shap, "data_samples") <- data_interaction

    return(data_shap)

  } else {

    return(data_shap)
  }
}
