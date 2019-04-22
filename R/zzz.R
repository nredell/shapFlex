
# Internal prediction function. Used at the end of shapFlex::shapFlex().
predict_shapFlex <- function(models, data, data_feature_effects, predict_functions,
                             dataset_weights = NULL, ...) {

  # Set the default weights for combining predictions across datasets to a uniform weighting scheme.
  if (is.null(dataset_weights)) {dataset_weights <- rep(1 / length(models), length(models))}

  datasets_with_a_target_feature <- which(unlist(lapply(data_feature_effects, function(x) {!is.null(x)})))

  # If one input dataset, nest the models in a list for indexing. This is a convenience for the user in the most
  # common problem setting--1 dataset with multiple models--so they don't have to create nested lists for the
  # shapFlex() input.
  if (length(data) == 1) {

    models <- list(models)
  }

  # Apply across models and datasets.
  data_pred_by_dataset <- lapply(datasets_with_a_target_feature, function(i) {

    # Select the dataset.
    data_shap_instances <- data_feature_effects[[i]]  # the Shapley-sampled dataset from shapFlex::shapFlex.

    # Select the Shapley-sampled dataset for the user-defined prediction function(s). The meta-information columns are
    # hardcoded so if the shapFlex::shapFlex function changes to add new meta info, this needs to change or become
    # dynamic.
    data_model <- data_shap_instances[, 1:(ncol(data_shap_instances) - 5), drop = FALSE]
    data_meta <- data_shap_instances[, (ncol(data_shap_instances) - 4):ncol(data_shap_instances), drop = FALSE]

    # length(predict_functions[[i]]) == 1 for each input dataset.
    dataset_predictions <- (eval(parse(text = predict_functions[[i]])))(models[[i]], data_model)

    dataset_predictions <- dplyr::bind_cols(data_meta, dataset_predictions)
    dataset_predictions
  })

  # Returns a length 1 numeric vector of the average prediction--i.e., intercept--per input dataset (not per Shapley-sampled dataset).
  dataset_intercept <- lapply(datasets_with_a_target_feature, function(i) {
    # Predict on the original data to get the baseline prediction--i.e., intercept--against which the Shapley
    # values deviate.
    dataset_intercept <- (eval(parse(text = predict_functions[[i]])))(models[[i]], data[[i]])
    dataset_intercept <- mean(dataset_intercept[, 1], na.rm = TRUE)  # hardcoded to handle univariate-outcome models.
    dataset_intercept
  })

  data_shap_by_dataset <- lapply(seq_along(data_pred_by_dataset), function(i) {

    x <- data_pred_by_dataset[[i]]

    dataset_number <- as.vector(x$dataset[1])

    user_fun_y_pred_name <- names(x)[ncol(x)]

    # Cast the dataframe to, for each random sample, take the difference between the column-shuffled row with
    # the fixed target feature and the dynamic target feature. This is the sample-level Shapley value for a given
    # feature and instance.
    x <- data.table::dcast(x, dataset + explained_instance + feature_name + sample ~ feature_group,
                           value.var = user_fun_y_pred_name)

    # Calculate the sample-feature-level Shapley value.
    x$shap_effect <- x$feature_static - x$feature_dynamic

    x <- x %>%
      dplyr::group_by(explained_instance, feature_name) %>%
      dplyr::mutate("shap_effect_sd" = sd(shap_effect, na.rm = TRUE)) %>%  # sd() not working with summarize().
      dplyr::summarize("dataset" = dataset_number,  # defined outside of the dataframe.
                       "shap_effect_sd" = shap_effect_sd[1],  # Shapley stamdard deviation.
                       "shap_effect" = mean(shap_effect, na.rm = TRUE),  # the feature-level Shapley value.
                       "dataset_weight" = dataset_weights[dataset_number],  # from the 'dataset_weights' input vector.
                       "intercept" = dataset_intercept[[i]])

    x
  })

  # Combine feature effects across datasets. If only 1 input dataset, this is the function output.
  data_shap <- as.data.frame(dplyr::bind_rows(data_shap_by_dataset))

  # Weighting Shapley effects across datasets.
  # To-do: a weighted standard deviation is needed for multiple datasets.
  #"pred_shap_overall" = weighted.mean(intercept, dataset_weight) + weighted.mean(pred_shap_dataset, dataset_weight)
  if (length(data) > 1) {

    data_shap_pred_intercept <- data_shap %>%
      dplyr::group_by(explained_instance, dataset) %>%  # group to get intercept.
      dplyr::summarize("intercept" = intercept[1],
                       "pred_shap_dataset" = sum(shap_effect, na.rm= TRUE),
                       "dataset_weight" = dataset_weight[1]) %>%
      dplyr::group_by(explained_instance) %>%  # group to get one instance-level intercept.
      dplyr::summarize("feature_name" = "intercept",
                       "shap_effect" = weighted.mean(intercept, dataset_weight))  # name columns to match the dataset below for dplyr::bind_rows().

    # Feature-level Shapley effects across datasets
    data_shap_pred_feature <- data_shap %>%
      dplyr::group_by(explained_instance, feature_name) %>%
      dplyr::summarize("shap_effect" = sum(shap_effect * dataset_weight, na.rm = TRUE))

  # Placehodler code.
  #   data_shap_pred_feature_var <- data_shap %>%
  #     dplyr::group_by(explained_instance, feature_name) %>%
  #     dplyr::summarize("shap_effect_var" = var(shap_effect, na.rm = TRUE)) %>%
  #     dplyr::ungroup() %>%
  #     dplyr::select(shap_effect_var)
  #
  #   data_shap_pred_feature <- suppressWarnings(dplyr::bind_cols(data_shap_pred_feature, data_shap_pred_feature_var))

    data_shap <- as.data.frame(dplyr::bind_rows(data_shap_pred_intercept, data_shap_pred_feature))
  }

  return(data_shap)
}
