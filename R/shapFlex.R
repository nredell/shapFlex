
#' Compute stochastic feature-level Shapley values for ensemble models using potentially different input datasets
#'
#' This function uses user-defined trained models and prediction functions to compute approximate Shapley values for
#' single or ensemble models. Shapley values can be calculated for a subset of model features which reduces the
#' typically expensive computation of approximate Shapley values in high-dimensional models or model ensembles.
#'
#' @param data A list of 1 or more dataframes that contain the instance/row to be explained.
#' @param explain_instance An integer that identifies the instance/row to be explained. If length(data) > 1 and the
#' number of rows differ between datasets, a row name that idenftifies the target instance in each dataset that can be coerced
#' to an integer with as.integer(row.names(data)).
#' @param explain_instance_id For finding the location of 'explain_instance'. 'row_index' is the dataset row number (default) or 'row_name' if
#' multiple datasets are used in an ensemble model.
#' @param models A list containing a vector of 1 or more trained models. For models with 1 input dataset, the length of the
#' list holding the model(s) is 1. For multiple input datasets, 'models' is a nested list with length equal to length(data).
#' @param predict_functions A list containing a length-1 characeter vector identifying the dataset-specifc prediction function name.
#' The length of the list is equal to length(data).
#' @param dataset_weights Optional. A sum-to-one vector with length(data) that weights the feature-level Shapley value combination across datasets.
#' Only needed for ensemble models that predict with different datasets (length(data) > 1). The default value is a uniform weighting scheme.
#' @param target_features Optional. The feature names in 'data' for which feature effects will be produced. For high-dimensional models, selecting a subset
#' of interesting features may dramatically speed up computation time. The default value is to return feature effects for all features across all input datasets.
#' @param sample_size The number of randomly selected rows with randomly shuffled features used to compute the feature-level Shapley value.
#' @param n_cores The number of cores used in the Shapley value calculations (>= 1). Limited to 1 core on Windows OS.
#' @return A data.frame of the feature-level Shapley values for the target instance and selected features.
#' @importFrom magrittr %>%
#' @export
shapFlex <- function(data, explain_instance = 1, explain_instance_id = c("row_index", "row_name"),
                     models, predict_functions, dataset_weights = NULL, target_features = NULL,
                     sample_size = 60, n_cores = 1) {

  # This function uses 1 internal function: shapFlex::predict_shapFlex.

  if (!methods::is(data, "list")) {
    stop("The 'data' argument needs to be a list of 1 or more dataframes.")
  }

  if (is.null(explain_instance) || (!methods::is(as.integer(explain_instance), "integer") &&
                                    length(explain_instance) == 1 && explain_instance >= 1)) {
    stop("The 'explain_instance' argument needs to be a length 1 numeric vector >= 1 in value or coercible to one. 'explain_instance'
         identifies the row number or as.integer(row.names(data)) for which Shapley values are calculated.")
  }

  # Set the default 'explain_instance' to arow index (i.e., row number) which assumes that all input datasets are the same number of rows.
  explain_instance_id <- if (length(explain_instance_id) > 1) {explain_instance_id[1]} else {explain_instance_id}

  if (!explain_instance_id %in% c("row_index", "row_name")) {
    stop("The 'explain_instance_id' argument needs to be one of 'row_index' or 'row_name'.")
  }

  if (is.null(sample_size) || sample_size < 1) {
    stop("Set the 'sample_size' argument to be > 0.")
  }

  # If not specified, use all unique features across all input datasets to explain the given instance's predictions.
  # To-do: create simulations to see how well this function scales with increased features.
  if (is.null(target_features)) {
    target_features <- unique(unlist(lapply(data, names)))
  }

  # Filter out datasets that do not contain any 'target_features', potentially reducing the number of outer-most loops.
  datasets_with_a_target_feature <- which(sapply(data, function(x) {any(target_features %in% names(x))}))

  if (length(datasets_with_a_target_feature) == 0) {
    stop("None of the 'target_features' appear as names in the input list of dataframes.")
  }

  # Initialize an empty set of lists to match the number of input datasets; some may be NULL if they do not
  # contain a target_feature, but this approach of keeping the NULL list slots makes the indexing clearer.
  ensemble_feature_effects <- vector("list", length(data))

  for (h in datasets_with_a_target_feature) {

    # If 'explain_instance' is a row name, convert it to an index. This will allow us to compare the same instance
    # across datasets with a different number of rows--think of datasets with lagged features having different nrow(data).
    if (explain_instance_id == "row_name") {
      explain_instance_index <- which(as.integer(row.names(data[[h]])) == explain_instance)  # convert row name to a row number.
    } else {
      explain_instance_index <- explain_instance  # row number in the dataset.
    }

    # If neither the converted 'row_name' to a row index for 'explain_instance' is in the dataset nor
    # is the 'row_index' less than nrow(data), skip the Shapley sampling and return NULL.
    if (length(explain_instance_index) != 0 && explain_instance_index <= nrow(data[[h]])) {

      # Select the one row that we will explain using the 'target_features' of interest.
      explain_instance_x <- data[[h]][explain_instance_index, , drop = FALSE]

      # Total features in dataset; this can change across 'data' if length(data) > 1.
      n_features <- ncol(data[[h]])

      # Keep the names of the 'target_features' for column indexing between the input dataset column order and
      # the shuffled column order.
      target_features_in_dataset <- names(data[[h]])[names(data[[h]]) %in% target_features]

      # How many of the target features are in this dataset; sets the duration of the inner-most apply function.
      n_target_features <- length(target_features_in_dataset)

      # How many rows is this dataset.
      n_rows <- nrow(data[[h]])

      data_feature_effects_by_sample <- parallel::mclapply(1:sample_size, function(i) {

        # Shuffle the column indices, keeping all column indices.
        feature_indices_random <- sample(1:n_features, size = n_features, replace = FALSE)

        # Select one row, or row index, from the input dataset.
        row_index_random <- sample(1:n_rows, size = 1, replace = FALSE)

        # Shuffle the column order for the randomly selected row. Adjust sampling methodology to work
        # with time-series data.
        random_instance <- data[[h]][row_index_random, feature_indices_random, drop = FALSE]

        # For the target row that is selected, shuffle the columns to match our randomly selected and shuffled instance.
        target_instance <- explain_instance_x[, feature_indices_random, drop = FALSE]

        # Each randomly sampled and shuffled comparison instance will isolate the effect of each target_feature:
        # This lapply() reutrns a 1 sample * n_target_features list.
        data_feature_effects_sample_n <- lapply(1:n_target_features, function(j) {

          # For each feature in the loop, find the position or index of the shuffled column.
          feature_index <- which(names(data[[h]])[feature_indices_random] == target_features_in_dataset[j])

          # This is our target instance, shuffled.
          target_instance_with_feature <- target_instance

          # If the shuffled feature is not in the last column, replace columns (feature_index + 1):(n_features)
          # in the target instance with the same columns from our randomly selected instance.
          if (feature_index <= (n_features - 1)) {
            target_instance_with_feature[, (feature_index + 1):(n_features)] <- random_instance[, (feature_index + 1):(n_features), drop = FALSE]
          }

          # Now, we'll take our target instance with the (feature_index + 1):(n_features) shuffled columns
          # and replace our current target feature in this loop to get a comparison instance that only
          # differs with respect to the current target feature.
          target_instance_without_feature <- target_instance_with_feature
          target_instance_without_feature[, feature_index] <- random_instance[, feature_index]

          # Re-order the datasets to match the data input. We'll also add a group identifier so that we can
          # calculate the marginal effect of each predictor with a group_by() function.
          target_instance_with_feature <- target_instance_with_feature[, order(feature_indices_random), drop = FALSE]
          target_instance_without_feature <- target_instance_without_feature[, order(feature_indices_random), drop = FALSE]

          data_target_instance <- data.table::rbindlist(list(target_instance_with_feature, target_instance_without_feature))

          # Save the output dataframe to our list.
          data_target_instance$sample <- i
          data_target_instance
        })

        # Return explicitly from the inner lapply()
        data_feature_effects_sample_n
      }, mc.cores = n_cores)

      # Collapse the sample-level dataset to get a dataset with n_samples * n_features * 2 rows.
      ensemble_feature_effects[[h]] <- data.table::rbindlist(unlist(data_feature_effects_by_sample, recursive = FALSE))

      # 'feature_static' represents the column-shuffled target instance with the unchanged feature of interest; 'feature_dynamic'
      # represents the column-shuffled comparison instance that only differs with respect to the feature of interest.
      ensemble_feature_effects[[h]]$feature_group <- rep(c('feature_static', 'feature_dynamic'), n_target_features * sample_size)
      ensemble_feature_effects[[h]]$feature_name <- rep(rep(target_features_in_dataset, each = 2), sample_size)

      ensemble_feature_effects[[h]]$dataset <- h
      ensemble_feature_effects[[h]]$explained_instance <- explain_instance  # function argument: row_index or row.name
    } else {

      ensemble_feature_effects[[h]] <- NULL  # The instance to explain was not in the dataset.
    }
  }  # end h loop.

  if (all(unlist(lapply(ensemble_feature_effects, is.null)))) {
    stop("No Shapley values were returned; the likely cause is that 'explain_instance' is not in any of the input datasets.")
  }

  # shapFlex::predict_shapFlex.
  data_out <- predict_shapFlex(models, data, data_feature_effects = ensemble_feature_effects, predict_functions,
                               dataset_weights = dataset_weights)

  # Add the feature values if only one input dataset is present. To-do: expand this to multiple datasets with
  # the potential of the same feature name having different values across datasets.
  if (length(data) == 1) {

    data_merge <- data[[1]]

    if (explain_instance_id == "row_index") {
      data_merge$explained_instance <- 1:nrow(data_merge)
    } else {
      data_merge$explained_instance <- as.numeric(row.names(data_merge))
    }

    # Suppress the warning of factors and numeric features being combined into one 'feature_value' column and coerced to characters.
    data_merge <- suppressWarnings(tidyr::gather(data_merge, key = "feature_name", value = "feature_value", -explained_instance))

    data_out <- dplyr::left_join(data_out, data_merge, by = c("explained_instance", "feature_name"))

    # Re-order columns.
    data_out <- as.data.frame(dplyr::select(data_out, explained_instance, feature_name, feature_value, shap_effect, shap_effect_sd))
  }

  return(data_out)
}
