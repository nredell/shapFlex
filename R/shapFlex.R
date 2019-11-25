#' Compute stochastic feature-level Shapley values for ensemble models using potentially different input datasets
#'
#' This function uses user-defined trained models and prediction functions to compute approximate Shapley values for
#' single models or ensemble models. Shapley values can be calculated for a subset of model features which reduces the
#' typically expensive computation of approximate Shapley values in high-dimensional models or model ensembles.
#'
#' @param data A list of 1 or more dataframes that contain the instance/row to be explained.
#' @param explain_instance An integer that identifies the instance/row to be explained. If \code{length(data)} > 1 and the
#' number of rows differ between datasets, a row name that idenftifies the target instance in each dataset that can be coerced
#' to an integer with \code{as.integer(row.names(data))}.
#' @param explain_instance_id For finding the location of \code{explain_instance}. \code{row_index} is the dataset row number (default) or
#' \code{row_name} if multiple datasets are used in an ensemble model.
#' @param models A list containing a vector of 1 or more trained models. For models with 1 input dataset, the length of the
#' list holding the model(s) is 1. For multiple input datasets, \code{models} is a nested list with length equal to length(data).
#' @param predict_functions A list containing the user-specified dataset-specifc prediction function.
#' The length of the list is equal to \code{length(data)}.
#' @param dataset_weights Optional. A sum-to-one numeric vector with \code{length(data)} that weights the feature-level Shapley value combination across datasets.
#' Only needed for ensemble models that predict with different datasets (length(data) > 1). The default value is a uniform weighting scheme.
#' @param target_features Optional. A character vector of feature names in \code{data} for which Shapley values will be produced. For high-dimensional models, selecting a subset
#' of interesting features may dramatically speed up computation time. The default value is to return feature effects for all features across all input datasets.
#' @param sample_size The number of randomly selected comparison rows or instances used to compute the feature-level Shapley value.
#' @param shuffle The number of times the features in each randomly selected comparison are shuffled. This is an explore, exploit parameter.
#' @param use_future Boolean. If \code{TRUE}, the \code{future} package is used to calculate Shapley values in parallel.
#' @param keep_samples = Boolean. If \code{TRUE}, the Monte Carlo sampling data is returned for \code{interaction()}.
#' @return A data.frame of the feature-level Shapley values for the target instance and selected features.
#' @importFrom magrittr %>%
#' @importFrom rlang !!
#' @importFrom rlang .data
#' @export
shapFlex <- function(data, explain_instance = 1, explain_instance_id = c("row_index", "row_name"),
                     models, predict_functions, dataset_weights = NULL, target_features = NULL,
                     sample_size = 60, shuffle = 1, use_future = FALSE, keep_samples = FALSE) {

  # This function uses 1 internal function: predict_shapFlex.

  if (!methods::is(data, "list")) {
    stop("The 'data' argument needs to be a list of 1 or more data.frames.")
  }

  if (is.null(explain_instance) || (!methods::is(as.integer(explain_instance), "integer") &&
                                    length(explain_instance) == 1 && explain_instance >= 1)) {
    stop("The 'explain_instance' argument needs to be a length 1 numeric vector >= 1 in value or coercible to one. 'explain_instance'
         identifies the row number or as.integer(row.names(data)) for which Shapley values are calculated.")
  }

  # Set the default 'explain_instance' to a row index (i.e., row number) which assumes that all input datasets are the same number of rows.
  explain_instance_id <- explain_instance_id[1]

  if (!explain_instance_id %in% c("row_index", "row_name")) {
    stop("The 'explain_instance_id' argument needs to be one of 'row_index' or 'row_name'.")
  }

  if (is.null(sample_size) || sample_size < 1) {
    stop("Set the 'sample_size' argument to be > 0.")
  }

  # If not specified, use all unique features across all input datasets to explain the given instance's predictions.
  if (is.null(target_features)) {
    target_features <- unique(unlist(lapply(data, names)))
  }

  # Filter out datasets that do not contain any 'target_features', potentially reducing the number of outer-most loops.
  datasets_with_a_target_feature <- which(sapply(data, function(x) {any(target_features %in% names(x))}))

  if (length(datasets_with_a_target_feature) == 0) {
    stop("None of the 'target_features' appear as names in the input list of dataframes.")
  }

  if (isTRUE(use_future)) {

    lapply_function <- future.apply::future_lapply

  } else {

    lapply_function <- base::lapply
  }

  # Initialize an empty set of lists to match the number of input datasets; some may be NULL if they do not
  # contain a target_feature, but this approach of keeping the NULL list slots makes the indexing clearer.
  ensemble_feature_effects <- vector("list", length(data))

  h <- 1
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

      n_rows <- nrow(data[[h]])

      i <- 1
      data_feature_effects_by_sample <- lapply_function(1:sample_size, function(i) {

        # Select one row, or row index, from the input dataset, excluding the target index.
        row_index_random <- sample((1:n_rows)[-explain_instance_index], size = 1, replace = FALSE)

        data_feature_effects_shuffle <- lapply(1:shuffle, function(pass) {

          # Shuffle the column indices, keeping all column indices.
          feature_indices_random <- sample(1:n_features, size = n_features, replace = FALSE)

          # Shuffle the column order for the randomly selected row. Adjust sampling methodology to work
          # with time-series data.
          random_instance <- data[[h]][row_index_random, feature_indices_random, drop = FALSE]

          # For the target row that is selected, shuffle the columns to match our randomly selected and shuffled instance.
          target_instance <- explain_instance_x[, feature_indices_random, drop = FALSE]

          # Each randomly sampled and shuffled comparison instance will isolate the effect of each target_feature:
          # This lapply() reutrns a 1 sample * n_target_features list.
          j <- 1
          data_feature_effects_sample_i <- lapply(1:n_target_features, function(j) {

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

            # Re-order the datasets to match the data input for the user-defined predict() function.
            # We'll also add a group identifier so that we can calculate the marginal effect of each
            # predictor with a group_by() function.
            target_instance_with_feature <- target_instance_with_feature[, order(feature_indices_random), drop = FALSE]
            target_instance_without_feature <- target_instance_without_feature[, order(feature_indices_random), drop = FALSE]

            data_target_instance <- dplyr::bind_rows(list(target_instance_with_feature, target_instance_without_feature))

            # Save the output data.frame to our list.
            data_target_instance$sample <- i

            # target_cols <- which(names(data[[h]]) %in% names(data[[h]])[feature_indices_random[1:feature_index]])
            # comparison_cols <- (1:n_features)[!1:n_features %in% target_cols]
            # data_target_instance$target_cols <- list(target_cols)
            # if (length(comparison_cols) == 0) {
            #
            #   data_target_instance$comparison_cols <- list(0)
            #
            # } else {
            #
            #   data_target_instance$comparison_cols <- list(comparison_cols)
            # }

            data_target_instance
          })  # End 'j' loop over model/target features.

          data_out <- dplyr::bind_rows(data_feature_effects_sample_i)

          data_out$shuffle <- pass

          data_out
        })  # End 'pass' loop of multiple shuffles between the target instance and the same random instance.

        dplyr::bind_rows(data_feature_effects_shuffle)
      })  # End 'i' loop over randomly sampled instances.

      # Collapse the sample-level dataset to get a dataset with n_samples * n_features * 2 rows.
      ensemble_feature_effects[[h]] <- dplyr::bind_rows(data_feature_effects_by_sample)

      # 'feature_static' represents the column-shuffled target instance with the unchanged feature of interest; 'feature_dynamic'
      # represents the column-shuffled comparison instance that only differs with respect to the feature of interest.
      ensemble_feature_effects[[h]]$feature_group <- rep(c('feature_static', 'feature_random'), n_target_features * sample_size * shuffle)
      ensemble_feature_effects[[h]]$feature_name <- rep(rep(target_features_in_dataset, each = 2), sample_size * shuffle)

      ensemble_feature_effects[[h]]$dataset <- h
      ensemble_feature_effects[[h]]$explained_instance <- explain_instance  # function argument: row_index or row.name

    } else {

      ensemble_feature_effects[[h]] <- NULL  # The instance to explain was not in the dataset.
    }
  }  # End 'h' loop over datasets if multiple datasets are provided.

  if (all(unlist(lapply(ensemble_feature_effects, is.null)))) {
    stop("No Shapley values were returned; the likely cause is that 'explain_instance' is not in any of the input datasets.")
  }

  #----------------------------------------------------------------------------
  # shapFlex:::predict_shapFlex.
  data_out <- predict_shapFlex(models = models, data = data, data_feature_effects = ensemble_feature_effects,
                               predict_functions = predict_functions, dataset_weights = dataset_weights, keep_samples = keep_samples)

  # If keep_samples is TRUE, those will be return()ed as an attribute.
  if (isTRUE(keep_samples)) {

    data_samples <- attributes(data_out)$data_samples
    attr(data_out, "data_samples") <- NULL
  }

  #----------------------------------------------------------------------------
  # Add the feature values if only one input dataset is present. To-do: expand this to multiple datasets with
  # the potential of the same feature name having different values across datasets.
  if (length(data) == 1) {

    data_merge <- data[[1]]

    if (explain_instance_id == "row_index") {

      data_merge$explained_instance <- 1:nrow(data_merge)

    } else {

      data_merge$explained_instance <- as.numeric(row.names(data_merge))
    }

    # Melt the data.frame for merging. Suppress the warning of factors and numeric features being
    # combined into one 'feature_value' column and coerced to characters.
    data_merge <- suppressWarnings(tidyr::gather(data_merge, key = "feature_name", value = "feature_value", -.data$explained_instance))

    data_out <- dplyr::left_join(data_out, data_merge, by = c("explained_instance", "feature_name"))

    # Re-order columns.
    data_out <- as.data.frame(dplyr::select(data_out, .data$explained_instance, .data$feature_name,
                                            .data$feature_value, .data$shap_effect, .data$shap_effect_sd))
  }

  if (isTRUE(keep_samples)) {

    attr(data_out, "data_samples") <- data_samples

    return(data_out)

  } else {

    return(data_out)
  }
}
