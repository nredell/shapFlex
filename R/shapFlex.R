#' Compute symmetric or asymmetric stochastic feature-level Shapley values
#'
#' This function uses user-defined trained models and prediction functions to compute approximate Shapley values for
#' single models. Shapley values can be calculated for a subset of model features which reduces the
#' typically expensive computation of approximate Shapley values in high-dimensional models.
#'
#' @param explain TBD
#' @param reference TBD
#' @param model TBD
#' @param predict_functions TBD
#' @param target_features Optional. A character vector of feature names in \code{data} for which Shapley values will be produced. For high-dimensional models, selecting a subset
#' of interesting features may dramatically speed up computation time. The default value is to return feature effects for all features across all input datasets.
#' @param causal TBD
#' @param causal_weights TBD
#' @param sample_size The number of randomly selected comparison rows or instances used to compute the feature-level Shapley value.
#' @param use_future Boolean. If \code{TRUE}, the \code{future} package is used to calculate Shapley values in parallel.
#' @return A data.frame of the feature-level Shapley values for the target instance and selected features.
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @export
shapFlex <- function(explain, reference = NULL, model, predict_function, target_features = NULL,
                     causal = NULL, causal_weights = NULL, sample_size = 60, use_future = FALSE) {

  if (missing(explain) || !methods::is(explain, "data.frame")) {
    stop("Enter a data.frame of 1 or more instances to explain.")
  }

  if (!is.null(reference) && !methods::is(reference, "data.frame")) {
    stop("Enter a data.frame of 1 or more instances as a reference group against which 'explain' will be compared.")
  }

  if (missing(model)) {
    stop("Enter a prediction model with the 'model' argument.")
  }

  if (missing(predict_function)) {
    stop("Enter a prediction function that works with the model from the 'model' argument.")
  }

  if (!is.null(target_features) && any(!target_features %in% names(explain))) {
    stop("All target features should be in names(explain).")
  }

  if (!is.null(causal) && !methods::is(causal, "list")) {
    stop("Enter the 'causal' specifications as a list of formulas.")
  }

  if (!is.null(causal) && is.null(causal_weights)) {
    causal_weights <- rep(1, length(causal))  # Default is to estimate a pure "causal" effect.
  }

  if (!is.null(causal) && length(causal) != length(causal_weights)) {
    stop("Enter a length(causal) numeric vector of weights between 0 and 1; '1' estimates a pure 'causal' effect.")
  }

  if (is.null(target_features)) {

    target_features <- names(explain)  # Default is to explain with all features.
  }
  #----------------------------------------------------------------------------
  if (isTRUE(use_future)) {

    lapply_function <- future.apply::future_lapply

  } else {

    lapply_function <- base::lapply
  }
  #----------------------------------------------------------------------------
  n_features <- ncol(explain)

  n_target_features <- length(target_features)
  #----------------------------------------------------------------------------
  if (is.null(reference)) {  # Default is to explain all instances in 'explain'.

    reference <- explain
    n_instances <- nrow(reference)

  } else {

    n_instances <- nrow(reference)
  }
  #----------------------------------------------------------------------------
  if (!is.null(causal)) {

    causal_formula <- lapply(causal, function(x){attributes(terms(x))$variables})
    causal_formula <- lapply(causal_formula, function(x){as.character(x)[-1]})

    causal_target_all <- unlist(lapply(causal_formula, function(x){x[1]}))
    causal_effects_all <- lapply(causal_formula, function(x){x[-1]})

    # shapFlex currently only supports unduplicated endogenous targets.
    if (any(duplicated(unlist(causal_target_all)))) {
      stop("Remove formulas from 'causal' with duplicate endogenous/outcome features e.g., list(y ~ x1, y ~ x2).")
    }

    if (any(!unlist(causal_target_all) %in% target_features)) {
      stop("One or more of the endogenous/outcome features from 'causal' is not in 'target_features'.")
    }

  } else {  # Set to avoid errors in several if() statements. To-do: clean this up.

    causal_target_all <- "causal_target_not_in_target_features[j]"
    causal_effects_all <- "causal_target_not_in_target_features[j]"
    causal_target <- "causal_target_not_in_target_features[j]"
    causal_effects <- "causal_target_not_in_target_features[j]"
  }
  #----------------------------------------------------------------------------
  i <- j <- 1
  data_sample <- lapply_function(1:sample_size, function(i) {  # Loop over Monte Carlo samples.

    # Select a reference instance.
    reference_index <- sample(1:n_instances, size = 1, replace = FALSE)

    # Shuffle the column indices, keeping all column indices.
    feature_indices_random <- sample(1:n_features, size = n_features, replace = FALSE)

    #feature_indices_random <- 1:ncol(explain)
    #feature_indices_random <- c(feature_indices_random[c(16, 21)], feature_indices_random[-c(16, 21)])
    #feature_indices_random <- c(feature_indices_random[c(21, 16)], feature_indices_random[-c(16, 21)])
    #feature_indices_random <- c(feature_indices_random[-c(21)], feature_indices_random[c(21)])
    #feature_indices_random <- c(feature_indices_random[-c(16, 21)], feature_indices_random[c(16, 21)])

    # Shuffle the column order for the randomly selected instance.
    reference_instance <- reference[reference_index, feature_indices_random, drop = FALSE]

    # For the instances to be explained, shuffle the columns to match the randomly selected and shuffled instance.
    explain_instances <- explain[, feature_indices_random, drop = FALSE]

    data_sample_feature <- lapply(1:n_target_features, function(j) {  # Loop over features per Monte Carlo sample.

      # For each feature in the loop, find the position or index of the shuffled column. This index is the pivot point/column
      # that separates the real instance (to the left) from the random instance (to the right).
      target_feature_index <- which(names(explain) == target_features[j])
      target_feature_index_shuffled <- which(names(explain)[feature_indices_random] == target_features[j])

      # If there are any causal specifications, select the formula where the outcome matches the target feature.
      if (any(causal_target_all %in% target_features[j])) {

        causal_target <- causal_target_all[which(causal_target_all == target_features[j])]
        causal_effects <- unlist(causal_effects_all[which(causal_target_all == target_features[j])])

      } else {

        causal_target <- "causal_target_not_in_target_features[j]"
        causal_effects <- "causal_target_not_in_target_features[j]"
      }
      #------------------------------------------------------------------------
      # Re-order shuffled column indices for causal analysis. The purpose is to shift the index of
      # the target feature closer to the middle of the feature index vector to give enough room to
      # place N causal effects to the left (real exogenous effects/features that are conditioned on) and
      # right (fake exogenous effects/features that are not conditioned on) of the target index. The actual
      # shifting of the effects indices happens immediately after this 'if (...)' statement.
      if (causal_target %in% target_features[j]) {

        # Find the location of all causal effects in the shuffled feature index vector.
        effects_indices <- which(names(explain) %in% causal_effects)
        # Used to move the effects to the left and right of the causal target pivot point.
        effects_indices_shuffled <- which(names(explain)[feature_indices_random] %in% causal_effects)

        # If the endogenous causal target is too close to the beginning of the shuffled feature index vector,
        # move it to position 1 + length(causal_effects) and place the effect indices
        if (target_feature_index_shuffled <= length(causal_effects)) {

          # Make enough room to place all causal effects to the left of the pivot point to test the real effects.
          target_feature_index_shuffled_original <- target_feature_index_shuffled
          target_feature_index_shuffled_shifted <- 1 + length(causal_effects)

          # Swap indices: Move the target feature closer to the middle and take the feature index that the
          # shifted target is about to replace and put it where the target feature was original. A simple swap.
          feature_index_overwritten_by_target <- feature_indices_random[target_feature_index_shuffled_shifted]
          feature_indices_random[target_feature_index_shuffled_shifted] <- target_feature_index
          feature_indices_random[target_feature_index_shuffled_original] <- feature_index_overwritten_by_target

          # Overwrite for use later in this function: This is the pivot point between real and fake features.
          target_feature_index_shuffled <- target_feature_index_shuffled_shifted

          # Used to move the effects to the left and right of the causal target pivot point.
          effects_indices_shuffled <- which(names(explain)[feature_indices_random] %in% causal_effects)

        } else if (target_feature_index_shuffled > (n_features - length(causal_effects))) {

          # Make enough room to place all causal effects to the right of the pivot point to test the fake effects.
          target_feature_index_shuffled_original <- target_feature_index_shuffled
          target_feature_index_shuffled_shifted <- n_features - length(causal_effects)

          # Swap indices: Move the target feature closer to the middle and take the feature index that the
          # shifted target is about to replace and put it where the target feature was original. A simple swap.
          feature_index_overwritten_by_target <- feature_indices_random[target_feature_index_shuffled_shifted]
          feature_indices_random[target_feature_index_shuffled_shifted] <- target_feature_index
          feature_indices_random[target_feature_index_shuffled_original] <- feature_index_overwritten_by_target

          # Overwrite for use later in this function: This is the pivot point between real and fake features.
          target_feature_index_shuffled <- target_feature_index_shuffled_shifted

          # Used to move the effects to the left and right of the causal target pivot point.
          effects_indices_shuffled <- which(names(explain)[feature_indices_random] %in% causal_effects)
        }  # End feature index shuffling to accomodate N causal effects.
        #----------------------------------------------------------------------
        # Move the feature indices for the causal effects to the left (real) and right (fake) of the target
        # feature in the shuffled feature index vector so that when the Frankenstein instance is made by
        # concatenating the instance to be explained with a random reference instance, the correct
        # features are either conditioned on (left/real) or marginalized (right/fake).
        feature_indices_random_real_effects <- c(feature_indices_random[effects_indices_shuffled], feature_indices_random[-(effects_indices_shuffled)])
        feature_indices_random_fake_effects <- c(feature_indices_random[-(effects_indices_shuffled)], feature_indices_random[effects_indices_shuffled])

        # The target feature may be at different positions in the shuffled feature index vector depending
        # on whether or not it was shifted closer to the middle to accomodate N causal effects. As a result,
        # the target feature pivot point needs to be tracked separately for both real- and fake-effects
        # Frankenstein instances.
        target_feature_index_shuffled_real_effects <- which(feature_indices_random_real_effects == target_feature_index)
        target_feature_index_shuffled_fake_effects <- which(feature_indices_random_fake_effects == target_feature_index)
      }  # End causal feature index setup.
      #------------------------------------------------------------------------
      # Create the Frankenstein instances: a combination of the instance to be explained with the
      # reference instance to create a new instance that [likely] does not exist in the dataset.
      if (!causal_target %in% target_features[j]) {  # Symmetric Shapley: non-causal target feature.

        # These instances have the real target feature under investigation and everything to the right is
        # from the random reference instance.

        # Initialize the instances to be explained.
        explain_instance_with_target_feature <- explain_instances

        # Only create a Frankenstein instance if the target is not the last feature and there is actually
        # one or more features to the right of the target to replace with the reference.
        if (target_feature_index_shuffled < n_features) {

          explain_instance_with_target_feature[, (target_feature_index_shuffled + 1):(n_features)] <- reference_instance[, (target_feature_index_shuffled + 1):(n_features), drop = FALSE]
        }

        # These instances are otherwise the same as the Frankenstein instance created above with the
        # exception that the target feature is now replaced with the target feature in the random reference
        # instance. The difference in model predictions between these two Frankenstein instances is
        # what gives us the stochastic Shapley value approximation.
        explain_instance_without_target_feature <- explain_instance_with_target_feature
        explain_instance_without_target_feature[, target_feature_index_shuffled] <- reference_instance[, target_feature_index_shuffled]

      } else {  # Asymmetric Shapley: causal target feature.

        reference_instance_real_effects <- reference[reference_index, feature_indices_random_real_effects, drop = FALSE]
        reference_instance_fake_effects <- reference[reference_index, feature_indices_random_fake_effects, drop = FALSE]

        # These instances have the real target feature under investigation and everything to the right is
        # from the random reference instance.

        # Initialize the instances to be explained.
        explain_instance_with_feature_real_effects <- explain[, feature_indices_random_real_effects, drop = FALSE]
        explain_instance_with_feature_fake_effects <- explain[, feature_indices_random_fake_effects, drop = FALSE]

        # Only create a Frankenstein instance if the target is not the last feature and there is actually
        # one or more features to the right of the target to replace with the reference.
        if (target_feature_index_shuffled_real_effects < n_features) {
          explain_instance_with_feature_real_effects[, (target_feature_index_shuffled_real_effects + 1):(n_features)] <- reference_instance_real_effects[, (target_feature_index_shuffled_real_effects + 1):(n_features), drop = FALSE]
        }

        if (target_feature_index_shuffled_fake_effects < n_features) {
          explain_instance_with_feature_fake_effects[, (target_feature_index_shuffled_fake_effects + 1):(n_features)] <- reference_instance_fake_effects[, (target_feature_index_shuffled_fake_effects + 1):(n_features), drop = FALSE]
        }

        # These instances are otherwise the same as the Frankenstein instances created above with the
        # exception that the target feature is now replaced with the target feature in the random reference
        # instance. The difference in model predictions between these four Frankenstein instances is
        # what gives us the stochastic Shapley value approximation. These four instances represent the four
        # instances in eqn. 14 in https://arxiv.org/pdf/1910.06358.pdf.
        explain_instance_without_feature_real_effects <- explain_instance_with_feature_real_effects
        explain_instance_without_feature_real_effects[, target_feature_index_shuffled_real_effects] <- reference_instance_real_effects[, target_feature_index_shuffled_real_effects]

        explain_instance_without_feature_fake_effects <- explain_instance_with_feature_fake_effects
        explain_instance_without_feature_fake_effects[, target_feature_index_shuffled_fake_effects] <- reference_instance_fake_effects[, target_feature_index_shuffled_fake_effects]
      }  # End Frankenstein
      #------------------------------------------------------------------------
      # Reset the randomly shuffled features in the Frankenstein instances so that they are in the
      # correct/original order from the user-defined predict() function.
      # We'll also add meta-data so that instance-level Shapley values can be calculated with dplyr::group_by().
      if (!causal_target %in% target_features[j]) {  # Symmetric Shapley: non-causal target feature.

        explain_instance_with_target_feature <- explain_instance_with_target_feature[, order(feature_indices_random), drop = FALSE]
        explain_instance_without_target_feature <- explain_instance_without_target_feature[, order(feature_indices_random), drop = FALSE]

        data_explain_instance <- dplyr::bind_rows(list(explain_instance_with_target_feature, explain_instance_without_target_feature))

        data_explain_instance$index <- rep(1:nrow(explain), 2)  # Two Frankenstein instances per explained instance.

        data_explain_instance$feature_group <- rep(c('feature_static', 'feature_random'), each = nrow(explain))

        data_explain_instance$feature_name <- target_features[j]

        #data_explain_instance$causal_effects <- list(NA)

        data_explain_instance$causal <- 0

      } else {  # Asymmetric Shapley: causal target feature.

        explain_instance_with_feature_real_effects <- explain_instance_with_feature_real_effects[, order(feature_indices_random_real_effects), drop = FALSE]
        explain_instance_with_feature_fake_effects <- explain_instance_with_feature_fake_effects[, order(feature_indices_random_fake_effects), drop = FALSE]
        explain_instance_without_feature_real_effects <- explain_instance_without_feature_real_effects[, order(feature_indices_random_real_effects), drop = FALSE]
        explain_instance_without_feature_fake_effects <- explain_instance_without_feature_fake_effects[, order(feature_indices_random_fake_effects), drop = FALSE]

        data_explain_instance <- dplyr::bind_rows(list(explain_instance_with_feature_real_effects,
                                                       explain_instance_with_feature_fake_effects,
                                                       explain_instance_without_feature_real_effects,
                                                       explain_instance_without_feature_fake_effects))

        data_explain_instance$index <- rep(1:nrow(explain), 4)  # Four Frankenstein instances per explained instance.

        data_explain_instance$feature_group <- rep(c('feature_static_real_effects', 'feature_static_fake_effects',
                                                     'feature_random_real_effects', 'feature_random_fake_effects'),
                                                   each = nrow(explain))

        data_explain_instance$feature_name <- target_features[j]

        #data_explain_instance$causal_effects <- list(t(eval(causal_effects)))

        data_explain_instance$causal <- 1
      }
      #------------------------------------------------------------------------
      data_explain_instance$sample <- i

      data_explain_instance
    })  # End 'j' loop for data_sample_feature.
    #--------------------------------------------------------------------------
    data_sample_feature <- dplyr::bind_rows(data_sample_feature)

    data_sample_feature
  })  # End 'i' loop for data_sample.
  #----------------------------------------------------------------------------
  # Put all Frankenstein instances from all instances passed in 'explain' into
  # a single data.frame for the user-defined predict() function.
  data_predict <- dplyr::bind_rows(data_sample)

  # shapFlex internal function to compute the final symmetric and/or asymmetric Shapley values.
  data_shap <- predict_shapFlex(
    reference = reference,  # input arg.
    data_predict = data_predict,  # Calculated.
    models = models,  # input arg.
    predict_function = predict_function,  # input arg.
    n_features = n_features,  # Calculated.
    causal = causal,  # input arg.
    causal_target = causal_target_all # Calculated.
  )
  #----------------------------------------------------------------------------
  # Melt the input 'explain' data.frame for merging the model features to the Shapley values. Suppress
  # the warning resulting from any factors and numeric features being combined into one 'feature_value'
  # column and coerced to characters.
  data_merge <- suppressWarnings(tidyr::gather(explain, key = "feature_name", value = "feature_value"))

  data_merge$index <- rep(1:nrow(explain), n_features)  # The merge index for each instance.

  # Each instance in explain has one Shapley value per instance in a long data.frame format.
  data_out <- dplyr::left_join(data_shap, data_merge, by = c("index", "feature_name"))

  # Re-order columns for easier reading.
  data_out <- as.data.frame(dplyr::select(data_out, .data$index, .data$feature_name,
                                          .data$feature_value, .data$shap_effect, .data$shap_effect_sd,
                                          .data$intercept))

  class(data_out) <- c("shapFlex", class(data_out))

  return(data_out)
}
