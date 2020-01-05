#' Compute symmetric or asymmetric stochastic feature-level Shapley values
#'
#' This function uses user-defined trained models and prediction functions to compute approximate Shapley values for
#' single models. Shapley values can be calculated for a subset of model features which reduces the
#' typically expensive computation of approximate Shapley values in high-dimensional models.
#'
#' @param explain A data.frame of instances to be explained using Shapley values. \code{explain} is passed internally
#' as a data.frame to \code{predict_function}.
#' @param reference Optional. A data.frame with the same format as \code{explain}--with possibly more or fewer rows--of instances
#' which serve as a reference group against which the Shapley value deviations from \code{explain} are compared. That is,
#' \code{reference} is used to calculate an average prediction or intercept value. \code{reference} is passed internally
#' as a data.frame to \code{predict_function}.
#' @param model A trained prediction model object used to compute Shapley values. \code{model} is passed internally to \code{predict_function}.
#' @param predict_function A \code{predict()}-type wrapper function that takes 2 required positional arguments--(1) the trained model from \code{model}
#' and (2) a data.frame of instances with the same format as \code{explain}. For numeric outcomes, the function should \code{return()} a
#' 1-column data.frame of model predictions; the column name does not matter.
#' @param target_features Optional. A character vector that is a subset of feature names in \code{explain} for which Shapley values will be computed.
#' For high-dimensional models, selecting a subset of interesting features may dramatically speed up computation time. The default behavior is
#' to return Shapley values for all instances and features in \code{explain}.
#' @param causal Optional. A list of 1 or more formulas that specify a causal direction for computing asymmetric Shapley values. For example,
#' \code{list(x1 ~ x2 + x3)} or \code{list(formula(x1 ~ x2 + x3))} computes Shapley values for \code{x_1} after conditioning on
#' the true/actual values of \code{x2} and \code{x3} for the instance being explained. Only 1 feature is allowed on the left hand side
#' of the formula, and only 1 formula is allowed per left hand side feature i.e., no duplicated causal constraints for a target feature.
#' @param causal_weights Optional. A numeric vector of \code{length(causal)} with weights between 0 and 1 that specifies the strength of
#' the causal asymmetric Shapley values. A weight of 1--the default if \code{causal_weights = NULL}--estimates a pure causal effect where the
#' instance to be explained is always conditioned on its true/actual values in the Monte Carlo sampling (e.g., Shapley values for \code{x1}
#' are based on the instance's true \code{x2} and \code{x3}). A weight of .5 is equivalent to the symmetric Shapley value
#' calculation--within sampling error--and represents the case where the researcher is uncertain as to whether or not \code{x2} and
#' \code{x_3} causally precede or follow \code{x1}.
#' @param sample_size A numeric vector of length 1 giving the number of Monte Carlo samples used to compute the stochastic Shapley values for
#' each feature.
#' @param use_future Boolean. If \code{TRUE}, the \code{future} package is used to calculate Shapley values in parallel across \code{sample_size}.
#' @return A data.frame with class \code{shapFlex} of the feature-level Shapley values for all instaces in \code{explain}.
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
  if (is.null(reference)) {  # Default is to explain all instances in 'explain' without a specific reference group.

    reference <- explain
    n_instances <- nrow(reference)

  } else {

    n_instances <- nrow(reference)
  }
  #----------------------------------------------------------------------------
  if (!is.null(causal)) {

    causal_formula <- lapply(causal, function(x){as.character(attributes(stats::terms(x))$variables)[-1]})

    causal_target_all <- lapply(causal_formula, function(x){x[1]})
    causal_effects_all <- lapply(causal_formula, function(x){x[-1]})

    # shapFlex currently only supports unduplicated endogenous targets.
    if (any(duplicated(unlist(causal_target_all)))) {
      stop("Remove formulas from 'causal' with duplicate endogenous/outcome features e.g., list(x1 ~ x2, x1 ~ x3) should be list(x1 ~ x2 + x3).")
    }

    # Shapley values are computed for all causal outcomes but only for causal effects specified in 'target_features'.
    if (any(!unlist(causal_target_all) %in% target_features)) {
      stop("One or more of the endogenous/outcome features from 'causal' is not in 'target_features'.")
    }

    if (any(!unlist(causal_effects_all) %in% names(explain))) {
      stop("One or more of the exogenous/effects features from 'causal' is not in the input data.")
    }
    #--------------------------------------------------------------------------
    causal_effects_unique <- unique(unlist(causal_effects_all))
    causal_effects_unique <- causal_effects_unique[causal_effects_unique %in% target_features]  # Estimate select effects.
    causal_effects_unique <- sapply(causal_effects_unique, as.list)

    # For each causal effect, find all of the causal targets that it is an effect of. The collection
    # of these causal targets will become causal effects.
    map_of_causal_targets_per_effect <- lapply(seq_along(unlist(causal_effects_unique)), function(i) {
      effect_in_formulas <- lapply(seq_along(causal_effects_all), function(j) {
        unlist(causal_effects_unique)[i] %in% causal_effects_all[[j]]
      })
      effect_in_formulas <- which(unlist(effect_in_formulas))
    })

    # This is the list of causal targets that each effect was paired with in the 'causal' input arg.
    new_causal_effects <- lapply(seq_along(map_of_causal_targets_per_effect), function(i) {
      unlist(causal_target_all[map_of_causal_targets_per_effect[[i]]])
    })

    # The causal effects need to be transformed into targets to get weighted properly when they are
    # on the right hand side of a causal formula. This focus is due to the fact that Shapley values
    # sum to the overall model prediction and if one Shapley value goes up another must go down.
    causal_target_all <- append(causal_target_all, causal_effects_unique)
    causal_target_all <- unlist(causal_target_all)

    # Add a list of causal effects to the newly created formulas where the causes have been transformed
    # to be endogenous.
    causal_effects_all <- append(causal_effects_all, new_causal_effects)

    # Re-order to identify features that are both causal targets and causal effects, placing causal targets
    # at the start of the 'j' loop.
    target_feature_order <- c(causal_target_all, target_features[!target_features %in% causal_target_all])

    # The length of the 'j' loop over features where Shapley values are calculated needs to be expanded
    # to account for the estimates of any causal effects that (a) aren't already endogenous and (b) are listed
    # as a target feature in the function call.
    n_target_features <- length(target_feature_order)
    #--------------------------------------------------------------------------
    # Users are only asked to enter 1 causal weight per formula. However,
    # weights need to be calculated for any causal effects (right-had side of a formula) in 'target_features'
    # to satisfy the sum constraint on the Shapley values equaling the model preictions (i.e., more than just
    # the target/outcome needs to be adjusted with asymmetric Shapley values). Causal targets, then, are also effects
    # for their own effects. The math will be spelled out in a vignette, but, roughly, the following setup of
    # list(x1 ~ x2, x2 ~ x1) with causal_weights = c(1, 1) in the function will be trainsformed into causal weights
    # of .5 and .5 in the code below.

    # Initialize a list of data.frames of the cause and effect weights as seen in eqn 16.
    target_weights <- lapply(causal_target_all, data.frame)
    effect_weights <- lapply(causal_effects_all, data.frame)

    target_weights <- lapply(seq_along(target_weights), function(i) {
      names(target_weights[[i]]) <- "feature_name"
      target_weights[[i]]$feature_name <- as.character(target_weights[[i]]$feature_name)
      target_weights[[i]]$weight_12 <- causal_weights[i]  # 1 is the pure causal impact for a causal target/outcome after conditioning on all causal effects.
      target_weights[[i]]$weight_21 <- 1 - causal_weights[i]
      target_weights[[i]]$causal_type <- "causal_target"
      target_weights[[i]]
    })

    effect_weights <- lapply(seq_along(causal_effects_all), function(i) {
      names(effect_weights[[i]]) <- "feature_name"
      effect_weights[[i]]$feature_name <- as.character(effect_weights[[i]]$feature_name)
      effect_weights[[i]]$weight_12 <- causal_weights[i]  # 1 is the pure impact of the causal effect relative to the average baseline output.
      effect_weights[[i]]$weight_21 <- 1 - causal_weights[i]
      effect_weights[[i]]$causal_type <- "causal_effect"
      effect_weights[[i]]
      })

    combined_weights <- dplyr::bind_rows(target_weights, effect_weights)

    # The weights are averaged out to account for all the times when a feautre was a causal target and/or
    # a causal effect as specified in the 'causal' argument.

    # This dataset is passed into predict_shapFlex in zzz.R and used to compute the asymmetric
    # Shapley values in eqn 16.
    combined_weights <- combined_weights %>%
      dplyr::group_by(feature_name, causal_type) %>%
      dplyr::summarize("weight_12" = mean(weight_12, na.rm = TRUE),
                       "weight_21" = mean(weight_21, na.rm = TRUE))

  } else {  # Set to avoid errors in several if() statements. To-do: clean this up.

    causal_target_all <- "causal_target_not_in_target_features[j]"
    causal_effects_all <- "causal_target_not_in_target_features[j]"
    causal_target <- "causal_target_not_in_target_features[j]"
    causal_effects <- "causal_target_not_in_target_features[j]"
    causal_effects_targets <- "causal_target_not_in_target_features[j]"

    target_feature_order <- target_features
  }
  #----------------------------------------------------------------------------
  data_sample <- lapply(1:sample_size, function(i) {  # Loop over Monte Carlo samples.

    # Select a reference instance.
    reference_index <- sample(1:n_instances, size = 1, replace = FALSE)

    # Shuffle the column indices, keeping all column indices.
    feature_indices_random <- sample(1:n_features, size = n_features, replace = FALSE)

    # Shuffle the column order for the randomly selected instance.
    reference_instance <- reference[reference_index, feature_indices_random, drop = FALSE]

    # For the instances to be explained, shuffle the columns to match the randomly selected and shuffled instance.
    explain_instances <- explain[, feature_indices_random, drop = FALSE]

    data_sample_feature <- lapply_function(1:n_target_features, function(j) {  # Loop over features per Monte Carlo sample.

      # For each feature in the loop, find the position or index of the shuffled column. This index is the pivot point/column
      # that separates the real instance (to the left) from the random instance (to the right).
      target_feature_index <- which(names(explain) == target_feature_order[j])
      target_feature_index_shuffled <- which(names(explain)[feature_indices_random] == target_feature_order[j])

      # If there are any causal specifications, select the formula where the outcome matches the target feature.
      # Note that a target feature may be endogenous or exogenous--both need to be adjust for asymmetric calculations.
      if (any(causal_target_all %in% target_feature_order[j])) {

        causal_target <- causal_target_all[j]  #[which(causal_target_all == target_features[j])]
        # Find the list of causal effects that matches the causal feature in this 'j' loop.
        causal_effects <- unlist(causal_effects_all[j]) #unlist(causal_effects_all[which(causal_target_all == target_features[j])])

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
      if (causal_target %in% target_feature_order[j]) {

        # Find the location of all causal effects in the shuffled feature index vector. For causal effects
        # where asymmetric Shapley values have been requested, the placeholder feature is ".".
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
      if (!causal_target %in% target_feature_order[j]) {  # Symmetric Shapley: non-causal target feature.

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
      if (!causal_target %in% target_feature_order[j]) {  # Symmetric Shapley: non-causal target feature.

        explain_instance_with_target_feature <- explain_instance_with_target_feature[, order(feature_indices_random), drop = FALSE]
        explain_instance_without_target_feature <- explain_instance_without_target_feature[, order(feature_indices_random), drop = FALSE]

        data_explain_instance <- dplyr::bind_rows(list(explain_instance_with_target_feature, explain_instance_without_target_feature))

        data_explain_instance$index <- rep(1:nrow(explain), 2)  # Two Frankenstein instances per explained instance.

        data_explain_instance$feature_group <- rep(c('feature_static', 'feature_random'), each = nrow(explain))

        data_explain_instance$feature_name <- target_feature_order[j]

        data_explain_instance$causal <- 0

        data_explain_instance$causal_type <- NA

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

        data_explain_instance$feature_name <- target_feature_order[j]

        data_explain_instance$causal <- 1

        # From eqn 16, asymmetric Shapley values for causal targets (shap_u_2) and causal effects (shap_u_1)
        # are calculated differently; calculations are in predict_shapFlex().
        if (j <= length(causal)) {  # True causal targets are in the first list slots.

          data_explain_instance$causal_type <- "causal_target"  # Left hand side of the user input formulas.

        } else {

          data_explain_instance$causal_type <- "causal_effect"  # Right hand side of the user input formulas.
        }
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
    model = model,  # input arg.
    predict_function = predict_function,  # input arg.
    n_features = n_features,  # Calculated.
    causal = causal,  # input arg.
    causal_weights = combined_weights,  # Calculated
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
