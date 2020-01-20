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
#' @param causal Optional. A 2-column data.frame of feature names: The 1st column gives causes, the 2nd column gives effects.
#' @param causal_weights Optional. A numeric vector of \code{nrow(causal)} with weights between 0 and 1 that specifies the strength of
#' the causal asymmetric Shapley values. A weight of 1--the default if \code{causal_weights = NULL}--estimates a pure causal effect where the
#' instance to be explained is always conditioned on its true/actual values in the Monte Carlo sampling. A weight of .5 is equivalent to
#' the symmetric Shapley value calculation--within sampling error.
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

  if (!is.null(causal) && !methods::is(causal, "data.frame")) {
    stop("Enter the 'causal' specifications as a 2-column data.frame of feature names of the causes (1) and effects (2).")
  }

  if (!is.null(causal) && is.null(causal_weights)) {
    causal_weights <- rep(1, nrow(causal))  # Default is to estimate a pure "causal" effect.
  }

  if (!is.null(causal) && nrow(causal) != length(causal_weights)) {
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
  #----------------------------------------------------------------------------
  if (is.null(reference)) {  # Default is to explain all instances in 'explain' without a specific reference group.

    reference <- explain
    n_instances <- nrow(reference)

  } else {

    n_instances <- nrow(reference)
  }
  #----------------------------------------------------------------------------
  if (!is.null(causal)) {

    causal_graph <- igraph::graph_from_data_frame(causal, directed = TRUE)

    nodes <- attributes(igraph::V(causal_graph))$names

    # Nodes where arrows are leaving.
    each_node_causes <- purrr::map(nodes, ~ names(igraph::subcomponent(causal_graph, ., mode = "out")))
    names(each_node_causes) <- nodes

    # Nodes where arrows are entering.
    each_node_is_an_effect_from <- purrr::map(nodes, ~ names(igraph::subcomponent(causal_graph, ., mode = "in")))
    names(each_node_is_an_effect_from) <- nodes

    # Determine if each node is a cause and or an effect for computing the asymmetric Shapley values.
    causal_nodes <- nodes[nodes %in% unlist(lapply(each_node_is_an_effect_from, function(x) {x[-1]}))]
    effect_nodes <- nodes[nodes %in% unlist(lapply(each_node_causes, function(x) {x[-1]}))]

    # Shapley values are computed for all causal outcomes but only for causal effects specified in 'target_features'.
    if (any(!nodes %in% target_features)) {
      stop("One or more of the features from 'causal' is not in 'target_features'.")
    }

    if (any(!nodes %in% names(explain))) {
      stop("One or more of the features from 'causal' is not in the input data.")
    }

  } else {  # Symmetric Shapley values

    nodes <- NULL  # Set for if () conditions.
  }
  #----------------------------------------------------------------------------
  # i <- j <- 1
  data_sample <- lapply(1:sample_size, function(i) {  # Loop over Monte Carlo samples.

    # Select a reference instance.
    reference_index <- sample(1:n_instances, size = 1, replace = FALSE)

    # Shuffle the column indices, keeping all column indices.
    feature_indices_random <- sample(1:n_features, size = n_features, replace = FALSE)

    feature_names_random <- names(explain)[feature_indices_random]

    # Shuffle the column order for the randomly selected instance.
    reference_instance <- reference[reference_index, feature_indices_random, drop = FALSE]

    # For the instance(s) to be explained, shuffle the columns to match the randomly selected and shuffled instance.
    explain_instances <- explain[, feature_indices_random, drop = FALSE]

    data_sample_feature <- lapply_function(seq_along(target_features), function(j) {  # Loop over features per Monte Carlo sample.

      target_feature_index <- which(names(explain) == target_features[j])
      target_feature_index_shuffled <- which(names(explain)[feature_indices_random] == target_features[j])

      # Feature index shuffling for asymmetric Shapley values.
      if (target_features[j] %in% nodes) {

        target_feature_causes_these_features <- unlist(each_node_causes[target_features[j]])  # Exogenous target feature
        target_feature_is_caused_by <- unlist(each_node_is_an_effect_from[target_features[j]])  # Endogenous target feature

        target_index <- target_feature_index_shuffled
        causes_indices <- which(feature_names_random %in% target_feature_is_caused_by[-1])
        effects_indices <- which(feature_names_random %in% target_feature_causes_these_features[-1])
        sample_indices <- feature_indices_random[!feature_indices_random %in% c(target_index, causes_indices, effects_indices)]
        sample_real_indices <- sample_indices[sample_indices < target_index]  # Not in causal diagram, feature data from 'explain'.
        sample_fake_indices <- sample_indices[sample_indices > target_index]  # Not in causal diagram, feature data from 'reference'.

        feature_indices_real_causes_real_effects <- c(sample_real_indices, causes_indices, effects_indices, target_index, sample_fake_indices)
        feature_indices_real_causes_fake_effects <- c(sample_real_indices, causes_indices, target_index, effects_indices, sample_fake_indices)
        feature_indices_fake_causes_real_effects <- c(sample_real_indices, effects_indices, target_index, causes_indices, sample_fake_indices)
        feature_indices_fake_causes_fake_effects <- c(sample_real_indices, target_index, causes_indices, effects_indices, sample_fake_indices)

        # Manual check.
        # feature_names_random[feature_indices_real_causes_real_effects]
        # feature_names_random[feature_indices_real_causes_fake_effects]
        # feature_names_random[feature_indices_fake_causes_real_effects]
        # feature_names_random[feature_indices_fake_causes_fake_effects]
      }
      #------------------------------------------------------------------------
      # Create the Frankenstein instances: a combination of the instance to be explained with the
      # reference instance to create a new instance that [likely] does not exist in the dataset.
      if (!target_features[j] %in% nodes) {  # Symmetric Shapley: non-causal target feature.

        # These instances have the real target feature and all features to the right of the shuffled
        # target feature index are from the random reference instance.

        # Initialize the instances to be explained.
        explain_instance_real_target <- explain_instances

        # Only create a Frankenstein instance if the target is not the last feature and there is actually
        # one or more features to the right of the target to replace with the reference.
        if (target_feature_index_shuffled < n_features) {

          explain_instance_real_target[, (target_feature_index_shuffled + 1):(n_features)] <- reference_instance[, (target_feature_index_shuffled + 1):(n_features), drop = FALSE]
        }

        # These instances are otherwise the same as the Frankenstein instance created above with the
        # exception that the target feature is now replaced with the target feature in the random reference
        # instance. The difference in model predictions between these two Frankenstein instances is
        # what gives us the stochastic Shapley value approximation.
        explain_instance_fake_target <- explain_instance_real_target
        explain_instance_fake_target[, target_feature_index_shuffled] <- reference_instance[, target_feature_index_shuffled, drop = TRUE]

      } else {  # Asymmetric Shapley: Causal target feature.

        # The following code creates the 8 versions of the instance(s) being explained using Eqn. 15 from https://arxiv.org/pdf/1910.06358.pdf.
        # The various Frankenstein instances differ by whether or not they (a) condition on the upstream causes,
        # the downstream effects, both, or neither and (b) if the target feature is real or fake.

        if (target_features[j] %in% causal_nodes) {

          #--------------------------------------------------------------------
          # Upper left: Upstream causes = real, downstream effects = fake.
          reference_instance_real_causes_fake_effects <- reference_instance[, feature_indices_real_causes_fake_effects, drop = FALSE]

          explain_instance_real_causes_fake_effects_real_target <- explain_instances[, feature_indices_real_causes_fake_effects, drop = FALSE]

          target_index_temp <- which(names(explain_instance_real_causes_fake_effects_real_target) == target_features[j])

          if (target_index_temp < n_features) {

            explain_instance_real_causes_fake_effects_real_target[, (target_index_temp + 1):(n_features)] <- reference_instance_real_causes_fake_effects[, (target_index_temp + 1):(n_features), drop = FALSE]
          }

          explain_instance_real_causes_fake_effects_fake_target <- explain_instance_real_causes_fake_effects_real_target
          explain_instance_real_causes_fake_effects_fake_target[, target_index_temp] <- reference_instance_real_causes_fake_effects[, target_index_temp, drop = TRUE]
          #--------------------------------------------------------------------
          # Upper right. Upstream causes = fake, downstream effects = real.
          reference_instance_fake_causes_real_effects <- reference_instance[, feature_indices_fake_causes_real_effects, drop = FALSE]

          explain_instance_fake_causes_real_effects_real_target_cause <- explain_instances[, feature_indices_fake_causes_real_effects, drop = FALSE]

          target_index_temp <- which(names(explain_instance_fake_causes_real_effects_real_target_cause) == target_features[j])

          if (target_index_temp < n_features) {

            explain_instance_fake_causes_real_effects_real_target_cause[, (target_index_temp + 1):(n_features)] <- reference_instance_fake_causes_real_effects[, (target_index_temp + 1):(n_features), drop = FALSE]
          }

          explain_instance_fake_causes_real_effects_fake_target_cause <- explain_instance_fake_causes_real_effects_real_target_cause
          explain_instance_fake_causes_real_effects_fake_target_cause[, target_index_temp] <- reference_instance_fake_causes_real_effects[, target_index_temp, drop = TRUE]
          #--------------------------------------------------------------------
        }

        if (target_features[j] %in% effect_nodes) {

          #--------------------------------------------------------------------
          # Bottom left. Upstream causes = real, downstream effects = fake.
          reference_instance_real_causes_fake_effects <- reference_instance[, feature_indices_real_causes_fake_effects, drop = FALSE]

          explain_instance_real_causes_fake_effects_real_target_effect <- explain_instances[, feature_indices_real_causes_fake_effects, drop = FALSE]

          target_index_temp <- which(names(explain_instance_real_causes_fake_effects_real_target_effect) == target_features[j])

          if (target_index_temp < n_features) {

            explain_instance_real_causes_fake_effects_real_target_effect[, (target_index_temp + 1):(n_features)] <- reference_instance_real_causes_fake_effects[, (target_index_temp + 1):(n_features), drop = FALSE]
          }

          explain_instance_real_causes_fake_effects_fake_target_effect <- explain_instance_real_causes_fake_effects_real_target_effect
          explain_instance_real_causes_fake_effects_fake_target_effect[, target_index_temp] <- reference_instance_real_causes_fake_effects[, target_index_temp, drop = TRUE]
          #--------------------------------------------------------------------
          # Bottom right. Upstream causes = fake, downstream effects = real.
          reference_instance_fake_causes_real_effects <- reference_instance[, feature_indices_fake_causes_real_effects, drop = FALSE]

          explain_instance_fake_causes_real_effects_real_target <- explain_instances[, feature_indices_fake_causes_real_effects, drop = FALSE]

          target_index_temp <- which(names(explain_instance_fake_causes_real_effects_real_target) == target_features[j])

          if (target_index_temp < n_features) {

            explain_instance_fake_causes_real_effects_real_target[, (target_index_temp + 1):(n_features)] <- reference_instance_fake_causes_real_effects[, (target_index_temp + 1):(n_features), drop = FALSE]
          }

          explain_instance_fake_causes_real_effects_fake_target <- explain_instance_fake_causes_real_effects_real_target
          explain_instance_fake_causes_real_effects_fake_target[, target_index_temp] <- reference_instance_fake_causes_real_effects[, target_index_temp, drop = TRUE]
        }  # End causal Frankenstein.
      }  # End Frankenstein.
      #------------------------------------------------------------------------
      # Reset the randomly shuffled features in the Frankenstein instances so that they are in the
      # correct/original order for the user-defined predict() function.
      # We'll also add meta-data so that instance-level Shapley values can be calculated with dplyr::group_by().
      if (!target_features[j] %in% nodes) {  # Symmetric Shapley: non-causal target feature.

        explain_instance_real_target <- explain_instance_real_target[, names(explain), drop = FALSE]
        explain_instance_fake_target <- explain_instance_fake_target[, names(explain), drop = FALSE]

        data_explain_instance <- dplyr::bind_rows(list(explain_instance_real_target, explain_instance_fake_target))

        data_explain_instance$index <- rep(1:nrow(explain), 2)  # Two Frankenstein instances per explained instance.

        data_explain_instance$feature_group <- rep(c('real_target', 'fake_target'), each = nrow(explain))

        data_explain_instance$feature_name <- target_features[j]

        data_explain_instance$causal <- 0

        data_explain_instance$causal_type <- NA

      } else {  # Asymmetric Shapley: causal target feature.

        if (target_features[j] %in% causal_nodes) {

          explain_instance_real_causes_fake_effects_real_target <- explain_instance_real_causes_fake_effects_real_target[, names(explain), drop = FALSE]
          explain_instance_real_causes_fake_effects_fake_target <- explain_instance_real_causes_fake_effects_fake_target[, names(explain), drop = FALSE]

          explain_instance_fake_causes_real_effects_real_target_cause <- explain_instance_fake_causes_real_effects_real_target_cause[, names(explain), drop = FALSE]
          explain_instance_fake_causes_real_effects_fake_target_cause <- explain_instance_fake_causes_real_effects_fake_target_cause[, names(explain), drop = FALSE]
        }

        if (target_features[j] %in% effect_nodes) {

          explain_instance_real_causes_fake_effects_real_target_effect <- explain_instance_real_causes_fake_effects_real_target_effect[, names(explain), drop = FALSE]
          explain_instance_real_causes_fake_effects_fake_target_effect <- explain_instance_real_causes_fake_effects_fake_target_effect[, names(explain), drop = FALSE]

          explain_instance_fake_causes_real_effects_real_target <- explain_instance_fake_causes_real_effects_real_target[, names(explain), drop = FALSE]
          explain_instance_fake_causes_real_effects_fake_target <- explain_instance_fake_causes_real_effects_fake_target[, names(explain), drop = FALSE]
        }
        #----------------------------------------------------------------------

        if (target_features[j] %in% causal_nodes) {

          data_explain_instance <- dplyr::bind_rows(list(
            explain_instance_real_causes_fake_effects_real_target,
            explain_instance_real_causes_fake_effects_fake_target,
            explain_instance_fake_causes_real_effects_real_target_cause,
            explain_instance_fake_causes_real_effects_fake_target_cause
          ))

          data_explain_instance$index <- rep(1:nrow(explain), 4)  # Four Frankenstein instances per explained instance.

          data_explain_instance$feature_group <- rep(c("real_causes_fake_effects_real_target", "real_causes_fake_effects_fake_target",
                                                       "fake_causes_real_effects_real_target_cause", "fake_causes_real_effects_fake_target_cause"),
                                                     each = nrow(explain))

          data_explain_instance$causal_type <- "target_is_a_cause"
        }

        if (target_features[j] %in% effect_nodes) {

          data_explain_instance <- dplyr::bind_rows(list(
            explain_instance_real_causes_fake_effects_real_target_effect,
            explain_instance_real_causes_fake_effects_fake_target_effect,
            explain_instance_fake_causes_real_effects_real_target,
            explain_instance_fake_causes_real_effects_fake_target
          ))

          data_explain_instance$index <- rep(1:nrow(explain), 4)  # Four Frankenstein instances per explained instance.

          data_explain_instance$feature_group <- rep(c("real_causes_fake_effects_real_target_effect", "real_causes_fake_effects_fake_target_effect",
                                                       "fake_causes_real_effects_real_target", "fake_causes_real_effects_fake_target"),
                                                     each = nrow(explain))

          data_explain_instance$causal_type <- "target_is_an_effect"
        }

        if (target_features[j] %in% causal_nodes && target_features[j] %in% effect_nodes) {

          data_explain_instance <- dplyr::bind_rows(list(
            explain_instance_real_causes_fake_effects_real_target,
            explain_instance_real_causes_fake_effects_fake_target,
            explain_instance_fake_causes_real_effects_real_target_cause,
            explain_instance_fake_causes_real_effects_fake_target_cause,
            explain_instance_real_causes_fake_effects_real_target_effect,
            explain_instance_real_causes_fake_effects_fake_target_effect,
            explain_instance_fake_causes_real_effects_real_target,
            explain_instance_fake_causes_real_effects_fake_target
            ))

          data_explain_instance$index <- rep(1:nrow(explain), 8)  # Eight Frankenstein instances per explained instance.

          data_explain_instance$feature_group <- rep(c(
            "real_causes_fake_effects_real_target", "real_causes_fake_effects_fake_target",  # Target is a causal node.
            "fake_causes_real_effects_target_cause", "fake_causes_real_effects_fake_target_cause",  # Target is a causal node.
            "real_causes_fake_effects_real_target_effect", "real_causes_fake_effects_fake_target_effect",  # Target is an effect node.
            "fake_causes_real_effects_real_target", "fake_causes_real_effects_fake_target"  # Target is an effect node.
            ),
          each = nrow(explain))

          data_explain_instance$causal_type <- rep(c(
            "target_is_a_cause", "target_is_a_cause", "target_is_a_cause", "target_is_a_cause",
            "target_is_an_effect", "target_is_an_effect", "target_is_an_effect", "target_is_an_effect"
          ),
          each = nrow(explain))
        }

        data_explain_instance$feature_name <- target_features[j]

        data_explain_instance$causal <- 1
      }  # End causal.
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
    causal_weights = causal_weights  # Calculated.
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
