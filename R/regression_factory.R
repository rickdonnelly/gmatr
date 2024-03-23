#' Factory method to fit regression and related models using caret
#'
#' @param df Name of the tibble (data frame) object containing a single
#'   dependent variable and one or more predictor variables
#' @param dependent_var A string identifying the dependent variable contained
#'   in `df`. 
#' @param this_method A string specifying which of the models available in
#'   `caret::train()` will be used for fitting the predictors to the dependent
#'   variable in `df`
#' @param gamma A floating point value specifying how close a predicted value must
#'   be to observed value to be considered accurate. The value must be between 0
#'   (exact match required) to 1.0 ("hitting the broad side of a barn"). The
#'   default value is 0 (zero).
#' @param add_plot A boolean variable denoting whether the returned list includes
#'   a plot of the observed (`dependent_var`) versus `predicted` values; the
#'   default value is TRUE
#'
#' @details This function prepares data for fitting regression models using the
#'   `caret::train()` function. It removes observations with missing values and
#'   converts categorical variables and factors to dummy variables before feeding
#'   the data to `train()`. Parameters specific to a particular method can be 
#'   specified, which will be passed to `train()` without modification for those
#'   functions requiring method-specific paramters. The dataset must contain a
#'   single dependent variable and as many predictor variables as desired.
#'   The `caret::train()` function is stricter about missing values than many
#'   (most?) R packages, stopping with error messages when they are found. Thus,
#'   the burden is placed on the user to ensure that missing values are handled
#'   before using this function. If missing values are found those observations
#'   are removed and a warning message is written. Fitting continues with the
#'   remaining observations. This function returns a list that includes (a) the
#'   object returned by `train()`, whose contents vary by the method used; (b)
#'   a ggplot object of observed versus predicted values (if `add_plot == TRUE`),
#'   (c) a brief set of fit statistics, and (d) model accuracy statistics. The
#'   latter simply compare the observed values in `dependent_var` to the values
#'   predicted by `this_method`. 
#'
#' @export
#' @examples
#' model_fit <- regression_factory(my_tibble, "observed", "lm", gamma = 0.1)

regression_factory <- function(eds, dependent_var, this_method, gamma = 0.0,
  add_plot = TRUE, ...) {
  # We first must omit missing values, which the user hopefully has already done
  # but we need to verify
  drop_missing <- na.omit(eds)
  n_dropped <- nrow(eds) - nrow(drop_missing)
  if (n_dropped > 0) {
    warning(paste0(n_dropped, " of ", nrow(eds), 
      " observations with missing values dropped"))
  }
  
  # Use one-hot encoding of dummy variables:
  my_formula <- paste0(dependent_var, " ~ .")
  add_dummies <- caret::dummyVars(my_formula, data = drop_missing)
  update_eds <- data.frame(predict(add_dummies, newdata = drop_missing))
  
  # Now run the model with the chosen method:
  local_fit <- caret::train(y = drop_missing[[dependent_var]], x = update_eds,
      method = this_method, ...)
  
  # Append the predicted value to the data frame so that we can add predictive
  # accuracy statistics
  drop_missing$predicted <- predict(local_fit)
  accuracy_stats <- gmatr::calc_model_accuracy(drop_missing, dependent_var,
    "predicted", gamma)
  
  # Retrieve a standard few set of statistics regardless of the method used, as
  # different packages report summaries differently
  fit_stats <- tibble(n = nrow(local_fit$trainingData),
    Rsquared = round(local_fit$results$Rsquared, 2),
    RMSE = round(local_fit$results$RMSE, 2), MAE = round(local_fit$results$MAE, 2))

  # If a picture of observed versus estimated accuracy is desired package it up
  if (add_plot == TRUE) {
    fit_brief <- paste0("n=", fit_stats$n, " Rsquared=", fit_stats$Rsquared,
      " RMSE=", fit_stats$RMSE, " MAE=", fit_stats$MAE)
    oneliner <- paste0("[", local_fit$method, "] ", dependent_var, " ~ . : ",
      fit_brief)
    x_label <- paste0("observed (", dependent_var, ")")
    plot_obj <- ggplot(drop_missing, aes(x = !!sym(dependent_var), y = predicted)) +
      geom_abline(intercept = 0, slope = 1, colour = "grey30", linetype = "dotted") +
      geom_point(alpha = 0.4) + labs(x = x_label, subtitle = oneliner)
  } else {
    plot_obj <- "No graphical outputs requested"
  }
  
  # Finally, return it all in one big ball and head for the beach
  return(list(fit_statistics = fit_stats, accuracy_sum = accuracy_stats,
    model_fit = local_fit, plot = plot_obj))
}