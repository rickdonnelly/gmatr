#' Calculate model accuracy statistics from a tibble of observed and predicted values
#'
#' @param df Name of the tibble (data frame) object containing the observed and
#'   predicted values
#' @param observed A string identifying the column name of the observed values
#' @param predicted A string identifying the column name of the predicted values
#' @param gamma A floating point value specifying how close a predicted value must
#'   be to observed value to be considered accurate. The value must be between 0
#'   (exact match required) to 1.0 ("hitting the broad side of a barn"). The
#'   default value is 0 (zero).
#' @param places An integer denoting the number of places past the decimal that
#'.  floating point statistics will be reported with. The default value is 2.
#'
#' @details This function returns a tibble with a variety of model accuracy 
#'   statistics based on fields containing observed and predicted values in the
#'   input tibble (data frame). The user specifies the field names containing 
#'   these two values. Most of the statistics are commonly used, so require no
#'   explanation. The last variable produced is _predictive accuracy_ (PA), a
#'   key performance measure used in ML models. The `gamma` parameter denotes
#'   how close the predicted value must be to the observed value to be
#'   considered a match. By default `gamma` is zero, meaning an exact match
#'   must be obtained for an observation to be considered accurate. A value of
#'   0.1 would allow any value +/- 10 percent of the observation to count as a
#'   match. A zero value for `gamma` is appropriate for categorical choice
#'   models, while non-zero values are typically used for numeric predictions.
#'
#' @export
#' @examples
#' calc_model_accuracy(assignment_results, "count", "assigned", 0.2)
#' # You can group the data when passing it to this function:
#' calc_model_accuracy(group_by(assignment_results, functional_type),
#'.  "count", "assigned", 0.1))

calc_model_accuracy <- function(dt, observed, predicted, gamma = 0, places = 2) {
  # Make sure that the user has passed a valid value for gamma
  if (gamma < 0 | gamma > 1) {
    warning("Value of gamma supplied is outside of allowable range")
    return(NULL)
  }
  
  # Create an expanded data frame with individual calculations
  expanded <- mutate(dt, P = !!sym(predicted), O = !!sym(observed),
    error = P - O, SSD = error^2, lower = O - (O * gamma), upper = O + (O * gamma),
    within_gamma = ifelse(P >= lower & P <= upper, 1, 0))

  # Drop observations with missing values for observed or predicted values,
  # notify the user, and ensure that we have 1 or more remaining observations
  verified <- filter(expanded, !is.na(O), !is.na(P))
  n_verified <- nrow(verified); n_provided <- nrow(dt)
  if (n_verified < n_provided) {
    n_dropped <- n_provided - n_verified
    message(paste(n_dropped, "of", n_provided,
      "observations with missing observed or predicted values dropped"))
  }
  if (n_verified == 0) {
    warning("Removing missing observations depleted the input dataset")
    return(NULL)
  }

  # Now calculate the aggregated statistics
  aggregated <- summarise(verified, n = n(), bias = sum(error) / n,
    MAE = round(sum(abs(error)) / n, places),
    PMAE = round(sum(abs(error)) / sum(O), places),
    RMSE = round(sqrt(sum(SSD) / n), places), 
    PRMSE = round(RMSE / mean(O), places),
    MSE = round(sum(SSD) / n, places), PA = round(sum(within_gamma) / n, places))

  # Rename predictive accuracy label to include the gamma level
  modify_names <- names(aggregated)
  modify_names[length(modify_names)] <- paste0("PA_", gamma)
  names(aggregated) <- modify_names
  return(aggregated)
}
