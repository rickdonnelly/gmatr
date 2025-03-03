#' Calculate the percent of links within MDE and AEC limits
#'
#' @param df Name of the tibble (data frame) object containing the data to be 
#'   plotted
#' @param observed The variable in `df` containing the counted or observed
#'   flows to compare to
#' @param predicted The variable in `df` containing the simulated or 
#'   predicted flows
#'
#' @details This function calculates the number of links that fall within 
#'   the NCHRP limits for maximum desirable error (MDE) and approximate
#'   error in count (AEC) as defined in NCHRP Report 750.
#'
#' @export
#' @examples
#' eh <- plot_NCHRP_comparison(df, "count", "assigned", "road_type")
#' print(eh)
#' pdf(filename, height = 8, width = 11); print(eh); dev.off()

calc_NCHRP_statistics <- function(df, observed, predicted) {
  # Remove observations that do not have valid observed or predicted flows. We
  # will assume that a count must be a positive number to quality, and predicted
  # values must be zero or greater. Missing values are invalid in both cases.
  drop_missing <- filter(df, !is.na(.data[[observed]]), .data[[observed]] >= 1,
    !is.na(.data[[predicted]]), .data[[predicted]] >= 0)
  n_dropped <- nrow(df) - nrow(drop_missing)
  if (n_dropped > 0) {
    warning(n_dropped, " observations with invalid observed or predicted values dropped")
  }

  # Calculate the percent error
  add_error <- mutate(drop_missing, 
    pct_error = ((.data[[predicted]] - .data[[observed]]) / .data[[observed]]) * 100)

  # Determine how many values are within the MDE and AEC limits
  calc_mde <- function(x) -13.7721899 + (555.13816 * x^-0.26024293)
  calc_aec <- function(x) ((3.706633 / log(x)) - 0.264598) * 100
  add_bounds <- mutate(add_error, this_mde = calc_mde(.data[[observed]]),
    within_mde = ifelse(abs(pct_error) <= this_mde, 1, 0),
    this_aec = calc_aec(.data[[observed]]),
    within_aec = ifelse(abs(pct_error) <= this_aec, 1, 0))
  pct_within_mde <- round((sum(add_bounds$within_mde) / nrow(add_bounds)) * 100, 1)
  pct_within_aec <- round((sum(add_bounds$within_aec) / nrow(add_bounds)) * 100, 1)

  # Return the results in a list
  return(list(MDE = pct_within_mde, AEC = pct_within_aec))
}