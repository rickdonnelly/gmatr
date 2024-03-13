#' Count the number of levels of a categorical variable
#'
#' @param dt Name of the tibble (data frame) object containing the variable
#' @param varname The categorical variable to be summarised
#' @param descending A boolean value denoting whether the results should be
#'   sorted in descending order by levels (default is TRUE)
#'
#' @details This function returns the number of observations in each level of a
#'   categorical variable. The levels are listed in descending frequency by 
#'   default.
#'
#' @export
#' @examples
#' sum_levels(this_tibble, "this_variable")

sum_levels <- function(dt, varname, descending = TRUE) {
  # TO-DO: Overload this function so that I can either supply the tibble name
  # and variable name separately (as above) or in tibble$variable format
  
  # Create a local function to calc percent
  percent_ <- function(x, y = sum(x), places = 1) round( (x / y) * 100, places)

  # Create the summary
  results <- group_by(dt, !!rlang::sym(varname)) %>% summarise(n = n())

  # If the user elects to have the levels sorted in descending frequency do so
  if (descending == TRUE) results <- arrange(results, desc(n))

  # Add the remaining info we desire and return the result
  add_stats <- mutate(results, cumulative = cumsum(n), percent = percent_(n),
    pct_cumulative = cumsum(percent))
  return(add_stats)
}
