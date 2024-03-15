#' Tabulate typical summary statistics for continuous numeric variables
#'
#' @param df Name of the tibble (data frame) object containing the variable
#' @param var The numeric variable to be summarised
#' @param places An integer specifying the number of places past the decimals to
#'   report the summary statistics (defaults to 4)
#' @param na_rm A boolean variable specifying whether missing values will be
#'   ignored in the calculation of the summaries (defaults to TRUE)
#' @param max_print The maximum number of rows in the output (defaults to 99)
#'
#' @details This function returns a tibble with the number of observations, 
#'   missing values, mean, median, min and max values, and the standard
#'   deviation for a numeric variable. To summarise the variable by grouping
#'   variables simply pass the exogenously `group_by` tibble to the `df`
#'   argument. A row is added to the summary for all levels of the grouping
#'   variable(s) combined in such cases.
#'
#' @export
#' @examples
#' sum_levels(this_tibble, "this_variable")
#' sum_levels(group_by(this_tibble, group_var), "this_variable")

sum_numeric <- function(df, var, places = 4, na_rm = TRUE, max_print = 99) {
  # Calculate the stats of interest
  detailed_sum <- summarise(df, n = n(), missing = NA, 
    min = min(!!sym(var), na.rm = na_rm),
    mean = mean(!!sym(var), na.rm = na_rm),
    median = median(!!sym(var), na.rm = na_rm),
    max = max(!!sym(var), na.rm = na_rm),
    SD = sd(!!sym(var), na.rm = na_rm)
  )

  # If the user inadvertently passes a tibble with a huge number of levels in
  # the grouping variables they might get an unexpected result. 
  if (nrow(detailed_sum) > max_print) {
    complaint <- paste("The output summary has", nrow(detailed_sum),
      "rows: increase max_print to at least that limit")
    warning(complaint)
    return(NULL)
  }
  
  # If the data are ungrouped we can just return the first summary, as that's 
  # all there will be. But if the data are grouped we want a composite summary
  # at the end as well.
  if (is_grouped_df(df)) {
    add_total <- ungroup(df) %>%
      summarise(n = n(), missing = NA, 
        min = min(!!sym(var), na.rm = na_rm),
        mean = mean(!!sym(var), na.rm = na_rm),
        median = median(!!sym(var), na.rm = na_rm),
        max = max(!!sym(var), na.rm = na_rm),
        SD = sd(!!sym(var), na.rm = na_rm))
    groupings <- setdiff(names(detailed_sum), names(add_total))
    if (length(groupings) > 0) add_total[[groupings[[1]]]] <- "Total"
    detailed_sum <- bind_rows(detailed_sum, add_total)
  }

  # Return the result
  return(detailed_sum)
}

