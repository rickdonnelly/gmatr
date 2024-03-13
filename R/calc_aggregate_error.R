#' Calculate aggregate comparison between two vectors
#'
#' @param df Name of the tibble (data frame) object containing vectors (columns)
#'   of observed and predicted values
#' @param observed A string identifying the column name of the observed values
#' @param predicted A string identifying the column name of the predicted values
#' @param places An integer denoting the number of places past the decimal that
#'.  floating point statistics will be reported with. The default value is 1.
#'
#' @details This function returns a table containing comparison of two summed
#'   vectors of data. The totals, the difference, and percent difference
#'   between them are reported instead of comparisons between individual
#'   observations within each vector. If the data are grouped these statistics
#'   will be compiled as expected with a row with totals added to the bottom.
#'
#' @export
#' @examples
#' calc_aggregate_error(assignment_results, "count", "assigned")
#' # You can group the data when passing it to this function:
#' calc_aggregate_error(group_by(assignment_results, functional_type),
#'   "count", "assigned")

calc_aggregate_error <- function(df, observed, predicted, places = 1) {
  # Define a percentage function
  percent_ <- function(x, y = sum(x), p) round((x / y) * 100, p)

  # Create the initial, and possibly only, summary
  initial_sum <- df %>%
    summarise(x0x_ = round(sum(!!rlang::sym(observed), na.rm = TRUE), places),
      y0y_ = round(sum(!!rlang::sym(predicted), na.rm = TRUE), places)) %>%
    mutate(error = y0y_ - x0x_, pct_error = percent_(error, x0x_, places))
  
  # Renaming the summarized vectors absolutely should not be this hard
  eh <- names(initial_sum)
  eh <- str_replace_all(eh, "x0x_", observed)     
  eh <- str_replace_all(eh, "y0y_", predicted)
  names(initial_sum) <- eh

  # If the input data are ungrouped we're done
  if (!is_grouped_df(df)) return(initial_sum)

  # But otherwise add a totals row to the bottoom
  total_sum <- ungroup(df) %>%
    summarise(x0x_ = round(sum(!!rlang::sym(observed), na.rm = TRUE), places),
      y0y_ = round(sum(!!rlang::sym(predicted), na.rm = TRUE), places)) %>%
    mutate(error = y0y_ - x0x_, pct_error = percent_(error, x0x_, places))
  
  # Yes, of course we must also change the variable names here as well
  ugh <- names(total_sum)
  ugh <- str_replace_all(ugh, "x0x_", observed)     
  ugh <- str_replace_all(ugh, "y0y_", predicted)
  names(total_sum) <- ugh

  # If the initial data had grouping variable(s) then they'd show up in the
  # column names for `initial_sum` but not for `total_sum`
  groupings <- setdiff(names(initial_sum), names(total_sum))
  if (length(groupings) > 0) total_sum[[groupings[[1]]]] <- "Total"
  total_sum <- bind_rows(initial_sum, total_sum)
  return(total_sum)
}   
