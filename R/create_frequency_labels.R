#' Create frequency labels for a categorical variable
#'
#' @param df Name of the tibble (data frame) object containing the field to be 
#'   be summarised
#' @param varname The field containing a string variable for which the
#'   frequencies of its occurrence will be tabulated
#' @param suffix The suffix attached to `varname` with the frequency labels
#'   appended to it, defaults to "_lab"
#'
#' @details Categorical variables are often used for colouring or symbols in
#'   ggplot2 graphics. This function clones a categorical variable and adds
#'   the frequency in parentheses (n=x) after each value of `varname`. The
#'   resulting tibble retains `varname`, one record for each value, and a new
#'   variable that appends the user-supplied (or defaulted to "_lab") suffix
#'   to `varname`. This tibble can then be joined to the original `df` using 
#'   `varname` as the by variable. Then use the new variable as the grouping
#'   variable in ggplot (e.g., colour = varname_lab).
#'
#' @export
#' @examples
#' eh <- create_frequency_labels(df, "Type")
#' new_df <- left_join(df, eh, by = "Type")
#' ggplot(new_df, aes(colour = Type_lab, ...)


create_frequency_labels <- function(df, varname, suffix = "_lab") {
  # Create the output variable name
  add_suffix <- paste0(varname, suffix)
  
  # Sum the number of observations for each level of `varname` and create a
  # label that appends that to each level
  result <- group_by(df, !!rlang::sym(varname)) %>%
    summarise(n = n()) %>%
    #mutate(Type_label = paste0(Type, " (n=", n, ")"))
    mutate(!! add_suffix := paste0(!!rlang::sym(varname), " (n=", n, ")"))
  
  # If `varname` is a factor we want to retain the same ordering of levels. This
  # should be straightforward, as `result` should already be sorted in the 
  # original order of the factor `varname`:
  if (class(df[[varname]]) == "factor") {
    result[[add_suffix]] <- factor(result[[add_suffix]], levels = result[[add_suffix]])
  }
  
  # Drop the number of observations (intermediate result) and exit stage left
  result$n <- NULL
  return(result)
}