#' Create two-dimensional matrix of grouped frequencies or weighted sums
#'
#' @param df Name of the tibble (data frame) object containing the data to be 
#'   summarised
#' @param group_vars A list of two or more variables that will be used to 
#'   group the data by
#' @param descending A boolean variable to indicate whether the resulting
#'   tibble will be sorted in descending order by row sum (defaults to TRUE)
#' @param weight_var An optional variable number to weight the observations
#'   by when constructing the frequency matrix (defaults to NULL for no 
#'   weighting
#'
#' @details This function creates two-dimensional matrix of frequencies or
#'   weighted frequencies using two or more grouping variables, with the row
#'   and column totals and percentages appended. It is important to note that
#'   all of the `group_var`s must be strings to avoid their values being
#'   unwittingly included in the row and column totals. If using a grouping 
#'   variable with numeric values cast it to character before passing it to 
#'   this function. 
#'
#' @export
#' @examples
#' eh <- gmatr::freq_matrix(survey_data, c("hhsize", "income"))


freq_matrix <- function(df, group_vars, descending = TRUE, weight_var = NULL) {
  # I want to first count or sum the data using the user-specified grouping and
  # optional weighting variable
  step_1 <- group_by(eh, across(all_of(group_vars)))
  if (!is.null(weight_var)) {
    step_2 <- reframe(step_1, zed = sum(freq, na.rm = TRUE))
  } else {
    step_2 <- reframe(step_1, zed = n())
  }
  
  # Now I want to rotate step 2 so that we have a frequency matrix. I will use the
  # second to last variable as the new columns and the last column as the values
  row_vars <- tail(group_vars, 1)  # Drop last variable, which become columns
  step_3 <- step_2 %>%
    pivot_wider(names_from = all_of(row_vars), values_from = zed, values_fill = 0) %>%
    mutate(total = rowSums(across(where(is.numeric)), na.rm = TRUE), 
      percent = round((total / sum(total)) * 100, 1))
  
  # If the user has asked to sort the columns in descending order by total do so
  if (descending == TRUE) step_3 <- arrange(step_3, desc(total))
  
  # Next create the column totals
  step_4 <- summarise(step_3, across(where(is.numeric), ~ sum(.x, na.rm = TRUE))) %>%
    mutate(!!sym(group_vars[1]) := "total")
  
  # Creating the percentages is a pain in the gluteus maximus, as it requires us
  # to go through several convolutions 
  step_5 <- select(step_4, -total, -percent, -!!sym(group_vars[1])) %>%
    gather(my_columns, my_values) %>%
    mutate(percent = ((my_values / sum(my_values)) * 100)) %>%
    select(-my_values) %>%
    spread(my_columns, percent) %>%
    mutate(total = rowSums(across(where(is.numeric)), na.rm = TRUE),
      !!sym(group_vars[1]) := "percent")
  
  # Finally, wrap it all together
  result <- bind_rows(step_3, step_4, step_5)
  return(result)
}
