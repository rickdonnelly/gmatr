#' Verify that full_join has observations from both input tibbles
#'
#' @param left_df Tibble with one or more columns in common with `right_df`
#' @param right_df Tibble with one of more columns in common with `left_db`
#' @param by_var A string containing the name(s) of column(s) to join the two
#'   two tibbles by
#'
#' @details This function does a full merge on two tibbles, ensuring that all
#'   records from both tibbles are included. It then ensures that all records
#'   are present in both datasets, in which case the joined tibble is returned
#'   to the calling program. However, if one or more records are present in
#'   one but not the other the mismatches are displayed and the function
#'   returns an empty tibble.
#'
#' @export
#' @examples
#' verify_full_join(assigned, counts, by = "link_ID")


verified_full_join <- function(left_df, right_df, by_var) {
  # Check that there are no observations in the left tibble that do not exist in 
  # the right tibble, and vice versa. Start by finding at least one variable 
  # (column) in each tibble that does not exist in the other.
  left_only <- setdiff(names(left_df), names(right_df))
  if (length(left_only) == 0) {
    stop('No variables in left_df that are unique to that tibble')
  } else {
    left_only = left_only[1]
  }
  
  # Repeat for the right tibble
  right_only <- setdiff(names(right_df), names(left_df))
  if (length(right_only) == 0) {
    stop('No variables in right_df that are unique to that tibble')
  } else {
    right_only = right_only[1]
  }
  
  # Now that we've identified columns (variables) unique to each tibble we can
  # join them and detect cases where an observation only in appears in one but
  # not the other
  combined <- full_join(left_df, right_df, by = by_var) %>%
    mutate(status = case_when(is.na(left_only) ~ "Only in left_df",
      is.na(right_only) ~ "Only in right_df", TRUE ~ "Matches both"))
  
  # Detect errant observations, and return empty tibble if we find them. 
  # Otherwise return the verified join
  problem_children <- filter(combined, status != "Matches both")
  n_problem_children <- nrow(problem_children)
  if (n_problem_children > 0) {
    print(paste(n_problem_children, "records did not match:"), quote = FALSE)
    print(problem_children)
    return(tibble())
  } else {
    return(select(combined, -status))
  }
}
