#' Print tibble as a data frame
#'
#' @param df A tibble
#'
#' @details Another utility to thwart Hadley's obsession with reducing R
#'   functionality. This function prints a tibble as a data frame, allowing us
#'   to see contents of all the fields. Printing a tibble by default will result
#'   in only the first few variables being displayed rather than showing us all
#'   the variables.  
#'
#' @export
#' @examples
#' peek(df)   # Prints a useful preview of a tibble

peek <- function(df) print(as.data.frame(df))
