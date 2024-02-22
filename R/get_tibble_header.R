#' Return a string with the dimensons of a tibble
#'
#' @param df Name of the tibble (data frame) object
#'
#' @details This function simply returns a string identifying the input tibble
#'   and its dimensions.
#'
#' @export
#' @examples
#' print(get_tibble_header(df))

get_tibble_header <- function(target) {
  # TO-DO: Check that `target` is indeed a tibble or data frame
  object_name <- deparse(substitute(target))
  return(paste0(object_name, ": ", nrow(target), " observations by ",
    length(names(target)), " columns"))
}
