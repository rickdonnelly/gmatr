#' Convert R matrix to tibble
#'
#' @param matrix A matrix in R format with row and column names defined
#' @param value_name A user-defined column name for the elements of the input
#'   matrix, defaults to `value`
#'
#' @details This is Greg Macfarlane's incredibly fast and efficient code for
#'   transforming a R matrix to tibble in i, j, value format. You can chain a
#'   rename() to the return value to rename the row and column names from 
#'   `origin` and `destination` to whatever names you'd like. 
#'
#' @export
#' @examples
#' eh <- gather_matrix(my_matrix, "distance_km")
#' neh <- gather_matrix(my_matrix) %>% rename(key = origin, y = destination)

gather_matrix <- function(matrix, value_name = "value") {
  list(
    # set matrix row and column names if they exist
    origin = rownames(matrix)[row(matrix)] %||% row(matrix),
    destination = colnames(matrix)[col(matrix)] %||% col(matrix),
    value = matrix
  ) %>%
    purrr::map_dfc(as.vector) %>%
    dplyr::rename(!!value_name := value)
}
