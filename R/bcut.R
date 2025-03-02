#' Cut a vector into intervals with sensible and flexible labels
#'
#' @param x A numeric vector to be sliced into intervals
#' @param min The minimum value to include in the intervals (defaults to zero)
#' @param stepsize The size of each interval (defaults to 1000)
#' @param labels A string that defines the type of labels will be returned
#'   (defaults to `upper`)
#'
#' @details This is a "better cut" (bcut) wrapper for the base R cut() function,
#'   which returns factors that describe the intervals that the values of the
#'   input vector `x` fall into. They're ugly although notationally accurate,
#'   but we often want to replace them with better, and usually shorter, labels.
#'   The format of the returned labels are specified in the `labels` parameter.
#'   If `labels=midranges` the labels will be the midrange value for the
#'   intervals that `x` is sliced into. If `labels=upper` only the upper end of
#'   the range for each interval will be returned. If `labels=ranges` the label
#'   will include both the lower and upper end of the interval, separated by a 
#'   hyphen. The function intentionally does not cast the returned values as 
#'   factors, leaving that to the user if desired.
#'
#' @export
#' @examples
#' results$interval <- bcut(results$count, labels = "midpoints")


bcut <- function(x, min = 0, stepsize = 1e4, labels = "upper") {
  # Make sure we're on same wavelength with the user
  if (!tolower(labels) %in% c("midpoints", "ranges", "upper")) {
    stop("Value of labels parameter = ", labels, " not valid")
    return()
  }
  
  # Cut the data. A bug in cut() prevents it from processing the `labels` para-
  # meter correctly, so assume we will always want the default (TRUE)
  x_ <- cut(x, seq(min, max(x) + stepsize, stepsize)) 

  # Whether we want midpoints or sensible range labels we'll first need to
  # deconstruct the labels that cut() produced
  tags <- tibble(ax = levels(x_), bx = str_extract(ax, pattern = "\\d.*\\d")) %>%
    separate(bx, c("lower", "upper"), sep = ',') %>%
    mutate(lower = as.numeric(lower), upper = as.numeric(upper))

  # Decide how to finish this based upon the user's label specification. I could
  # do this in case_when but I think this is clearer
  if (labels == "midpoints") {
    tags$label <- tags$lower + ((tags$upper - tags$lower) / 2)
  } else if (labels == "upper") {
    tags$label <- tags$upper
  } else {  # We must want revised intervals
    tags$label <- paste0(tags$lower + 1, '-', tags$upper)
  }
  
  # Now we need to join the updated labels to the cut data and return the 
  # results
  relabel <- tibble(ax = x_) %>% left_join(., tags, by = "ax")
  return(relabel$label)
}
