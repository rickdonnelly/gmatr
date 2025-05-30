% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/peek.R
\name{peek}
\alias{peek}
\title{Print tibble as a data frame}
\usage{
peek(df)
}
\arguments{
\item{df}{A tibble}
}
\description{
Print tibble as a data frame
}
\details{
Another utility to thwart Hadley's obsession with reducing R
  functionality. This function prints a tibble as a data frame, allowing us
  to see contents of all the fields. Printing a tibble by default will result
  in only the first few variables being displayed rather than showing us all
  the variables.
}
\examples{
peek(df)   # Prints a useful preview of a tibble
}
