#' Create NCHRP highway validation plot
#'
#' @param df Name of the tibble (data frame) object containing the data to be 
#'   plotted
#' @param observed The variable in `df` containing the counted or observed
#'   flows to compare to
#' @param predicted The variable in `df` containing the simulated or 
#'   predicted flows
#' @param groupvar The variable in `df` to colour the observations by
#'   (often link type or functional classification)
#' @param plot_title A character string that corresponds to ggplot's title
#'   label, displayed left-justified at the top of the plot
#' @param max_pct_error Integer defining the maximum percent error that will
#'   be shown on the y-axis (see notes below about negative values are
#'   handled), default is 300 percent
#' @param show_AEC Boolean variable indicating whether the approximate error
#'   in count (AEC) is shown, defaults to TRUE
#'
#' @details This function creates a plot of percent error in traffic assignment
#'   by comparing observed and predicted values as shown in NCHRP Report 750.
#'   The percent error is shown on the `y` axis and the observed value on the
#'   `x` axis. The maximum desirable error (MDE) and approximate error in 
#'   count (AEC) lines are shown, as well as a report of how many observations
#'   fall within each set of lines. This plot differs from the NCHRP approach
#'   in that both positive and negative percent errors are shown, rather than
#'   just absolute values. The observations (points) can be coloured if the 
#'   user specifies a `groupvar`. If so, the number of observations are 
#'   reported in the legend beside each level of `groupvar`. The function
#'   returns a ggplot object that contains the plot with annotations.
#'
#'   The function normally plots observations with percent error ranging from 
#'   -150 to 300 percent. The positive value is set by the `max_pct_error`
#'   parameter. The plot is not symmetrical, but by definition no observation
#'   can have a negative percent error less than -100 percent. If you want to
#'   show all observations regardless of how compressed the space between the
#'   MDE lines is simply set `max_pct_error` to the largest percent error in
#'   your data. (Note that you cannot set `max_pct_error` to a huge number, as
#'   ggplot will use that as the maximum value on the y-axis.
#'
#'   The maximum desirable error (MDE) lines (positive and negative) are
#'   always shown, as that is a key part of the NCHRP display. The approximate
#'   error in count (AEC) is less often used. It is turned on by default but
#'   used a dotted line to distinguish it from MDE (solid line). The AEC line
#'   can be turned off by setting the `show_AEC` parameter to FALSE.
#'
#' @export
#' @examples
#' eh <- plot_NCHRP_comparison(df, "count", "assigned", "road_type")
#' print(eh)
#' pdf(filename, height = 8, width = 11); print(eh); dev.off()

plot_NCHRP_comparison <- function(df, observed, predicted, groupvar,
  plot_title = NULL, max_pct_error = 300, show_AEC = TRUE) {
  # Remove observations that do not have valid observed or predicted flows. We
  # will assume that a count must be a positive number to quality, and predicted
  # values must be zero or greater. Missing values are invalid in both cases.
  drop_missing <- filter(df, !is.na(.data[[observed]]), .data[[observed]] >= 1,
    !is.na(.data[[predicted]]), .data[[predicted]] >= 0)
  n_dropped <- nrow(df) - nrow(drop_missing)
  if (n_dropped > 0) {
    warning(n_dropped, " observations with invalid observed or predicted values dropped")
  }

  # Calculate the percent error
  add_error <- mutate(drop_missing, 
    pct_error = ((.data[[predicted]] - .data[[observed]]) / .data[[observed]]) * 100)

  # Determine the largest count volume, which will set limits on x axis plot
  # x dimensions
  largest_count <- max(add_error[[observed]])

  # Add labels for the grouping variable that show the number of observations
  add_labels <- gmatr::create_frequency_labels(add_error, {{groupvar}}) %>%
    left_join(add_error, ., by = {{groupvar}})
  new_groupvar <- names(add_labels)[length(names(add_labels))]

  # Determine how many values are within the MDE and AEC limits
  calc_mde <- function(x) -13.7721899 + (555.13816 * x^-0.26024293)
  calc_aec <- function(x) ((3.706633 / log(x)) - 0.264598) * 100
  add_bounds <- mutate(add_labels, this_mde = calc_mde(.data[[observed]]),
    within_mde = ifelse(abs(pct_error) <= this_mde, 1, 0),
    this_aec = calc_aec(.data[[observed]]),
    within_aec = ifelse(abs(pct_error) <= this_aec, 1, 0))
  pct_within_mde <- round((sum(add_bounds$within_mde) / nrow(add_bounds)) * 100, 1)
  pct_within_aec <- round((sum(add_bounds$within_aec) / nrow(add_bounds)) * 100, 1)

  # Count the number of observations that fall outside of the user-specified
  # threshold for y axis 
  beyond_limits <- nrow(filter(add_bounds, abs(pct_error) > max_pct_error))

  # Create text strings reporting the number of observations not plotted and
  # percent of observations within the MDE and AEC curves. Note that percent
  # error can be negative, so compare to absolute values.
  caption_text <- (paste0(pct_within_mde,
    "% of observations within MDE (solid line) and ", pct_within_aec,
    "% within AEC (dashed line)"))
  if (beyond_limits > 0) {
    caption_text <- paste0(caption_text, ", ", beyond_limits,
      " observations with percent error > ", max_pct_error, " not shown")
  }

  # Finally, create the plot object. We will need to create the MDE and AEC
  # lines on the fly
  mde_line <- tibble(x = seq(10, largest_count * 1.2, 100), y = calc_mde(x))
  plot_obj <- ggplot(add_bounds, aes(x = count, y = pct_error)) +
    geom_point(size = 1.0, aes(colour = .data[[new_groupvar]])) +
    ylim(max(-150, -max_pct_error), max_pct_error) +
    xlim(0, largest_count * 1.2) +
    geom_line(data = mde_line, aes(x = x, y = y), color="grey25", linewidth = 0.375) +
    geom_line(data = mde_line, aes(x = x, y = -y), color = "grey25", linewidth = 0.375) +
    labs(title = plot_title, y = "percent error", caption = caption_text,
      color = groupvar) +
    theme(plot.caption = element_text(hjust = 0))

  # Finally, add the AEC lines unless the user has asked us not to
  if (show_AEC) {
    aec_line <- tibble(x = mde_line$x, y = calc_aec(x))
    plot_obj <- plot_obj +
      geom_line(data = aec_line, aes(x = x, y = y), color="grey25",
        linewidth = 0.375, linetype = "dotted") +
      geom_line(data = aec_line, aes(x = x, y = -y), color = "grey25",
        linewidth = 0.375, linetype = "dotted")
  }

  # Return the plot object
  return(plot_obj)
}