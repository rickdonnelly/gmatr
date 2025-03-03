#' Create NCHRP screenline comparison plot
#'
#' @param df Name of the tibble (data frame) object containing the data to be 
#'   plotted
#' @param observed The variable in `df` containing the counted or observed
#'   flows to compare to
#' @param predicted The variable in `df` containing the simulated or 
#'   predicted flows
#' @param screenline An integer identifier for the screenline that the 
#'   observations are associated with
#' @param plot_title A character string that corresponds to ggplot's title
#'   label, displayed left-justified at the top of the plot
#' @param max_pct_error Integer defining the maximum percent error that will
#'   be shown on the y-axis (see notes below about negative values are
#'   handled), default is 300 percent
#'
#' @details This function creates a plot of percent error on screenlines by 
#'   comparing observed and predicted values as shown in Figure A-9 in NCHRP
#'   Report 255. The percent error is shown on the `y` axis and the observed 
#'   values on the `x` axis. The maximum desirable error (MDE) bars are shown,
#'   as well as a report of how many observations fall between the bounds.
#'   This plot differs from the NCHRP approach in that both positive and
#'   negative percent errors are shown, rather than just absolute values. The
#'   function returns a ggplot object that contains the plot with annotations.
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
#' @export
#' @examples
#' eh <- plot_NCHRP_screenlines(df, "count", "assigned", "screen_id")
#' print(eh)
#' pdf(filename, height = 8, width = 11); print(eh); dev.off()

plot_NCHRP_screenlines <- function(df, observed, predicted, screenline,
  plot_title = NULL, max_pct_error = 300) {
  # Remove observations that do not have valid observed or predicted flows. We
  # will assume that a count must be a positive number to quality, and predicted
  # values must be zero or greater. Missing values are invalid in both cases.
  drop_missing <- filter(df, !is.na(.data[[observed]]), .data[[observed]] >= 1,
    !is.na(.data[[predicted]]), .data[[predicted]] >= 0)
  n_dropped <- nrow(df) - nrow(drop_missing)
  if (n_dropped > 0) {
    warning(n_dropped, " observations with invalid observed or predicted values dropped")
  }

  # Next jettison otherwise valid observations that are not associated with a
  # screenline and then add the error calculations
  add_error <- drop_missing %>%
    filter(!is.na(.data[[screenline]]), .data[[screenline]] > 0) %>%
    group_by(.data[[screenline]]) %>%
    summarise(assigned = sum(.data[[predicted]]), count = sum(.data[[observed]])) %>%
    mutate(pct_error = ((assigned - count) / count) * 100)
  
  # Determine the largest count volume, which will set limits on x axis plot
  # x dimensions
  largest_count <- max(add_error$count)

  # The function that fits the MDE line is a bit more complicated than for links
  calc_mde = function(x) sqrt(-232.45703 + (263568.71 / x^0.5))
  add_bounds <- mutate(add_error, this_mde = calc_mde(count),
    within_mde = ifelse(abs(pct_error) <= this_mde, 1, 0))
  pct_within_mde <- round((sum(add_bounds$within_mde) / nrow(add_bounds)) * 100, 1)

  # Count the number of observations that fall outside of the user-specified
  # threshold for y axis 
  beyond_limits <- nrow(filter(add_bounds, abs(pct_error) > max_pct_error))

  # Create text strings reporting the number of observations not plotted and
  # percent of observations within the MDE and AEC curves. Note that percent
  # error can be negative, so compare to absolute values.
  caption_text <- (paste0(pct_within_mde, "% of screenlines within MDE bounds"))
  if (beyond_limits > 0) {
    caption_text <- paste0(caption_text, ", ", beyond_limits,
      " screenlines with percent error > ", max_pct_error, " not shown")
  }
  
  # I'd like to label the biggest outliers
  add_outlier_labels <- add_bounds %>%
    arrange(desc(abs(pct_error))) %>%
    mutate(seq = row_number(), outlier_label = ifelse(seq <= 10, screenline, NA))

  # Finally, create the plot object
  mde_line <- tibble(x = seq(10, largest_count * 1.2, 100), y = calc_mde(x))
  plot_obj <- ggplot(add_outlier_labels, aes(x = count, y = pct_error)) +
    geom_point(size = 1.0) +
    ylim(max(-150, -max_pct_error), max_pct_error) +
    xlim(0, largest_count * 1.2) +
    geom_text(aes(label = outlier_label), size = 2.5, vjust = 1.8, colour = "red") +
    geom_line(data = mde_line, aes(x = x, y = y), color="grey25", linewidth = 0.375) +
    geom_line(data = mde_line, aes(x = x, y = -y), color = "grey25", linewidth = 0.375) +
    labs(title = plot_title, y = "percent error", caption = caption_text) +
    theme(plot.caption = element_text(hjust = 0))

  # Return the plot object
  return(plot_obj)
}