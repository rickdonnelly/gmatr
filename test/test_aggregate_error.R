require(tidyverse)
test_aggregate_error <- function() {
  # Define the test data
  test_data <- tibble(category = c(rep("fools", 5), rep("clueless", 5)),
  measured = c(79.6, 40.3, 36.1, 7.5, 96.1, 22.5, 93.2, 30.1, 82.5, 15),
  simulated = c(14.1, 0, 36.8, 12.1, 21, 25.6, 91.6, 43.9, 28.4, 28.9))

  # Make a function call
  calc_aggregate_error(test_data, "measured", "simulated")
  calc_aggregate_error(group_by(test_data, category), "measured",
    "simulated")
}