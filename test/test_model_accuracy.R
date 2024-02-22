# Test the model accuracy calculator
require(tidyverse)
test_harness <- function() {
  # Define the test data
  test_data <- tibble(category = c(rep("fools", 5), rep("clueless", 5)),
    measured = c(79.6, 40.3, 36.1, 7.5, 96.1, 22.5, 93.2, 30.1, 82.5, 15),
    simulated = c(14.1, 0, 36.8, 12.1, 21, 25.6, 91.6, 43.9, 28.4, 28.9))

  # Test out the function using all observations and also feeding it grouped
  # data
  calc_model_accuracy(test_data, "measured", "simulated")
  calc_model_accuracy(group_by(test_data, category), "measured", "simulated", 0.1)

  # Mess with the data to ensure that it catches missing data
  test_data$measured[6] <- NA; test_data$simulated[8] <- NA
  calc_model_accuracy(test_data, "measured", "simulated", 0.1)

  # Disqualifying all of the data should stop the function
  test_data$simulated <- NA
  calc_model_accuracy(test_data, "measured", "simulated", 0.1)
}
