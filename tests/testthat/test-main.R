# tests/testthat/test-main.R
library(testthat)
library(shiny)

test_that("data loading works correctly", {
  # Test CSV loading
  test_csv <- data.frame(
    a = 1:5,
    b = letters[1:5],
    stringsAsFactors = FALSE
  )
  write.csv(test_csv, "test.csv", row.names = FALSE)
  loaded_data <- load_data("test.csv", "csv")
  expect_equal(nrow(loaded_data), 5)
  expect_equal(ncol(loaded_data), 2)
  unlink("test.csv")
  
  # Test error handling for invalid file
  expect_error(load_data("nonexistent.csv", "csv"))
})

test_that("weight computation works", {
  test_data <- data.frame(
    x = rnorm(100),
    y = factor(sample(letters[1:3], 100, replace = TRUE))
  )
  
  params <- list(
    method = "post_strat",
    strata_vars = "y",
    target_props = data.frame(
      y = letters[1:3],
      prop = c(0.3, 0.3, 0.4)
    )
  )
  
  weights <- compute_weights(test_data, params)
  expect_equal(length(weights), 100)
  expect_true(all(weights > 0))
  expect_equal(sum(weights), 100)
})

test_that("parameter validation works", {
  valid_params <- list(
    max_iterations = 100,
    convergence_threshold = 1e-6,
    method = "post_strat",
    strata_vars = c("var1")
  )
  
  invalid_params <- list(
    max_iterations = -1,
    convergence_threshold = 0,
    method = "invalid_method"
  )
  
  expect_true(validate_parameters(valid_params, "post_strat")$valid)
  expect_false(validate_parameters(invalid_params, "post_strat")$valid)
})

test_that("visualization functions work", {
  test_weights <- rnorm(100, mean = 1, sd = 0.1)
  test_convergence <- data.frame(
    iteration = 1:10,
    error = exp(-0.5 * (1:10))
  )
  
  expect_error(create_weight_distribution_plot(test_weights), NA)
  expect_error(create_convergence_plot(test_convergence), NA)
})

test_that("export functions work", {
  test_data <- data.frame(
    x = 1:5,
    y = letters[1:5]
  )
  test_weights <- rep(1, 5)
  test_diagnostics <- list(
    method = "post_strat",
    iterations = 10,
    final_error = 1e-6
  )
  
  # Test CSV export
  expect_error(
    export_results(test_data, test_weights, test_diagnostics,
                  "csv", TRUE, "test_export.csv"),
    NA
  )
  expect_true(file.exists("test_export.csv"))
  unlink("test_export.csv")
  
  # Test Excel export
  expect_error(
    export_results(test_data, test_weights, test_diagnostics,
                  "xlsx", TRUE, "test_export.xlsx"),
    NA
  )
  expect_true(file.exists("test_export.xlsx"))
  unlink("test_export.xlsx")
})