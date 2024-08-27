# Create a test file named test-wrightMap_new.R in the tests/testthat/ directory.

library(testthat)
library(ggplot2)
library(cowplot)

test_that("wrightMap_new handles Numeric point labels correctly", {
  person <- rnorm(1000)
  thresholds <- matrix(runif(30, -2, 2), nrow = 10, ncol = 3)
  colnames(thresholds) <- c("Step 1", "Step 2", "Step 3")
  rownames(thresholds) <- paste0("Item ", 1:10)

  plot <- wrightMap_new(person, thresholds, point_label = "Numeric", points_size = 3, p_width = 1.5)

  testthat::expect_s3_class(plot, "gg")
})

test_that("wrightMap_new handles Column names point labels correctly", {
  person <- rnorm(1000)
  thresholds <- matrix(runif(30, -2, 2), nrow = 10, ncol = 3)
  colnames(thresholds) <- c("Step 1", "Step 2", "Step 3")
  rownames(thresholds) <- paste0("Item ", 1:10)

  plot <- wrightMap_new(person, thresholds, point_label = "Column names", points_size = 3, p_width = 1.5)

  testthat::expect_s3_class(plot, "gg")
})

test_that("wrightMap_new handles single threshold column", {
  person <- rnorm(1000)
  thresholds <- matrix(runif(10, -2, 2), nrow = 10, ncol = 1)
  rownames(thresholds) <- paste0("Item ", 1:10)

  plot <- wrightMap_new(person, thresholds, point_label = "Column names", points_size = 3, p_width = 1.5)

  testthat::expect_s3_class(plot, "gg")
})

test_that("wrightMap_new respects plot dimensions", {
  person <- rnorm(1000)
  thresholds <- matrix(runif(30, -2, 2), nrow = 10, ncol = 3)
  colnames(thresholds) <- c("Step 1", "Step 2", "Step 3")
  rownames(thresholds) <- paste0("Item ", 1:10)

  plot <- wrightMap_new(person, thresholds, point_label = "Numeric", points_size = 3, p_width = 2)

  testthat::expect_s3_class(plot, "gg")
})

test_that("wrightMap_new handles empty thresholds", {
  person <- rnorm(1000)
  thresholds <- matrix(numeric(0), nrow = 0, ncol = 0)

  testthat::expect_error(
    wrightMap_new(person, thresholds, point_label = "Numeric", points_size = 3, p_width = 1.5),
    "The length of thresholds is 0."
  )
})
