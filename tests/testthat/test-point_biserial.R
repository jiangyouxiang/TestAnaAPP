# Create a test file named test-point_biserial.R in the tests/testthat/ directory.

test_that("Basic Functionality", {
  binary_item <- c(1, 0, 1, 1, 0)
  total_score <- c(10, 8, 9, 10, 7)

  result <- point_biserial(binary_item, total_score)

  # Manual calculation of expected result:
  mean_1 <- mean(total_score[binary_item == 1])
  mean_0 <- mean(total_score[binary_item == 0])
  p <- mean(binary_item)
  q <- 1 - p
  sd_total <- sd(total_score)

  expected_result <- ((mean_1 - mean_0) * sqrt(p * q)) / sd_total

  testthat::expect_equal(result, expected_result, tolerance = 1e-6)
})

test_that("Edge Case with All Ones", {
  binary_item <- c(1, 1, 1, 1, 1)
  total_score <- c(10, 8, 9, 10, 7)

  result <- point_biserial(binary_item, total_score)

  testthat::expect_true(is.na(result))  # Should return NA as there is no variability in binary_item
})

test_that("Edge Case with All Zeros", {
  binary_item <- c(0, 0, 0, 0, 0)
  total_score <- c(10, 8, 9, 10, 7)

  result <- point_biserial(binary_item, total_score)

  testthat::expect_true(is.na(result))  # Should return NA as there is no variability in binary_item
})

test_that("Different Lengths and Variability", {
  binary_item <- c(1, 0, 1, 1, 0, 1, 0, 1, 0, 1)
  total_score <- c(10, 8, 9, 10, 7, 10, 6, 9, 5, 8)

  result <- point_biserial(binary_item, total_score)

  mean_1 <- mean(total_score[binary_item == 1])
  mean_0 <- mean(total_score[binary_item == 0])
  p <- mean(binary_item)
  q <- 1 - p
  sd_total <- sd(total_score)

  expected_result <- ((mean_1 - mean_0) * sqrt(p * q)) / sd_total

  testthat::expect_equal(result, expected_result, tolerance = 1e-6)
})

test_that("Consistency Check", {
  binary_item <- c(0, 1, 0, 1, 0, 1)
  total_score <- c(8, 9, 7, 10, 6, 8)

  result1 <- point_biserial(binary_item, total_score)

  # Reverse the binary item (1s to 0s and 0s to 1s) and reverse total_score
  result2 <- point_biserial(1 - binary_item, total_score)

  testthat::expect_equal(result1, -result2, tolerance = 1e-6)  # Should be the negative of the original
})
