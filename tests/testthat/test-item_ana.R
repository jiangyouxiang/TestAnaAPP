test_that("Basic Functionality - Binary Items", {
  data <-  data.frame(
    Item1 = c(rep(1,50), rep(0,50)),
    Item2 = c(rep(0,50), rep(1,50)),
    Item3 = c(rep(1,50), rep(1,50))
  )

  result <- item_ana(data)

  # Expected values for "Difficult" column (simple check)
  testthat::expect_equal(round(result["Item1", "Difficult"], 2), 0.5)
  testthat::expect_equal(round(result["Item2", "Difficult"], 2), 0.5)

  # Expected structure
  testthat::expect_equal(ncol(result), 4)
  testthat::expect_equal(nrow(result), 3)
  testthat::expect_equal(colnames(result), c("Difficult","Discrimination","Coefficient of variation", "Item-total correlation"))
})

test_that("Handling Missing Data (NA values)", {
  data <- data.frame(
    Item1 = c(1, 0, 1, NA, 1),
    Item2 = c(NA, 1, 0, 1, 0),
    Item3 = c(1, 1, 1, 0, NA)
  )

  testthat::expect_warning(result <- item_ana(data), "The case with NA were deleted.")

  # Check that the resulting matrix has correct dimensions after NA removal
  testthat::expect_equal(ncol(result), 4)
  testthat::expect_equal(nrow(result), 3)
})

test_that("Edge Case - Single Item", {
  data <- data.frame(
    Item1 = c(1, 0, 1, 1, 0)
  )

  result <- item_ana(data)

  testthat::expect_equal(ncol(result), 4)
  testthat::expect_equal(nrow(result), 1)
  testthat::expect_equal(rownames(result), "Item1")
})

test_that("Multi-Level Scoring Items", {
  data <- data.frame(
    Item1 = c(1, 2, 3, 1, 2),
    Item2 = c(0, 1, 0, 2, 1),
    Item3 = c(3, 2, 1, 2, 3)
  )

  result <- item_ana(data)

  # Check if the calculation for multi-level items is done correctly
  testthat::expect_true(all(result[, "Difficult"] <= 1))
  testthat::expect_true(all(result[, "Difficult"] >= 0))
})
