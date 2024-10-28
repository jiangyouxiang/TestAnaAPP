test_that("Basic Functionality - Binary Items", {
  data <-  data.frame(
    Item1 = c(rep(1,50), rep(0,50)),
    Item2 = c(rep(0,50), rep(1,50)),
    Item3 = c(rep(1,50), rep(1,50))
  )

  result <- item_ana(data)

  # Expected values for "Difficult" column (simple check)
  testthat::expect_equal(round(result["Item1", "Difficulty"], 2), 0.5)
  testthat::expect_equal(round(result["Item2", "Difficulty"], 2), 0.5)


  # Expected structure
  testthat::expect_equal(ncol(result), 4)
  testthat::expect_equal(nrow(result), 3)
  testthat::expect_equal(colnames(result), c("Difficulty","Discrimination","Coefficient of variation", "Item-total correlation"))
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
  testthat::expect_true(all(result[, "Difficulty"] <= 1))
  testthat::expect_true(all(result[, "Difficulty"] >= 0))
})
