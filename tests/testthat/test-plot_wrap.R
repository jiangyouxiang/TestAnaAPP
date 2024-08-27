library(testthat)
library(ggplot2)
test_that("Test ICC Plot", {
  theta <- seq(-3, 3, length.out = 100)
  y_matrix <- matrix(runif(100 * 4), ncol = 4)
  grade_vector <- c(2, 2)
  colnames(y_matrix) <- c("Item 1.P.0", "Item 1.P.1", "Item 2.P.0","Item 2.P.1")

  plot <- plot_wrap(
    theta = theta,
    y_matrix = y_matrix,
    lines = "ICC",
    is.include.zore = TRUE,
    grade_vector = grade_vector,
    main_vector = paste0("Item ", 1:2),
    title = "ICC Plot"
  )

  testthat::expect_s3_class(plot, "ggplot")
})

test_that("Test IIC Plot", {
  theta <- seq(-3, 3, length.out = 100)
  y_matrix <- matrix(runif(100 * 3), ncol = 3)
  main_vector <- c("Item 1", "Item 2", "Item 3")

  plot <- plot_wrap(
    theta = theta,
    y_matrix = y_matrix,
    lines = "IIC",
    main_vector = main_vector,
    title = "IIC Plot"
  )

  testthat::expect_s3_class(plot, "ggplot")
})

test_that("Test for Empty theta or y_matrix", {
  theta <- numeric(0)
  y_matrix <- matrix(numeric(0), ncol = 0)
  grade_vector <- c()
  main_vector <- c()

  testthat::expect_error(
    plot_wrap(
      theta = theta,
      y_matrix = y_matrix,
      lines = "ICC",
      grade_vector = grade_vector,
      main_vector = main_vector,
      title = "Empty Plot"
    ), "The length of theta or y_matrix is 0."
  )
})

