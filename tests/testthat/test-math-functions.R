context("test-math-functions")

test_that("cor.pTs works.", {

  # Test that errors are caught

  # input is not a single point
  point <- matrix(rnorm(100), ncol = 2)
  field <- point
  expect_error(cor.pTs(point, field))

  # input has no temporal overlap
  point <- pTs(rnorm(100), time = 1 : 100)
  field <- pField(1, time = 101 : 200, lat = 1, lon = 1 : 2)
  expect_error(cor.pTs(point, field))

  # Test output

  # input pField as field
  point <- pTs(1 : 5, time = 1 : 5)
  field <- pField(1 : 10, time = 1 : 5, lat = 1, lon = 1 : 2)
  output <- cor.pTs(point, field)
  expect_true(is.pField(output))
  expect_equal(dim(output), c(1, 2))
  expect_equal(c(output), rep(1, 2))

  # input pTs as field
  point <- pTs(1 : 5, time = 1 : 5)
  field <- pTs(matrix(1 : 10, nrow = 5), time = 1 : 5, lat = 1 : 2, lon = 1 : 2)
  output <- cor.pTs(point, field)
  expect_true(is.pTs(output))
  expect_equal(dim(output), c(1, 2))
  expect_equal(c(output), rep(1, 2))

  # input standard matrix as field
  point <- pTs(1 : 5, time = 1 : 5)
  field <- matrix(1 : 10, nrow = 5)
  output <- cor.pTs(point, field)
  expect_equal(dim(output), c(1, 2))
  expect_equal(c(output), rep(1, 2))

  # shorter common temporal overlap
  point <- pTs(1 : 5, time = 1 : 5)
  field <- pField(1 : 6, time = 2 : 4, lat = 1, lon = 1 : 2)
  expect_message(output <- cor.pTs(point, field, debug = TRUE))
  expect_equal(c(stats::time(output)), 4)
  
})
