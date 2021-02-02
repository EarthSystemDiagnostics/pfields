context("test-apply-functions")

test_that("ApplySpace works.", {

  # Test that errors are caught

  # input is not pfield nor pts
  input <- rnorm(100)
  expect_error(output <- ApplySpace(input, sd))

  # input is no matrix
  input <- pTs(input, time = 1 : length(input))
  expect_error(output <- ApplySpace(input, sd))

  # input has only one column
  input <- rnorm(100)
  field <- pField(input, time = 1 : length(input), lat = 1, lon = 1)
  expect_error(output <- ApplySpace(field, sd))

  # input is only NA
  input <- rep(NA, 200)
  field <- pField(input, time = 1 : (length(input) / 2), lat = 1, lon = 1 : 2)
  expect_error(output <- ApplySpace(field, sd))

  # input has only one non-NA column
  input <- matrix(c(1 : 100, rep(NA, 100)), ncol = 100, byrow = TRUE)
  field <- pField(input, time = 1 : (length(input) / 2), lat = 1, lon = 1 : 2)
  expect_warning(output <- ApplySpace(field, sd))
  expect_equal(as.numeric(output), input[1, ])

  # Test output

  # using deprecated function name
  field <- pField(1, time = 1 : 100, lat = 1, lon = 1 : 2)
  expect_warning(applyspace(field, sd))

  # input pField
  field <- pField(1, time = 1 : 100, lat = 1, lon = 1 : 2)
  output <- ApplySpace(field, sd)
  expect_true(is.pTs(output))
  expect_equal(as.numeric(output), rep(0, 100))

  # input pTs
  input <- matrix(rep(0, 200), nrow = 100)
  field <- pTs(input, time = 1 : 100)
  output <- ApplySpace(field, sd)
  expect_true(is.pTs(output))
  expect_equal(as.numeric(output), rep(0, 100))
  
})

test_that("ApplyTime works.", {

  # Test that errors are caught

  # input is not pfield nor pts
  input <- rnorm(100)
  expect_error(output <- ApplyTime(input, sd))

  # input is no matrix
  input <- pTs(input, time = 1 : length(input))
  expect_error(output <- ApplyTime(input, sd))

  # input has only one time step
  input <- rnorm(100)
  field <- pField(input, time = 1, lat = 1, lon = 1 : 100)
  expect_error(output <- ApplyTime(field, sd))

  # result has >1 time step but no new time axis supplied
  field <- pField(1, time = 1 : 100, lat = 1, lon = 1 : 5)
  expect_error(output <- ApplyTime(field, range))
  expect_error(output <- ApplyTime(field, range, newtime = c(1, 2)), NA)

  # Test output

  # using deprecated function name
  field <- pField(1, time = 1 : 100, lat = 1, lon = 1 : 5)
  expect_warning(applytime(field, sd))

  # input pField
  field <- pField(1, time = 1 : 100, lat = 1, lon = 1 : 5)
  output <- ApplyTime(field, sd)
  expect_true(is.pField(output))
  expect_equal(as.numeric(output), rep(0, 5))

  # input pTs
  input <- matrix(rep(0, 100), nrow = 20)
  field <- pTs(input, time = 1 : 20)
  output <- ApplyTime(field, sd)
  expect_true(is.pTs(output))
  expect_equal(as.numeric(output), rep(0, 5))

})

test_that("ApplyFields works.", {

  # Test that errors are caught

  # input is not pField
  expect_error(ApplyFields(1 : 10, 1 : 10, FUN = cor))
  expect_error(ApplyFields(1 : 10,
                           pField(1 : 10, time = 1 : 10, lat = 1, lon = 1),
                           FUN = cor))

  # input has different dimensions
  fld1 <- pField(1 : 10, time = 1 : 10, lat = 1, lon = 1)
  fld2 <- pField(1 : 10, time = 1 : 5, lat = 1, lon = 1 : 2)
  expect_error(ApplyFields(fld1, fld2, FUN = cor))

  # input has different observation times
  fld1 <- pField(1 : 10, time = 1 : 10, lat = 1, lon = 1)
  fld2 <- pField(1 : 10, time = 11 : 20, lat = 1, lon = 1)
  expect_error(ApplyFields(fld1, fld2, FUN = cor))

  # input has different longitudes
  fld1 <- pField(1 : 10, time = 1 : 10, lat = 1, lon = 1)
  fld2 <- pField(1 : 10, time = 1 : 10, lat = 1, lon = 2)
  expect_error(ApplyFields(fld1, fld2, FUN = cor))

  # input has different latitudes
  fld1 <- pField(1 : 10, time = 1 : 10, lat = 1, lon = 1)
  fld2 <- pField(1 : 10, time = 1 : 10, lat = 2, lon = 2)
  expect_error(ApplyFields(fld1, fld2, FUN = cor))

  # Test output

  fld1 <- pField(1 : 10, time = 1 : 5, lat = 1, lon = 1 : 2)
  fld2 <- pField(1 : 10, time = 1 : 5, lat = 1, lon = 1 : 2)
  output <- ApplyFields(fld1, fld2, FUN = cor)
  expect_true(is.pField(output))
  expect_equal(dim(output), c(1, 2))
  expect_equal(c(output), rep(1, 2))

})
