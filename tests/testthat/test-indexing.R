
context("test-indexing")

test_that("pField indexing works correctly", {

  # Create a pField
  lat <- c(-75, -80)
  lon <- c(0, 135, 215)
  time <- 1 : 4
  space <- c(1, 1, 1, 2, 2, 2)
  spacetime <- c(space, 10 * space, 100 * space, 1000 * space)

  pfield <- pField(data = spacetime, lat = lat, lon = lon, time = time)

  # Test single indexing

  expect_error(subset <- pfield[1, ], NA)
  expect_true(is.pField(subset))

  expect_error(subset <- pfield[, 1], NA)
  expect_true(is.pTs(subset))

  expect_warning(subset <- pfield[10])
  expect_equal(c(subset), 10)

  expect_error(subset <- pfield[1, 1], NA)
  expect_true(is.pTs(subset))
  expect_equal(unname(c(subset)), 1)

  # Test 1D vector indexing

  expect_warning(subset <- pfield[1 : 10])

  expect_error(subset <- pfield[1 : 3, ], NA)
  expect_true(is.pField(subset))

  expect_error(subset <- pfield[, 1 : 3], NA)
  expect_true(is.pField(subset))

  expect_error(subset <- pfield[, 1 : 4], NA)
  expect_true(is.pTs(subset))

  # Test indexing by one vector and one integer

  expect_error(subset <- pfield[1, 1 : 3], NA)
  expect_true(is.pField(subset))

  expect_error(subset <- pfield[1, 1 : 4], NA)
  expect_true(is.pTs(subset))

  expect_error(subset <- pfield[1 : 3, 1], NA)
  expect_true(is.pTs(subset))

  # Test 2D vector indexing

  expect_error(subset <- pfield[1 : 3, 1 : 3], NA)
  expect_true(is.pField(subset))

  expect_error(subset <- pfield[1 : 3, 1 : 5], NA)
  expect_true(is.pTs(subset))

})
