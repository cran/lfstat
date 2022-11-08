test_that("water years are assigned correctly", {
  # generating monthly sequence
  x <- seq(from = as.Date("1992-01-01"),
           by = "months", length.out = 12)

  # specifying the beginning with a decimal number
  expect_equal(
    object = water_year(x, origin = 10),
    expected = factor(rep(c(1992, 1993),  times = c(9, 3)))
  )

  # using a month name, can be abbreviated (case insensitive)
  expect_equal(
    object = water_year(x, origin = "Jul"),
    expected = factor(rep(c(1992, 1993),  times = c(6, 6)))
  )

  expect_equal(
    object = water_year(x, origin = "July"),
    expected = factor(rep(c(1992, 1993),  times = c(6, 6)))
  )

  expect_equal(
    object = water_year(x, origin = "jUL"),
    expected = factor(rep(c(1992, 1993),  times = c(6, 6)))
  )

  # using an POSIX or Date object (only month is taken)
  expect_equal(
    object = water_year(x, origin = as.Date("2012-08-22")),
    expected = factor(rep(c(1992, 1993),  times = c(7, 5)))
  )
  expect_equal(
    object = water_year(x, origin = as.POSIXct("2012-08-22")),
    expected = factor(rep(c(1992, 1993),  times = c(7, 5)))
  )

  # or by specifying an institution
  expect_equal(
    object = water_year(x, origin = "usgs"),
    expected = factor(rep(c(1992, 1993),  times = c(9, 3)))
  )
})
