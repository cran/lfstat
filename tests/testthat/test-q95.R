context("Flow Quantiles")
data(ngaruroro)

nyear <- length(unique(ngaruroro$year))

test_that("Qxx works for all combinations of arguments", {

  # we do not check for correct results, just if dimensions are correct
  x <- Q95(ngaruroro)
  expect_equal(length(x), 1)

  x <- Q95(ngaruroro, breakdays = c("01/03", "01/06"))
  expect_equal(dim(x), c(2, 2))

  x <- Q95(ngaruroro, yearly = TRUE)
  expect_equal(dim(x), c(nyear, 2))

  x <- Q95(ngaruroro, year = 1970:1980, yearly = TRUE,
           breakdays = c("01/03", "01/06"))
  expect_equal(dim(x), c(11 * 2, 3))

  x <- Q95(ngaruroro, year = 1970:1975, yearly = TRUE, monthly = TRUE)
  expect_equal(dim(x), c(6 * 12, 3))

  x <- Q95(ngaruroro, monthly = TRUE)
  expect_equal(dim(x), c(12, 2))
})

