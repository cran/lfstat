context("lfobj to xts conversion")

test_that("as.xts.lfobj() method work", {
  data("ngaruroro")
  expect_s3_class(ngaruroro, "lfobj")

  ng <- as.xts(ngaruroro)
  expect_s3_class(ng, "xts")


})

test_that("tyears and find_droughts() handle irregular time series correctly", {
  data("ngaruroro")

  ngaruroroSummer <- subset(ngaruroro, month %in% 6:8)

  # for extreme value statistics, we allow for irregular time series
  expect_silent(tyears(ngaruroroSummer, dist = "wei", plot = FALSE))

  # deficit statistics require a regular time series
  expect_warning(find_droughts(ngaruroroSummer),
                 "NAs always terminate a drought event.")


})
