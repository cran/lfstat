context("tyears reproduces results of DWA Merkblatt")

fn <- system.file("samplesheets/donauwoerth.dat", package = "lfstat")
lfu <- readlfdata(fn, type = "LFU", baseflow = FALSE, hyearstart = 4)

lfdw <- lfu[lfu$hyear > 1959 & lfu$hyear < 2009, ]

# results calculated by lfstat
lfdw$flow <- ma(lfdw$flow, 7)
expect_warning(lfstat <- tyears(lfdw, event = c(2, 5, 10, 30, 50, 100),
                                dist="gevR", plot = FALSE))


test_that("estimated parameters of the distribution are correct", {
  expect_equal(object = round(lfstat$parameters[[1]], 2),
               expected = c(xi = -93.05, alpha = 17.73, k = 0.34))
})

test_that("estimated parameters of the distribution are correct", {
  expect_equal(object = round(lfstat$T_Years_Event[, "gevR"], 1),
               expected = c("2" = 86.9, "5" = 72.3, "10" = 65.3, "30" = 57.6,
                            "50" = 55.0, "100" = 52.2))
})



context("evfit reproduces fitting of durations as in DWA Merkblatt")

infile <- read.csv2("borstendorf.csv", header = TRUE,
                    colClasses = c("Date", "numeric"))

bdorf <- xts(x = infile$discharge, order.by = infile$time)
flowunit(bdorf) <- "m^3/s"
hyear_start(bdorf) <- 4

# find droughts und MA pooling
dr <- find_droughts(bdorf, threshold = 1.79) # MNQ aus der DWA S. 105
expect_warning(pooled <- pool_ma(dr, n = 7))

# partial series of durations
partial <- summary(pooled, drop_minor = FALSE)[,c("start", "volume", "duration")]
partial$hyear <- water_year(partial$start, assign = "start", origin = 4)

annual <- tapply(partial$duration, partial$hyear, max)
annual[is.na(annual)] <- 0
annual <- data.frame(time = as.numeric(rownames(annual)), duration = annual)


# fit a GEV distribution
expect_warning(fit <- evfit(annual$duration, "gev", extreme = "maximum"))


test_that("parameters are equal to DWA", {
  evp <- round(as.numeric(fit$parameters$gev), 4)
  expect_equal(evp, c(13.2302, 10.8314, -0.3732))
})


test_that("return periods are equal to DWA", {
  rp <- c(5, 10, 20, 50, 100)
  quant <- round(evquantile(fit, return.period = rp)$T_Years_Event[, "gev"], 1)

  expect_equal(unname(quant), c(17.2, 30.1, 45.4, 71.7, 98.1))
})
