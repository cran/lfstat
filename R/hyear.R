# Let the hydrological year start at any month
# If startmonth is Jan-Jun -> hyear reduces to the previous year,
# else the month after startmonth move to the next year.
hyear <- function(dat, startmonth = 1){
  if(startmonth > 6.5){
    dat$hyear <- dat$year + (dat$month >= startmonth)
  } else {
    dat$hyear <- dat$year - (dat$month < startmonth)
  }
  dat
}


calendar_year <- function(x) {
  x <- as.Date(x)
  x <- as.numeric(format(x, "%Y"))
  return(factor(x, levels = seq(min(x), max(x))))
}



water_year <- function(x, origin = "din", as.POSIX = FALSE,
                       assign = c("majority", "start", "end"), ...) {

  assign <- match.arg(assign)
  x <- as.POSIXlt(x, ...)

  # there are multiple ways to specify the start of the hydrological year
  # translate all of them to an integer between {1..12}
  if (length(origin) != 1)
    stop("argument 'origin' must be of length 1.", call. = FALSE)

  if (is.numeric(origin) & origin %in% 1:12) {
    idx <- origin
  } else {

  # as.POSIXlt() converts integers to seconds since 1970
  # test for Date object as string
  idx <- tryCatch(as.POSIXlt(origin)$mon + 1,
                  error = function(x) suppressWarnings(as.numeric(origin)))
  }

  if(is.character(origin)) {
    origin <- gsub(".", "", tolower(origin), fixed = TRUE)

    # first try to match exactly against given definitions of popular institutions
    defs <- c("din" = 11, "usgs" = 10, "swiss" = 10, "glacier" =  9)
    if (origin %in% names(defs)) {
      idx <- as.numeric(defs[origin])
    } else {
      # English month names
      idx <- pmatch(origin, tolower(month.name))
      if (is.na(idx)) {
        # month names in current locale
        abbr.locale <- format(seq(from = as.Date("1992-01-01"), by = "months", length.out = 12), format("%B"))
        abbr.locale <- tolower(abbr.locale)
        idx <- pmatch(origin, abbr.locale)
      }
    }
  }

  if (is.na(idx)) {
    stop("argument 'origin' must be either one of ",
         paste(sQuote(names(defs)), collapse=", "),
         " or a (possibly abbreviated) name of a month,",
         " an integer between 1 and 12 or valid POSIX/Date object.")
  }

  origin <- idx

  # when extracting components of POSIXlt, the year gets counted from 1900
  # and for months Jan = 0, Dec = 11: +1 because we want integers {1..12}
  year <- x$year + 1900
  month <- x$mon + 1

  # The water year can be designated by the calendar year in which it ends or
  # in which it starts.
  # if not specified, the calendar year sharing the majority of months is taken
  if(assign == "majority") assign <- ifelse(origin > 6, "end", "start")
  offset <- if(assign == "start") 0 else 1
  y <- year - (month < origin) + offset

  if (as.POSIX) {
    y <- as.POSIXct(paste(y, origin, "01", sep = "-"))
  } else {
    # its convenient to have the water year as a factor, otherwise years without
    # observations don't appear after aggregation
    y <- factor(y, levels = seq(min(y), max(y)))
  }

  return(y)
}



"hyear_start<-" <- function(x, value) {
  UseMethod("hyear_start<-")
}

"hyear_start<-.lfobj" <- function(x, value) {
  attr(x, "lfobj")$hyearstart <- value
  time <- time(x)

  x$hyear <- as.numeric(as.character(water_year(x = time, origin = value)))
  return(x)
}

"hyear_start<-.xts" <- function(x, value) {
  # also support other inputs
  if(!value %in% 1:12) stop("must be an integer between 1 and 12.")
  xtsAttributes(x)$hyearstart <- value
  return(x)
}


hyear_start <- function(x, abbreviate = FALSE) {
  UseMethod("hyear_start")
}


hyear_start.data.frame <- function(x, abbreviate = FALSE){
  hy <- attr(x, "lfobj")$hyearstart
  if(is.null(hy) || (!hy %in% 1:12)) hy <- .guess_hyearstart(x)

  if(is.null(hy)) {
    warning("Couldn't determine start of hydrological year from attributes or columns.\nDefaulting to 'January'.",
            call. = FALSE)
    hy <- 1
  }

  if(abbreviate) hy <- month.abb[hy]
  return(hy)
}

hyear_start.xts <- function(x, abbreviate = FALSE){
  hy <- xtsAttributes(x)$hyearstart

  if(is.null(hy) || (!hy %in% 1:12)) {
    warning("Couldn't determine start of hydrological year from attributes.\nDefaulting to 'January'.",
            call. = FALSE)
    hy <- 1
  }

  if(abbreviate) hy <- month.abb[hy]
  return(hy)
}


.guess_hyearstart <- function(lfobj) {
  if(!"hyear" %in% names(lfobj)) {
    hyearstart <- NULL
  } else {
    ii <- subset(lfobj, year != hyear, month)
    if(nrow(ii) == 0){
      # year and hyear are identical
      hyearstart <- 1
    } else if(max(ii) < 5.5){
      hyearstart <- max(ii) + 1
    } else {
      hyearstart <- min(ii)
    }
  }

  return(hyearstart)
}


