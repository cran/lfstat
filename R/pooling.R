
find_droughts <- function(x, threshold = vary_threshold, varying = "constant",
                          interval = "day", ...) {
  if(!inherits(x, "xts")) x <- as.xts(x)
  x <- .regularize(x, warn = TRUE, interval = interval)

  if(ncol(x) == 1) {
    discharge <- x[, 1]
    colnames(discharge) <- "discharge"
  } else {
    if(!"discharge" %in% names(x)) {
      stop("'x' must eihter be an object of class lfobj, an xts object with one column or with a column named 'discharge'.")
    }
    discharge <- x[, "discharge"]
  }

  missing <- sum(is.na(discharge$discharge))
  if(missing) {
    warning(round(missing / nrow(x) * 100, 1), "% of the discharges (", missing,
            " observations) are NA values. NAs always terminate a drought event.",
            call. = FALSE)
  }

  att <- xtsAttributes(x)
  # we want volumes always in qubic metre and times always in seconds
  f.vol <- .conv_factor(from = att$unit.parsed["volume"], to = "m",
                        dimension = "vol")
  f.time <- att$deltat * .conv_factor(from = att$unit.parsed["time"],
                                      to = "secs", dimension = "time")
  f <- f.time / f.vol

  names(discharge) <- "discharge"

  # if it is a function, apply it directly
  if(is.function(threshold)) {
    if("varying" %in% names(formals(threshold))) {
      threshold <- threshold(discharge, varying = varying, ...)
    } else {
      threshold <- threshold(discharge, ...)
    }
  } else {
    # shortcut to specify quantiles
    fun.quant <- try(.quant_character(threshold, ...))
    if(is.function(fun.quant)) {
      threshold <- vary_threshold(discharge$discharge, fun = fun.quant,
                                  varying = varying, ...)
    }
  }

  start <- 1
  event.no <- 0
  len <- nrow(x)

  x <- cbind(discharge = discharge,
             threshold  = threshold,
             def.increase = as.numeric(threshold - coredata(discharge)) * f,
             event.no = numeric(len))

  is.deficit <- as.numeric(x$def.increase) >= 0

  # group events
  if(any(na.omit(is.deficit))) {
    x$event.no <- group(is.deficit, new.group.na = TRUE)

    # only deficit events get an id, when discharge > threshold: id <- 0
    x$event.no[is.deficit] <- as.numeric(factor(x$event.no[is.deficit]))
    x$event.no[!is.deficit | is.na(x$def.increase)] <- 0
  }

  class(x) <- unique(c("deficit", class(x)))
  return(x)
}

pool_it <- function(x, tmin = 5) {
  pool_ic(x, tmin = tmin, ratio = Inf)
}

pool_ic <- function(x, tmin = 5, ratio = 0.1) {
  tab <- summary(x, drop_minor = 0)

  x$event.orig <- x$event.no
  if(nrow(tab)) {

    # inter event time and volume
    ti <- as.numeric(difftime(tail(tab$start, -1), head(tab$end, -1),
                              units = "days")) - 1

    if (ratio != Inf) {
      vi <- numeric(length(nrow(tab) - 1))
      for(i in seq_len(nrow(tab) - 1)) {
        start <- tab$end  [i]   + as.difftime(1, units = "days")
        end   <- tab$start[i+1] - as.difftime(1, units = "days")
        vi[i] <- sum(x[paste(start, end, sep = "/"), "def.increase"], na.rm = TRUE)
      }
    } else {
      vi <- rep(-1, nrow(tab) -1)
    }

    tab$vol.pooled <- tab$volume
    event <- c(1, 2)

    repeat {
      # check if events need to become poooled
      interEventTime <- ti[event[2] - 1]
      ratioVol <- abs(vi[event[2] - 1] / tab$vol.pooled[event[2] - 1])

      if(interEventTime <= tmin && ratioVol < ratio){
        # pooling needed
        tab$event.no[event[2]] <- event[1]
        tab$vol.pooled[event[2]] <- sum(tab$vol.pooled[event[2] - 0:1]) + vi[event[2] - 1]

        # merge the events
        rng <- with(tab, paste(end[event[2] - 1], end[event[2]], sep = "/"))
        x$event.no[rng] <- tab$event.no[event[1]]

        event[2] <- event[2] + 1
      } else {
        # no pooling for current event needed, restart with the next two events
        event <- event[2] + c(0, 1)
      }

      if(event[2] > nrow(tab)) break
    }
  }

  n.pooled <- length(unique(x$event.orig)) - length(unique(x$event.no))

  xtsAttributes(x) <- c(xtsAttributes(x), list(pooling = "IC",
                                               n.pooled = n.pooled))
  return(x)
}


pool_ma <- function(x, n = 10) {

  x$discharge <- ma(x$discharge, n = n, sides = 2)

  y <- find_droughts(x$discharge, threshold = x$threshold)
  y$event.orig <- x$event.no

  n.pooled <- length(unique(y$event.orig)) - length(unique(y$event.no))
  xtsAttributes(y) <- c(xtsAttributes(y), list(pooling = "Moving Average",
                                               n.pooled = n.pooled))

  return(y)
}

pool_sp <- function(x) {
  # ignores precomupted event numbers
  x$event.orig <- x$event.no
  x$event.no <- 0

  len <- nrow(x)
  start <- 1
  event.no <- 0
  repeat {
    # find start of drought, first day with a positive increase of the deficit
    start <- start + which(x$def.increase[start:len] > 0)[1] - 1
    if(is.na(start)) break

    # can't use head() because n = -0 returns an empty object
    deficit <- cumsum(x$def.increase[start:len])

    # NAs start a new event
    deficit$def.increase[is.na(deficit$def.increase)] <- -Inf

    # range of the event
    # Tallaksen excluded the day of zero-crossing, maybe we keep it?
    duration <- which(deficit < 0)[1] - 1

    if(is.na(duration)) {
      # no end of deficit event found
      warning("Time series ended before recovery of drought. (event number: ", event.no + 1, ")")
      duration <- len - start + 1
    }
    rng <- seq(start, start + duration - 1)

    event.no <- event.no + 1
    x$event.no[rng] <- event.no

    start <- start + duration
    if(start > len) break
  }

  n.pooled <- length(unique(x$event.orig)) - length(unique(x$event.no))
  xtsAttributes(x) <- c(xtsAttributes(x), list(pooling = "Sequent Peak",
                                               n.pooled = n.pooled))
  return(x)
}


summarize.drought <- function(x, drop_minor = c("volume" = 0, "duration" = 0),
                              poolMethod = c("event", "peak")) {
  poolMethod <- match.arg(poolMethod)


  time.ind <- time(x)


  # for Sequent Peak algorithm
  # drought duration is defined as time until maximum depletion

  if (poolMethod == "peak") {
    def.vol <- cumsum(as.numeric(x$def.increase)) #* 86400
    duration <- which.max(def.vol)
    def.vol <- def.vol[duration]
    # time <- time.ind[duration]
  } else {
    # NA values can be present in input time series
    # pool_it() can merge two events seperated just by an NA
    # x$def.increase[is.na(x$def.increase < 0] <- 0
    def.vol <- sum(as.numeric(x$def.increase)) #* 86400
    duration <- length(time.ind)
  }

  below <- as.numeric(x$def.increase) >= 0

  y <- data.frame(event.no = coredata(x$event.no)[1],
                  start = time.ind[1],
                  time = time.ind[duration],
                  end = tail(time.ind, 1),
                  # todo: inform user about how the drought event was terminated
                  # terminated by: NA, end of record, discharge
                  volume = def.vol,
                  duration = duration,
                  dbt = sum(below),
                  vbt = sum(as.numeric(x$def.increase)[below]),
                  qmin = min(x$discharge, na.rm = TRUE),
                  tqmin = time.ind[which.min(x$discharge)])

  # neglect minor events
  if (is.finite(def.vol) && (nrow(x) == 0 ||
                             (drop_minor["volume"] != 0 & def.vol < drop_minor["volume"]) ||
                             (drop_minor["duration"] != 0 & duration < drop_minor["duration"])) )  {
    y <- y[numeric(), ]
  }

  # drop column 'time' unless sequent peak is used
  # if (poolMethod != "peak")  y <- y[,setdiff(names(y), "time")]

  return(y)
}



.parse_minor_arg <- function(arg, x) {
  if(length(arg) == 1 && isFALSE(arg)) return(c("volume" = 0, "duration" = 0))

  if(any(grepl("%", arg, fixed = TRUE))) {
    x <- summary(x, drop_minor = c("volume" = 0, "duration" = 0))
    if(nrow(x) == 0) x <- data.frame("volume" = 0, "duration" = 0)
  }

  f <- function(val, sample) {
    if(grepl("%", val, fixed = TRUE)) {
      val <- as.numeric(sub("%", "", val, fixed = TRUE))
      if (val < 0 || val > 100) stop("fraction must be between 0% and 100%.")
      # sample can be a single NA if time series has only one drought event
      # which contains NAs
      if (length(na.omit(sample)) == 0) {
        y <- Inf
      } else {
        y <- max(sample, na.rm = TRUE) * val / 100
      }
    } else {
      y <- as.numeric(val)
    }

    return(y)
  }

  if (length(arg) == 1){
    if (arg == 0) {
      arg <- c("volume" = 0, "duration" = 0)
    } else {
      stop("if of length one, argument 'drop_minor' has to be zero.")
    }
  } else if (length(arg) == 2){
    if(is.null(names(arg))) {
      names(arg) <- c("volume", "duration")
    } else {
      pos <- charmatch(names(arg), c("volume", "duration"))
      if (any(is.na(pos) | pos == 0)) {
        stop("names 'volume' and 'duration' not found in argument 'drop_minor'")
      } else {
        arg <- arg[pos]
        names(arg) <- c("volume", "duration")
      }
    }
  } else {
    stop("argument 'drop_minor' has to be of length one or two.")
  }

  res <- numeric(length(arg))
  names(res) <- names(arg)
  for(i in seq_along(arg)) {
    res[i] <- f(arg[i], sample = x[, names(arg[i])])
  }

  return(res)
}


summary.deficit <- function(object,
                            drop_minor = c("volume" = "0.5%", "duration" = 5),
                            ...) {

  drop_minor <- .parse_minor_arg(drop_minor, object)

  x <- object[object$event.no > 0, ]
  if(nrow(x) > 0) {

    pooling <- xtsAttributes(object)$pooling
    poolMethod <- if(!is.null(pooling) && pooling == "Sequent Peak") "peak" else "event"

    y <- lapply(split(x, x$event.no), summarize.drought, drop_minor = drop_minor,
                poolMethod = poolMethod)
    omitted <- sum(sapply(y, function(x) nrow(x) == 0))
    total <- length(y)

    y <- do.call(rbind, y)
  } else {
    y <- summarize.drought(object[1, ])
    total <- 0
    omitted <- 0
  }


  class(y) <- c("summaryDrought", class(y))
  attr(y, "deficit") <- c(xtsAttributes(x),
                          list(drop_minor = drop_minor, omitted = omitted,
                               total = total))
  return(y)
}

print.summaryDrought <- function(x, ...) {
  attlist <- attr(x, "deficit")
  cat("Summary of", if (!is.null(attlist$pooling)) "pooled", "droughts")

  if(!is.null(attlist)) {
    if (!is.null(attlist$river)) cat("\nRiver:", attlist$river)
    if (!is.null(attlist$station)) cat(" at", attlist$station)
    if (!is.null(attlist$pooling)) cat("\nPooling:", attlist$pooling)
    if (!is.null(attlist$n.pooled)) cat(", ", attlist$n.pooled, " were pooled", sep = "")

    cat("\nUnits: volumes in m\u00B3, duration in days")

    if(attlist$omitted) {
      with(attlist, cat("\n\nFiltered", omitted, "minor events of", total , "total.",
                        "\nOnly droughts with volume >=", drop_minor["volume"],
                        "m\u00B3 and duration >=", drop_minor["duration"], "days are",
                        "reported."))
    }
  }

  cat("\n\n")

  NextMethod(x)
}


print.deficit <- function(x, ...) {
  # pool oder event.no
  cat("streamflow defict", fill = TRUE)
  cat("data.frame with", nrow(x), "observations and",
      length(unique(x$event.no)), "events\n\n")

  NextMethod(x)
}

plot.deficit <- function(x, type = "dygraph", ...) {
  arg <- list(...)
  if (type == "dygraph") {
    do.call(plot.deficit_dygraph, c(list(x = x), arg))
  } else {
    attlist <- xtsAttributes(x)
    river <- .char2html(attlist$river)
    station <- .char2html(attlist$station)

    title <- paste(if(length(river)) paste("River", river),
                   if(length(river) & length(station)) "at" else "",
                   if(length(station)) paste("station", station))

    do.call(plot.xts, c(list(x = x$discharge, type = type, main = title), arg))
    do.call(lines, c(list(x = x$threshold, col = 2), arg))
  }
}


plot.deficit_dygraph <- function(x, ...) {
  arg <- list(...)
  if("step" %in% names(arg)) step <- arg$step else step = TRUE
  if("log" %in% names(arg)) log <- arg$log else log = FALSE

  is.drought  <- x$event.no != 0
  x$lwr <- x$upr <- x$discharge

  # expand xts object with cols upr and lwr, indicating deficits
  x$lwr[is.drought] <- with(x[is.drought], ifelse(threshold >= discharge, discharge, threshold))
  x$upr[is.drought] <- with(x[is.drought], ifelse(threshold <= discharge, discharge, threshold))

  attlist <- xtsAttributes(x)
  river <- .char2html(attlist$river)
  station <- .char2html(attlist$station)

  title <- paste(if(length(river)) paste("River", river),
                 if(length(river) & length(station)) "at" else "",
                 if(length(station)) paste("station", station))
  ylab <- .char2html(paste("Flow in", attlist$unit))
  p <- dygraph(x[, c("discharge", "lwr", "threshold", "upr")],
               main = title, ylab = ylab) %>%
    dyRangeSelector() %>%
    dySeries("discharge", stepPlot = step, drawPoints = TRUE, color = "darkblue") %>%
    dySeries(c("lwr", "threshold", "upr"), stepPlot = step, color = "red",
             strokePattern = "dashed") %>%
    dyAxis("y", logscale = log)

  tbl <- summary(x, ...)

  if(length(tbl)) {
    ttip <- with(tbl,
                 paste0("volume: ", signif(volume, 3), "\n",
                        "duration: ", duration, " days\n",
                        "intensity: ", signif(volume/duration, 3)))


    for (i in seq_len(nrow(tbl))) {
      p <- dyShading(p, from = tbl$start[i], to = tbl$end[i] + 1, color = "lightgrey")
      p <- dyAnnotation(p, x = round(mean(c(tbl$start[i], tbl$end[i] + 1))),
                        text = tbl$event.no[i],
                        tooltip = ttip[i], width = 30)
    }
  }
  return(p)
}

