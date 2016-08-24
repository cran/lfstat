# example from Prof. Ripley, mod. M Maechler
.peaks <- function (series, span = 3) {
  if ((span = as.integer(span))%%2 != 1) stop("'span' must be odd")

  s1 = 1:1 + (s = span%/%2)
  z = embed(series, span)
  v = apply(z[, s1] > z[, -s1, drop = FALSE], 1, all)

  pad = rep.int(FALSE, s)
  return(c(pad, v, pad))
}

# # only keep segments which fall below the threshold and have proper length
# .selectSegment <- function(x, threshold, nmin, nmax, tp.factor = 1) {
#   x <- x[as.vector(x < threshold), , drop = FALSE]
#   increasing <- as.vector(tail(x, -1)) * tp.factor > as.vector(head(x, -1))
#   decreases.until <- min(head(which(increasing), 1), length(increasing))
#   x <- head(x, decreases.until)
#   if(length(x) >= nmin) return(head(x, nmax))
# }


.splitAt <- function(x, pos) {
  unname(split(x, findInterval(seq_along(x), pos)))
}

.rsegment <- function(x, span = 5, smooth = 0) {
  # cut the series at the flood peaks
  y <- as.vector(x)
  idxPeak <- which(.peaks(y, span = span))
  seg <- .splitAt(y, idxPeak)

  # only keep limb if flow is decreasing
  lapply(seg, function(x){
    y <- na.exclude(x)

    # always keep first element
    inc <- c(FALSE, tail(y, -1) * (1 - smooth) > head(y, -1))

    if(any(is.na(x))) {
      # consider an NA as a decrease
      inc.padded  <- rep(F, length(x))
      inc.padded[-attr(y, "na.action")] <- inc
    } else {
      inc.padded <- inc
    }

    decreases.until <- min(head(which(inc.padded), 1) - 1, length(x))
    return(head(x, decreases.until))
  })
}

.plot_recession_segments <- function(segments) {
  ylim <- extendrange(unlist(segments))
  xlim <- c(0, max(sapply(segments, length)))

  plot(1, 1, xlim = xlim, ylim = ylim, type = "n")
  nul <- lapply(segments, lines, col = "lightgrey")
}

.sanititze_segment <- function(x, length = c(4, 7), drop.first = 2,
                              cut.at.NA = FALSE, threshold = NULL) {

  y <- tail(x, -drop.first)
  if(!is.null(threshold))   y <- y[y < threshold]


  if(cut.at.NA) y <- head(y, min(c(which(is.na(y)), length(y))))

  if(length(y) >= length[1]) return(head(y, length[2]))
}

.rpair <- function(x) cbind("t0" = head(x, -1), "t1" = tail(x, -1))


mrc <- function(x, span = 5, smooth = 0, length = c(4, 7), drop.first = 2,
                cut.at.NA = TRUE, threshold = NULL, plot = TRUE) {

  seg <- .rsegment(x = x, span = span, smooth = smooth)

  seg <- lapply(seg, .sanititze_segment, length = length, drop.first = drop.first,
                cut.at.NA = cut.at.NA, threshold = threshold)

  seg <- seg[!sapply(seg, is.null)]

  pair <- lapply(seg, .rpair)

  # combine all recession curves to one master recessinon curve
  pair <- do.call(rbind, pair)

  # fit model
  model <- lm(t1 ~ t0 + 0, as.data.frame(pair))

  if(plot) {
    plot(pair, asp = 1)
    abline(model, col = 2, lty = 2)
    abline(a = 0, b = 1, col = "lightgrey")
  }

  k <- unname(coef(model)["t0"])
  C <- -1/log(k)

  cat("k = ", k, fill = TRUE)
  cat("C = ", C, fill = TRUE)
  return(C)
}


irs <- function(x, span = 5, smooth = 0, length = c(4, 7), drop.first = 2,
                cut.at.NA = TRUE, threshold = NULL) {

  seg <- .rsegment(x = x, span = span, smooth = smooth)

  seg <- lapply(seg, .sanititze_segment, length = length, drop.first = drop.first,
                cut.at.NA = cut.at.NA, threshold = threshold)

  seg <- seg[!sapply(seg, is.null)]

  pair <- lapply(seg, .rpair)

  # fit a model for each recession curve
  model <- lapply(pair, function(x) lm(t1 ~ t0 + 0, as.data.frame(x)))

  # extract just the slope
  k <- sapply(model, function(x) unname(coef(x)["t0"]))

  # slopes have to be weighted by the length of the recession curves
  len <- sapply(seg, length)
  k <- weighted.mean(k, len)
  C <- -1/log(k)

  cat("k = ", k, fill = TRUE)
  cat("C = ", C, fill = TRUE)
  return(C)
}
