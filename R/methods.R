summary.lfobj <- function(object, digits = 4, ...){
  lfobj <- object
  lfcheck(lfobj)
  nr <- nrow(lfobj)

  rng <- time(lfobj[c(1, nr), ])
  nas <- sum(is.na(lfobj$flow))
  nyears <- round(as.double(diff(rng), units = "days") / 365, 1)

  out <- c("Meanflow" = meanflow(lfobj), "MAM7" = MAM(lfobj, n=7), Q95(lfobj))

  if("baseflow" %in% names(lfobj)) {
    out <- c(out, "BFI" = BFI(lfobj))
  }

  cat("Startdate: ", as.character(rng[1]), "    (calendar year)", fill = TRUE)
  cat("Enddate:   ", as.character(rng[2]), "    (calendar year)", fill = TRUE)

  if(nas) {
    perc <- round(nas / nr, 3) * 100
    perc <- if(perc < 0.01) "< 0.01" else perc
    nastring <- paste0(" and contains ", nas, " missing observations (",
                       perc, " %)", sep = "")
} else {
    nastring <- ""
  }

  cat("\n")
  cat("Time series covers ", nyears, " years", nastring, ".", sep = "",
      fill = TRUE)
  cat("The hydrological year is set to start on", month.name[hyear_start(lfobj)],
      "1st.", fill = TRUE)

  xx <- signif(out, digits = digits)

  cat("\n")
  print.table(xx, ...)
  invisible()
}


plot.lfobj <- function(x,...){
  hydrograph(x,...)
}
