readlfdata <- function(file, type = c("GRDC","HZB","LFU","TU"),
                       lfobj = TRUE, readmeta = TRUE, encoding = NULL, ...){
  type <- match.arg(type)
  meta <- list()

  a <- switch(type,
              "HZB"  = read.hzb(file, readmeta = readmeta),
              "GRDC" = read.grdc2(file, encoding = if(is.null(encoding)) "cp1252" else encoding),
              "LFU"  = read.lfu2(file, encoding = if(is.null(encoding)) "cp1252" else encoding),
              "TU"   = read.tu(file))

  dat <- data.frame(day =  as.numeric(format(a[,1], "%d")),
                    month =  as.numeric(format(a[,1], "%m")),
                    year =  as.numeric(format(a[,1], "%Y")),
                    flow = a[,3])

  if(lfobj){
    lfobj <- createlfobj(dat, meta = attr(a, "meta"), ...)
    return(lfobj)
  } else {
    return(dat)
  }
}



# exists also in readhyd, but more elegantly...
read.hzb <- function(file, readmeta = TRUE) {
  lines <- readLines(file, n=50,encoding = "latin1")
  wert <- grep("Werte:",lines)
  a <- read.table(file,header = FALSE,
                  skip = wert,
                  na.strings = iconv("L\374cke",from = "latin1",to = "latin1"),
                  encoding = "latin1")
  a[, 1] <- as.Date(a[,1],"%d.%m.%Y")

  if(readmeta){
    fluss <- grep(iconv("Gew\344sser:",from = "latin1", to = "latin1"),lines)
    river <- tail(strsplit(lines[fluss],split = "  ")[[1]],1)
    hzb <- grep("HZB-Nummer:", lines)
    hzbnummer <- as.numeric(tail(strsplit(lines[hzb],split = "  ")[[1]],1))
    mess <-  grep("Messstelle:",lines)
    messst <- tail(strsplit(lines[mess],split = "  ")[[1]],1)
    einheit <- grep("Einheit:", lines)
    unit <- tail(strsplit(lines[einheit],split = " ")[[1]],1)
    geo <- grep("Exportzeitreihe:", lines) -1
    geoline <- strsplit(lines[geo], split = "  ")[[1]]
    subgeo <- geoline[geoline!=""]
    lon <- gsub(" ","",subgeo[2])
    lat <- gsub(" ","",subgeo[3])
    hline <- grep("Geographische Koordinaten", lines) -1
    highline <- strsplit(lines[hline], split = "  ")[[1]]
    height <- as.numeric(highline[highline!=""][2])
    ezg <- grep("orogr.Einzugsgebiet", lines)
    ezgr <- as.numeric(tail(strsplit(lines[ezg], split = " ")[[1]],1))



    attr(a, "meta") <- list(ID = hzbnummer, station = messst, river = river,
                            catchmentsize = ezgr, unit = unit,
                            coord = list(long = lon, lat = lat,
                                         altitude = height, proj = "Bessel 1841"))
  }


  return(a)
}

read.lfu2 <- function(...) {
  x <- read.lfu(...)
  x[, 3] <- x$flow

  return(x)
}

read.grdc2 <- function(...) {
  x <- read.grdc(...)
  x[, 3] <- x$original

  return(x)
}

read.tu <- function(...) {
  b <- read.table(..., header = FALSE)

  a1 <- as.Date(paste(b[, 3], b[, 2], b[, 1], sep = "/"), "%Y/%m/%d")
  a2 <- b[, 4]
  a3 <- a2
  a3[a2 == -999] <- NA

  return(data.frame(a1, a2, a3))
}

