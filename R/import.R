read.vardat2 <- function(file, check = TRUE) {
  infile <- readLines(file)

  # find fist line not starting with a hash
  line1 <- head(grep("^[^#]", infile), 1)

  # parse the header
  header <- head(infile, line1 - 1)
  version <- as.numeric(gsub("#", "", header[1], fixed = TRUE))
  if(check && version != 2) {
    stop("File ", shQuote(file), " not in vardat2 format. ", "Skipping it.")
    return()
  }

  # extract attributes
  dcha <- header[grep("^#DCHA", header)]
  dcha <- strsplit(dcha, " ", fixed = TRUE)[[1]][2]
  dcha <- as.numeric(strsplit(dcha, ".", fixed = TRUE)[[1]])
  names(dcha) <- c("regime", "station", "point", "quantity", "version")


  x <- read.table(text = tail(infile, -line1 + 1), header = FALSE,
                  comment.char = "#", colClasses = c("character", "numeric"),
                  na.strings = c("-9999.000000", "-9999"),
                  col.names = c("time", "discharge"))

  x$time <- as.POSIXct(x$time, format = "%Y%m%d/%H%M")

  attr(x, "meta") <- as.list(dcha)
  return(x)
}
