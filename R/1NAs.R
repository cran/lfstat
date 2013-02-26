#Dealing with NAs

lfnacheck <- function(lfobj){
  lfcheck(lfobj)
  total <- sum(is.na(lfobj$flow))

  percentage <- total/length(lfobj$flow)

  year <- aggregate(is.na(flow)~hyear, data = lfobj, sum)

  dur <- rle(is.na(lfobj$flow))
  duration <- table(dur$length[dur$value])

  na <- list(total,percentage, year, duration)
  names(na) <- c("total","percentage", "hydrologicalyear", "duration")
  na}


#Checks for missing dates! Includes dates and sets NA as flow value!
lfcomplete <- function(lfobj){
dates <- as.Date(paste(lfobj$year,lfobj$month,lfobj$day,sep = "-"))

#Order if not ordered!
if(sum(diff(dates) < 0) > 0){
  lfobj <- lfobj[order(dates),]
  dates <- as.Date(paste(lfobj$year,lfobj$month,lfobj$day,sep = "-")) }

#Fill gaps:



}

lfnainterpolate <- function(lfobj){
  lfcheck(lfobj)
  if(sum(is.na(lfobj$flow) > 0)){
     lfobj$flow <-approx(1:length(lfobj$flow),lfobj$flow,n=length(lfobj$flow))$y
   }
  lfobj
}
