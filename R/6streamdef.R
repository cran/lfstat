#Threshold Ideen:
#Threshold - Jahresvektor an Funktion übergeben
#Threshold - Breaktage übergeben (z.b. 15. Mai - 15. Oktober)


#DOTO: Startdate und Enddate übergeben! UNIT!!!

streamdef <- function(lfobj,
                      pooling = c("none", "MA", "IT","IC"),
                      threslevel = 70,
                      thresbreaks = c("fixed","monthly","daily","seasonal"),
                      breakdays = c("01/06","01/10"),
                      MAdays = 7,
                      tmin =5,
                      IClevel = .1,
                      mindur = 0, #min days for IT/IC
                      minvol = 0, #min volume for IT/IC
                      table = c("all","volmax","durmax"),
                      na.rm = TRUE
                      ){
  lfcheck(lfobj)
  pooling <- match.arg(pooling)
  thresbreaks <- match.arg(thresbreaks)
  table <- match.arg(table)

threshold <- buildthres(lfobj = lfobj, threslevel = threslevel, thresbreaks=thresbreaks, breakdays = breakdays,na.rm = na.rm)

#Calculation FLOW
  if(pooling == "MA"){
    ma <- function(x,n){a<-filter(x,rep(1/n,n),
                                  sides=2,
                                  circular = FALSE)
                        a[is.na(a)]<-x[is.na(a)]
                        a} #to avoid NA's -> first and last values are not averaged
  }
  
  flow <- switch(pooling,
                 "none" = lfobj$flow,
                 "IT" = lfobj$flow,
                 "IC" = lfobj$flow,
                 "MA" = as.vector(ma(x = lfobj$flow,n = MAdays)))

  #Comparing FLOW and THRESHOLD
  check <- NULL
  temp <- merge(x=lfobj, y=threshold, by = c("day", "month"),sort = FALSE)
  temp <- temp[order(temp$year,temp$month,temp$day),]  
  defrun <-  rle(flow <= temp$flow.y)
  pos <- c(cumsum(c(1,defrun$lengths)))
  streamdef <- data.frame(matrix(ncol = 9))
  names(streamdef) <-  c("pos","d","v","mi","Qmin","startyear","startmonth", "startday","hyear")
  a <-  pos[defrun$values]
  a <- a[!is.na(a)]
  # a <- a[-length(a)] # switched off (GL)
  d <- defrun$lengths[defrun$values]
  d <- d[!is.na(d)]
  def <- flow - temp$flow.y
  for(ii in seq_along(a)){
    streamdef[ii,1] <- a[ii]
    streamdef[ii,6] <- lfobj[a[ii],"year"]
    streamdef[ii,7] <- lfobj[a[ii],"month"]
    streamdef[ii,8] <- lfobj[a[ii],"day"]
    streamdef[ii,9] <- lfobj[a[ii],"hyear"]
    streamdef[ii,2] <- d[ii]
    streamdef[ii,3] <- sum(def[a[ii]:(a[ii]+d[ii]-1)])
                                        #  streamdef[ii,4] <- streamdef[ii,3]/d[ii]
                                        #  streamdef[ii,5] <- min(flow[a[ii]:(a[ii]+d[ii]-1)])
  }

  if(pooling == "IT"){
    for(ii in nrow(streamdef):2){
      if(streamdef[ii,"pos"]-streamdef[ii-1,"d"]-tmin < streamdef[ii-1,"pos"]){
        streamdef[ii-1,"d"] <- streamdef[ii,"d"]+streamdef[ii-1,"d"]
        streamdef[ii-1,"v"] <- streamdef[ii,"v"]+streamdef[ii-1,"v"]
        streamdef <- streamdef[-ii,]}
    }
  }

  if(pooling == "IC"){
    for(ii in nrow(streamdef):2){
      if(streamdef[ii,"pos"]-streamdef[ii-1,"d"]-tmin < streamdef[ii-1,"pos"]){
        s <- sum((def[(streamdef[ii-1,"pos"]+streamdef[ii-1,"d"]):(streamdef[ii,"pos"]-1)]))
        if(-s/streamdef[ii-1,"v"]<IClevel){
          streamdef[ii-1,"d"] <- streamdef[ii,"pos"]-streamdef[ii-1,"pos"]+streamdef[ii,"d"]
          streamdef <- streamdef[-ii,]}}
    }
  }

  for(ii in seq_along(streamdef$pos)){
    if(pooling == "IC"){
      streamdef[ii,3] <- sum(def[streamdef[ii,"pos"]:(streamdef[ii,"pos"]+streamdef[ii,"d"]-1)])}
    streamdef[ii,4] <- streamdef[ii,"v"]/streamdef[ii,"d"]
    streamdef[ii,5] <- min(flow[streamdef[ii,"pos"]:(streamdef[ii,"pos"]+streamdef[ii,"d"]-1)])
  }

                                        #excluding minor deficits in IC/IT methods
  if(pooling %in% c("IC","IT")){
    streamdef <- streamdef[streamdef$d >mindur & streamdef$v < -minvol,]
  }

streamdef$v <- -streamdef$v*60**2*24
streamdef$mi <- -streamdef$mi*60**2*24
if(table == "all") {
  return(streamdef[,-c(1,9)])
}else{
  if(table == "volmax"){
  agg <- aggregate(v ~ hyear, streamdef,max)
  lin <- NULL
  for(ii in seq_along(agg$hyear)){
    lin[ii] <- min(which(streamdef$hyear == agg$hyear[ii] & streamdef$v == agg$v[ii]))
  }
  streamdef[lin,-c(1)]
}else{
  agg <- aggregate(d ~ hyear, streamdef,max)
    lin <- NULL
  for(ii in seq_along(agg$hyear)){
    lin[ii] <- min(which(streamdef$hyear == agg$hyear[ii] & streamdef$d == agg$d[ii]))
  }
  streamdef[lin,-c(1)]
}


  
}}
#gives data.frame: pos in lfobj, length of lf, flow deficit 

#########################
#Streamflow plot        #
#########################

streamdefplot <- function(lfobj, year,threslevel = 70,
                          thresbreaks = c("fixed","monthly","daily","seasonal"),
                          breakdays = c("01/06","01/10")){
   if(!year %in% c(min(lfobj$hyear):max(lfobj$hyear),"any")){
    stop("'year must be within the range of your data or \"any\" for taking whole series")}

thresbreaks <- match.arg(thresbreaks)
threshold <- buildthres(lfobj=lfobj,
                        threslevel = threslevel,
                        thresbreaks = thresbreaks,
                        breakdays = breakdays)
dummi <- year
sublf <- subset(lfobj, hyear == dummi)
full <- merge(sublf, threshold, by = c("day","month"),sort = FALSE)

plot(x=1:length(full$flow.x),
     y=full$flow.x,
     type = "n",
     xaxt = "n",
     xaxs = "i",
     xlab = "",
     ylab = lflabel("Flow"))

#Higher resolution to reduce gaps when crossing the threshold   
if(thresbreaks != "daily"){
approx.x <- approx(1:length(full$flow.x), full$flow.x,xout = seq(1,length(full$flow.x),by = 0.05))
#approx.y <- approx(1:length(full$flow.y), full$flow.y,xout = seq(1,length(full$flow.x),by = 0.05))
approx.y2 <- approx(1:length(full$flow.y), full$flow.y,xout = seq(1,length(full$flow.x),by = 0.05),method = "constant")
level <- apply(cbind(approx.x$y,approx.y2$y),1,min)   

 
polygon(c(approx.x$x,rev(approx.y2$x)),c(level,rev(approx.y2$y)),col = "lightblue")#,border = NA)
points(1:length(full$flow.y),full$flow.y,type = "s", col = "red")
}else{
approx.x <- approx(1:length(full$flow.x), full$flow.x,xout = seq(1,length(full$flow.x),by = 0.05))
approx.y <- approx(1:length(full$flow.y), full$flow.y,xout = seq(1,length(full$flow.x),by = 0.05))  
level <- apply(cbind(approx.x$y,approx.y$y),1,min)
polygon(c(approx.x$x,rev(approx.y$x)),c(level,rev(approx.y$y)),col = "lightblue")#,border = NA)
points(1:length(full$flow.y),full$flow.y,type = "l", col = "red")
}
points(x=1:length(full$flow.x), y=full$flow.x, type = "l")
   
months <- which(lfobj$day[lfobj$hyear == year] == 1)
    monthsex <-  which(subset(lfobj,hyear == dummi,month)[months[1],] ==lfobj$month & lfobj$day == 1 & lfobj$hyear == (year +1))
    lab <-rbind(subset(x = lfobj,
                       subset = hyear == dummi,
                       select = c(month,year))[months,],
                c(lfobj$month[monthsex],lfobj$year[monthsex]))
    months <- c(months,366)
    label <- paste(lab$month,lab$year,sep = "/")
    while(length(label) < length(months)){
      label <- c(label,"")}
    axis(1, at = months, labels = label)
}


#BUILDING threshold
   buildthres <- function(lfobj,
                          threslevel = 70,
                          thresbreaks = c("fixed","monthly","daily","seasonal"),
                          breakdays = c("01/06","01/10"),
                          na.rm = TRUE){
   thresbreaks <- match.arg(thresbreaks)
   threshold <- data.frame(day = rep(1:31,12),month = sort(rep(1:12,31)))

  if(thresbreaks == "fixed")
    {threshold$flow <- quantile(lfobj$flow,1-threslevel/100,na.rm = na.rm)}
  if(thresbreaks == "monthly"){
     mon <- aggregate(flow~month,lfobj,quantile,probs = 1-threslevel/100)
     threshold <- merge(threshold,mon,sort = FALSE)
     }
   if(thresbreaks == "daily"){
      mon <- aggregate(flow~day + month,lfobj,quantile,probs = 1-threslevel/100)
      threshold <- merge(threshold,mon, sort = FALSE)
    }
  if(thresbreaks == "seasonal"){
    ii <- subset(lfobj, year != hyear, month)
      if(nrow(ii)==0){hyearstart <-1} else if(max(ii) <5.5){hyearstart <- max(ii)+1}else{hyearstart <- min(ii)}
     if(length(breakdays) == 1){
        breakdays <- c(paste("1/",hyearstart,sep = ""),breakdays)
      }
    bdays <- data.frame(matrix(ncol = 2))
    names(bdays) <- c("day", "month")
    str <- strsplit(breakdays,"/")
    for(ii in seq_along(str)){
      bdays[ii,] <- as.numeric(c(str[[ii]]))
    }
    threshold$breakp <- FALSE
    for(ii in seq_along(bdays$day)){
    threshold$breakp[threshold$day == bdays[ii,"day"] & threshold$month == bdays[ii,"month"]]<-TRUE
  }
    threshold$season = cumsum(threshold$breakp)
    threshold$season[threshold$season == 0] <- max(threshold$season)
    threshold$breakp <- NULL
    something <- merge(lfobj, threshold, by = c("day", "month"),sort = FALSE)
    sea <- aggregate(flow ~ season, something, quantile, probs = 1-threslevel/100)
    threshold <- merge(threshold, sea, by = "season", sort = FALSE)
    threshold$season <- NULL
  }                   
threshold} 
