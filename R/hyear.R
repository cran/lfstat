#Let the hydrological year start at any month

#If startmonth is Jan-Jun -> hyear reduces to the previous year, else the month after startmonth move to the next year.

hyear <- function(dat, startmonth = 1){
if(startmonth > 6.5){
dat$hyear <- dat$year + (dat$month >= startmonth)
     }else
     {
  dat$hyear <- dat$year - (dat$month < startmonth)
     }
dat}
