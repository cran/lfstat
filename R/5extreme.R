
#Calculates the t-years events and gives probability plot
tyears <- function(lfobj,event = 100,n = 7,dist = c("wei","gev","ln3","gum","pe3"),legend = TRUE,zetawei = NULL){
lfcheck(lfobj)
distr <- match.arg(dist,several.ok = TRUE)
annual <- MAannual(lfobj,n)
pnull <- NULL
if(min(annual$MAn) == 0){
 zero <-  which(annual$MAn == 0)
 pnull <- length(zero)/length(annual$MAn)
 warning(paste("There were",length(zero),"with 0-flow extremes. Therefore a mixed distribution with p_0 = ", pnull, "was fitted. See ?tyears for details!"))
 annual <- annual[-zero,]
}


lmom <- samlmu(annual$MAn)
result <- list(T_Years_Event = NULL,pnull = pnull, parameters = NULL, lmom = lmom)

for(ii in distr){
  if(ii == "wei" &!is.null(zetawei)){
    assign(paste(ii,"para",sep = ""),eval(parse(text = paste("pel",ii,"(lmom,bound = zetawei)",sep = ""))))
}else{
assign(paste(ii,"para",sep = ""),eval(parse(text = paste("pel",ii,"(lmom)",sep = ""))))
}
eval(parse(text = paste("result$parameters$",ii,"<-",ii,"para",sep ="")))
}


evplot(annual$MAn,rp.axis = FALSE)

if(!is.null(result$parameters$wei)){
  if(result$parameters$wei["zeta"]<0 & is.null(zetawei)){
       warning(paste("Estimation of zeta in the Weibull distribution resulted in:", result$parameter$wei["zeta"], "Zeta was set to 0. If you want to use a zeta smaller then zero please use the 'zetawei' argument of 'tyears'"))
    parawei <- pelwei(lmom,bound = 0)
    result$parameters$wei <- parawei
  }
}

if(is.null(pnull)){
for(ii in distr){
eval(
     parse(text =
           paste("evdistq(qua",ii,",",ii,"para, col =",which(ii == distr),")",sep = "")
           )
     )
result[["T_Years_Event"]][ii] <- eval(
                   parse(text =
                         paste("qua",ii,"(1/event,",ii,"para)",sep = "")
                         )
                   )
}}else{
if(pnull > 1/event){
  for(ii in distr){
    eval(
     parse(text =
           paste("evdistq(qua",ii,",",ii,"para, col =",which(ii == distr),")",sep = "")
           )
     ) 
    result[["T_Years_Event"]][ii] <- 0}}

if(pnull < 1/event & !is.null(pnull)){
      for(ii in distr){
        eval(
     parse(text =
           paste("evdistq(qua",ii,",",ii,"para, col =",which(ii == distr),")",sep = "")
           )
     )  
        result[["T_Years_Event"]][ii] <- eval(
                   parse(text =
                         paste("qua",ii,"((1/event-pnull)/(1-pnull),",ii,"para)",sep = "")
                         )
                   )
}
    title("Plot is fit for values > 0 only")}}

  

if(legend)
  legend("bottomright", c("Empirical",distr), col = c(1,1:length(distr)), pch = c(1,rep(-1,length(distr))),lty = c(-1,rep(1,length(distr))))

#evdistq(quawei,weipara,col = "red")
#evdistq(quagev,gevpara,col = "blue")
result
}


#############################
#Regional frequency analysis#
#############################

#gets a list of lfobjs!
#rfa <- function(lfobj,n,...){
#lmom <- data.frame(matrix(ncol = 4))  
#  for(ii in seq_along(lfobj)){
#    lfcheck(lfobj[[ii]])
#    annual <- MAannual(lfobj[[ii]],n)
#    lmom[ii,] <-samlmu(annual$MAn)
#  }
#  names(lmom) <-names(samlmu(annual$MAn))
#  lmrd(lmom,...)
#  }

rfa <- function(lflist, n = 7, event = 100, dist =  c("wei","gev","ln3","gum","pe3")){
  lapply(lflist,lfcheck)
  distr <- match.arg(dist,several.ok = FALSE)
  agg <- function(x,N){MAannual(x,N)$MAn}
  ma <- lapply(lflist,agg, N = n)
  reg <- regsamlmu(ma)
  rfit <- regfit(reg, distr)
#  tyears <- eval(parse(text = paste0("qua",dist,"(1/event,rfit$para)")))
#  rfit$tyears <- rfit$index * tyears
  rfit
}

rfaplot <- function(lflist, n = 7,...){
 lapply(lflist,lfcheck)
 agg <- function(x,N){MAannual(x,N)$MAn}
  ma <- lapply(lflist,agg, N = n)
  reg <- regsamlmu(ma)
 lmrd(reg,...)
}


#sitequant(c(0.9, 0.99, 0.999), rfit, sitenames=1:3)

#Tyears und rfa liefern für einen Standort  die selben Ergebnisse wenn:
# GEV: ersten beiden parameter mit "Index" gestreckt werden
# Das T-Years-Event mit "Index" gestreckt wird.
# Gregor klären, ob:
# Return values so ok,
# Anleitung/verweis auf Hoskings zum Checken, weiterrechnen...
