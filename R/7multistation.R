multistationsreport <- function(...,
                                indices = c("meanflow", "Q95", "MAM1", "MAM7", "MAM10", "MAM30", "MAM90", "baseflowindex", "recession"),
                                recessionmethod = "MRC",
                                recessionseglength = 7,
                                recessionthreshold = 70,
                                recessiontrimIRS = 0.1
                                ){

ind <- match.arg(indices, several.ok = TRUE)
args <- list(...)
names <- as.character(as.list(substitute(list(...)))[-1L])
table <- data.frame(row.names = names)
for(ii in args){
    lfcheck(ii)
  }
if("meanflow" %in% ind){
  table$meanflow <- lapply(args,meanflow)
}
if("Q95" %in% ind){
   table$Q95 <- lapply(args,Q95)
 }
if("MAM1" %in% ind){
   table$MAM1 <- lapply(args,MAM, n=1)
 }
if("MAM7" %in% ind){
   table$MAM7 <- lapply(args,MAM, n=7)
 }
if("MAM10" %in% ind){
   table$MAM10 <- lapply(args,MAM, n=10)
 }
if("MAM30" %in% ind){
   table$MAM30 <- lapply(args,MAM, n=30)
 }
if("MAM90" %in% ind){
   table$MAM90 <- lapply(args,MAM, n=90)
 }
if("baseflowindex" %in% ind){
   table$BFI <- lapply(args,BFI)
 }
if("recession" %in% ind){
   table$recession <- lapply(args,recession,method = recessionmethod, seglength = recessionseglength, threshold = recessionthreshold,plotMRC = FALSE, trimIRS=recessiontrimIRS)
 }
table}
