"gsid2str" <- function(siteNumbers, hack=FALSE, ...) {
  if(hack) {
     tmp <- strsplit(as.character(siteNumbers), "") # split each down to each digit
     sapply(1:length(tmp), function(i) { me <- tmp[[i]] # extract each and cat
       if(length(me) == 7 ) me <- paste(c("0",me), collapse="") # leading zero
       if(length(me) == 9 ) me <- paste(c("0",me), collapse="")
       if(length(me) == 11) me <- paste(c("0",me), collapse=""); me })
  } else {
     return(siteNumbers)
  }
}
