"akq_counts" <- function(akqenv, silent=FALSE, ...) {
  if(is.null(akqenv)) {
     warning(" need to specify the akqenv environment")
     return()
  } else if(! is.environment(akqenv)) {
     return(akqenv$counts)
  }
  SITES <- sort(ls(akqenv)); n <- length(SITES)
  TN <- TD <- TI <- TC <- TNA <- TZ <- TRNA <- DD <- rep(NA, n)
  for(i in 1:n) {
    if(! silent) message(" Counts for ", SITES[i], " (",i,"/",n,")", appendLF = FALSE)
    Z <- get(SITES[i], envir=akqenv)
    if(length(Z) == 1) {
       if(! silent) message(" -- none available"); next
    } else {
       if(! silent) message(" -- got'em")
    }
    CC <- Z$counts; # print(CC)
    TN[i] <- CC$total_count; TD[i] <- CC$decreases; TI[i] <- CC$increases
    TC[i] <- CC$nochanges;  TNA[i] <- CC$NAs;       TZ[i] <- CC$rawDV_zeros
    TRNA[i] <- CC$rawDV_NAs; DD[i] <- CC$delDates_ne_lag
  }
  zz <- data.frame(site=SITES, total_count=TN, decreases=TD, increases=TI, nochanges=TC,
                   NAs=TNA, rawDV_zeros=TZ, rawDV_NAs=TRNA,
                   delDates_ne_lag=DD, stringsAsFactors=FALSE)
  return(zz)
}
