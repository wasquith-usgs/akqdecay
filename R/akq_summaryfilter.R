"akq_summaryfilter" <-
 function(akqenv, mindv=NA, begyr=NA, endyr=NA, silent=FALSE, ...) {
    if(is.null(akqenv)) {
      warning(" need to specify the akqenv environment")
      return()
   } else if(! is.environment(akqenv)) {
      stop(" function requires and environment for processing")
   }
   SITES <- sort(ls(akqenv)); n <- length(SITES)
   wnts <- vector(mode="numeric"); j <- 0
   for(i in 1:n) {
      if(! silent) message(" Summary evaluation for ", SITES[i],
                           " (",i,"/",n,")", appendLF = FALSE)
      Z <- get(SITES[i], envir=akqenv)
      if(length(Z) == 1) {
         if(! silent) message(" -- none available"); next
      } else {
         if(! silent) message(" -- got'em")
      }
      SUM <- Z$summary
      if(! is.na(mindv) & SUM$total_count >= mindv) {
         j <- j + 1; wnts[j] <- SITES[i]; next
      }
      if(! is.na(begyr) & SUM$beg_year >= begyr) {
         j <- j + 1; wnts[j] <- SITES[i]; next
      }
      if(! is.na(endyr) & SUM$end_year <= endyr) {
         j <- j + 1; wnts[j] <- SITES[i]; next
      }
  }
  return(wnts) # wanted sites
}
