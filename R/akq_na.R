"akq_na" <- function(akqenv, silent=FALSE, ...) {
  if(is.null(akqenv)) {
     warning(" need to specify the akqenv [fill_akqenv()] or daily-value [fill_dveqn()] environment")
     return()
  } else if(! is.environment(akqenv)) {
     warning(" akqenv is not an environment")
     return()
  }

  SITES <- sort(ls(akqenv)); n <- length(SITES); t <- 0; NASITES <- NULL
  for(i in 1:n) {
    Z <- get(SITES[i], envir=akqenv)
    if(length(Z) == 1) {
       if(is.na(Z)) {
          t <- t + 1; NASITES[t] <- SITES[i]; next
       } else {
          stop(" did not expect anything other than NA")
       }
    } else if(is.data.frame(Z)) {
       if(length(Z[,1]) == 0) {
         t <- t + 1; NASITES[t] <- SITES[i]; next
       }
    }
  }
  m <- length(NASITES)
  if(! silent & m == 0) {
     message(" Found zero sites with NA akdecay() processing or zero rows of daily values dvget().")
  } else if(! silent) {
     message(" Found ", length(NASITES),
             " having NA akdecay() processing or zero rows of daily values dvget()\n",
             "    Consider running akq_rm(akqenv, THE_SITES) on these sites, and\n",
             "    don't forget to update your master site list though!")
  }
  return(NASITES)
}
