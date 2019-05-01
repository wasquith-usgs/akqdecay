"dvpartenv2mnpartenv" <- function(sites=NULL, dvpartenv=NULL, envir=NULL, silent=FALSE,...) {
   if(! is.environment(dvpartenv)) {
      warning(" dvpartenv is not testing as an environment")
      return()
   }
   if(is.null(envir)) {
      warning(" need to specify the envir environment")
      return()
   } else if(! is.environment(envir)) {
      warning(" envir is not testing as an environment")
      return()
   }
   ifelse(is.null(sites), SITES <- sort(ls(dvpartenv)), SITES <- sites)
   n <- length(SITES); k <- 0
   for(i in 1:n) {
      site <- SITES[i]
      if(! silent) message(" dvpart2mnpart() for ",site," (",i,"/",n,")")
      D <- get(site, envir=dvpartenv)
      if(length(as.data.frame(D)[1,]) == 1) {
         assign(site, NA, envir=envir)
      } else {
         Z <- dvpart2mnpart(D, ...)
         k <- k + 1
         assign(site, Z,  envir=envir)
      }
   }
   names(k) <- "number dvpart2mnpart() processed"
   return(k)
}

