"fill_akqenv" <- function(sites=NULL, dvenv=NULL, envir=NULL, silent=FALSE,  ...) {
   if(! is.environment(dvenv)) {
      warning(" dvenv is not testing as an environment")
      return()
   }
   if(is.null(envir)) {
      warning(" need to specify the envir environment")
      return()
   } else if(! is.environment(envir)) {
      warning(" envir is not testing as an environment")
      return()
   }
   ifelse(is.null(sites), SITES <- sort(ls(dvenv)), SITES <- sites)
   n <- length(SITES); k <- 0
   for(i in 1:n) {
      site <- SITES[i]
      if(! silent) message(" akqdecay() for ",site," (",i,"/",n,")")
      D <- get(site, envir=dvenv)
      if(length(as.data.frame(D)[1,]) == 1) {
         assign(site, NA, envir=envir)
      } else {
         Z <- akqdecay(D, site=site, ...)
         k <- k + 1
         assign(site, Z,  envir=envir)
      }
   }
   names(k) <- "number akqdecay() processed"
   return(k)
}
