"fill_dvpartenv" <- function(sites=NULL, dvenv=NULL, envir=NULL, cdas=NULL,
                             fillgaps=FALSE, silent=FALSE,...) {
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
   if(is.null(cdas)) cdas <- rep(NA, n)
   for(i in 1:n) {
      site <- SITES[i]; cda  <- cdas[i]
      if(! silent) message(" dvpart() for ",site," (",i,"/",n,")")
      D <- get(site, envir=dvenv)
      if(length(as.data.frame(D)[1,]) == 1) {
         assign(site, NA, envir=envir)
      } else {
         Z <- dvpart(D, site_no=site, cda=cda, fillgaps=fillgaps, ...)
         k <- k + 1
         assign(site, Z,  envir=envir)
      }
   }
   names(k) <- "number dvpart() processed"
   return(k)
}

# wolfpart.env <- new.env()
# fill_dvpartenv(dvenv=wolf.env, envir=wolfpart.env, fillgaps=FALSE)
# fill_dvpartenv(dvenv=wolf.env, envir=wolfpart.env, fillgaps=TRUE)
