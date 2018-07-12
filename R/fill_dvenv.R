"fill_dvenv" <-
function(siteNumbers, envir=NULL,
         sdates=NULL, edates=NULL, flowlos=NULL, flowhis=NULL, silent=FALSE, ...) {
   if(is.null(envir)) {
      warning(" need to specify the envir environment")
      return()
   } else if(! is.environment(envir)) {
      warning(" envir is not testing as an environment")
      return()
   }
   siteNumbers <- sort(as.character(siteNumbers))
   n <- length(siteNumbers)
   if(  is.null(sdates)) sdates <- rep("", n)
   if(  is.null(edates)) edates <- rep("", n)
   if(! is.null(flowlos) & length(flowlos) != n) {
      stop("flowlos argument is not the same length as siteNumbers")
   }
   if(! is.null(flowhis) & length(flowhis) != n) {
      stop("flowhis argument is not the same length as siteNumbers")
   }
   if(length(sdates) == 1) sdates <- rep(sdates, n)
   if(length(edates) == 1) edates <- rep(edates, n)
   for(i in 1:n) {
      siteNumber <- siteNumbers[i]
      txt <- paste0("via fill_dvenv (",i,"/",n,")")
      akqdv <- dvget(siteNumber, message=txt, silent=silent,
                          sdate=sdates[i],   edate=edates[i],
                        flowlo=flowlos[i], flowhi=flowhis[i], ...)
      assign(siteNumber, akqdv, envir=envir)
   }
}
