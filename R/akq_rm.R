"akq_rm" <- function(siteNumbers, envir=NULL, invert=FALSE, ...) {
  if(is.null(envir)) {
     warning(" need to specify a dvenv or akqenv environment")
     return()
  } else if(! is.environment(envir)) {
     warning(" envir is not an environment")
     return()
  }
  sites <- ls(envir)
  if(is.null(siteNumbers)) {
     warning("siteNumbers is NULL")
     return(NULL)
  }
  siteNumbers <- siteNumbers[! is.na(siteNumbers)]
  siteNumbers <- unique(siteNumbers)
  if(length(siteNumbers) == 0) {
    warning("length of non-NA siteNumbers is zero, returning as.list(envir)")
    return(as.list(envir))
  }
  ifelse(invert, them <- setdiff(sites, siteNumbers), them <- siteNumbers)
  tmp <- as.list(envir)
  tmp[them] <- NULL
  return(tmp)
}
