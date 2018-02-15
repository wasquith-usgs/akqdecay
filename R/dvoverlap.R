"dvoverlap" <-
function(dvenv, begyr=1987, endyr=2016, type=c("wyear", "year"),
                missing.days=7, silent=TRUE, ...) {
   if(!is.environment(dvenv)) {
     warning(" dvenv is not testing as an environment")
     return()
   }
   if(endyr < begyr) {
     warning(" bad ending and starting years given")
     return()
   }
   type <- match.arg(type)
   if(type == "wyear") {
      txt <- c("-10-01", "-09-30")
      off <- 1
   } else {
      txt <- c("-01-01", "-12-31")
      off <- 0
   }
   yrs <- begyr:endyr; nyrs <- length(yrs)
   diyenv <- new.env(); diys <- vector(mode="numeric", nyrs)
   for(i in 1:length(yrs)) {
      beg <- as.Date(paste0(yrs[i]-off,txt[1]))
      end <- as.Date(paste0(yrs[i],    txt[2]))
      assign(as.character(yrs[i]), 1+as.numeric(end - beg), envir=diyenv)
   }
   sites_desired <- new.env()
   for(site in ls(dvenv)) {
      if(! silent) message(" dvoverlap() for ", site, appendLF=FALSE)
      Z <- get(site, envir=dvenv)
      if(length(Z) == 1) {
         warning(site, " --- no dvs available?")
         next
      }
      ifelse(type == "wyear", Z <- Z[Z$wyear >= begyr & Z$wyear <= endyr,],
                              Z <- Z[Z$year  >= begyr & Z$year  <= endyr,])
      k <- 0
      for(yr in yrs) {
         ifelse(type == "wyear", ab <- Z[Z$wyear == yr,], ab <- Z[Z$year == yr,])
         if(length(ab$Flow) >= get(as.character(yr), diyenv)-missing.days) k <- k + 1
      }
      if(k != nyrs) {
         if(! silent) message(" --- does not meet criteria")
         next
      }
      if(! silent) message(" has ",k," 'full' years: (",begyr,",",endyr,")")
      assign(site, "yep", sites_desired)
   }
   sites_desired <- ls(sites_desired)
   return(sites_desired)
}


