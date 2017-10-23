"fill_akqenv_parallel" <- function(sites=NULL, dvenv=NULL, envir=NULL, silent=FALSE, ...) {
   if(! .Platform$OS.type == "unix") {
      warning("This function is not supported on Windows because of a lack of fork(), returning")
      return() # Special handling for Windows
   }
   if(is.null(dvenv)) {
      warning(" need to specify the dvenv environment")
      return()
   } else if(! is.environment(dvenv)) {
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
   ifelse(is.null(sites), SITES <- ls(dvenv), SITES <- sites)
   n <- length(SITES)
   if(n == 0) {
      warning(" no sites for processing")
      return(NULL)
   }

   # kill rsession forked processes based on PID extraction from mcparallelDo. However,
   # the mcparallelDo is custom modified for this to work!
   # (See akqdecay package documentation)
   "killZombies" <- function(jobs) {
      if(! .Platform$OS.type == "unix") return("") # Special handling for Windows
      pids <- sapply(strsplit(jobs, "-"), function(i) return(i[2]))
      for(pid in pids) system(paste0("kill ",pid))
   }

   k <- 0; theby <- 6 # The theby is not really used per se, a piece for experiments
   for(i in seq(1,n, by=theby)) {
      sA <- SITES[i];   sB <- SITES[i+1]; sC <- SITES[i+2];
      sD <- SITES[i+3]; sE <- SITES[i+4]; sF <- SITES[i+5];
      jA <- jB <- jC <- jD <- jE <- jF <- NULL
      if(! is.na(sA)) {
         if(exists(sA, envir=dvenv)) {
             k <- k + 1; if(! silent) message(" launching siteA ", sA, " (",k,"/",n,")")
            jA <- mcparallelDo::mcparallelDo(akqdecay(get(sA, envir=dvenv),
                                          site=sA, ...), sA, targetEnvironment=envir)
         } else {
            if(! silent) message(" ** siteA=",sA, " does not exist (quietly skipping)")
         }
      }
      if(! is.na(sB) & theby >= 2) {
         if(exists(sB, envir=dvenv)) {
             k <- k + 1; if(! silent) message(" launching siteB ", sB, " (",k,"/",n,")")
            jB <- mcparallelDo::mcparallelDo(akqdecay(get(sB, envir=dvenv),
                                          site=sB, ...), sB, targetEnvironment=envir)
         } else {
            if(! silent) message(" ** siteB=",sB, " does not exist (quietly skipping)")
         }
      }
      if(! is.na(sC) & theby >= 3) {
         if(exists(sC, envir=dvenv)) {
             k <- k + 1; if(! silent) message(" launching siteC ", sC, " (",k,"/",n,")")
            jC <- mcparallelDo::mcparallelDo(akqdecay(get(sC, envir=dvenv),
                                          site=sC, ...), sC, targetEnvironment=envir)
         } else {
            if(! silent) message(" ** siteD=",sC, " does not exist (quietly skipping)")
         }
      }
      if(! is.na(sD) & theby >= 4) {
         if(exists(sD, envir=dvenv)) {
             k <- k + 1; if(! silent) message(" launching siteD ", sD, " (",k,"/",n,")")
            jD <- mcparallelDo::mcparallelDo(akqdecay(get(sD, envir=dvenv),
                                          site=sD, ...), sD, targetEnvironment=envir)
         } else {
            if(! silent) message(" ** siteD=",sD, " does not exist (quietly skipping)")
         }
      }
      if(! is.na(sE) & theby >= 5) {
         if(exists(sE, envir=dvenv)) {
             k <- k + 1; if(! silent) message(" launching siteE ", sE, " (",k,"/",n,")")
            jE <- mcparallelDo::mcparallelDo(akqdecay(get(sE, envir=dvenv),
                                          site=sE, ...), sE, targetEnvironment=envir)
         } else {
            if(! silent) message(" ** siteE=",sE, " does not exist (quietly skipping)")
         }
      }
      if(! is.na(sF) & theby >= 6) {
         if(exists(sF, envir=dvenv)) {
             k <- k + 1; if(! silent) message(" launching siteF ", sF, " (",k,"/",n,")")
            jF <- mcparallelDo::mcparallelDo(akqdecay(get(sF, envir=dvenv),
                                          site=sF, ...), sF, targetEnvironment=envir)
         } else {
            if(! silent) message(" ** siteF=",sF, " does not exist (quietly skipping)")
         }
      }
      while(1) { # infinite loop---hopefully the jobs will end!
         if(all(mcparallelDo::mcparallelDoCheck()) == TRUE) {
            killZombies(c(jA, jB, jC, jD, jE, jF))
            break
         }
      }
   }
   names(k) <- "number akqdecay() processed"
   return(k)
}

