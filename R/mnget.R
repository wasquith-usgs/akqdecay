"mnget" <-
function(siteNumber, sdate="", edate="", flowlo=NULL, flowhi=NULL,
                     silent=FALSE, drsilent=TRUE, drget=FALSE, pCode="00060", message="", ...) {
   if(length(unique(siteNumber)) > 1) {
      warning("multiple siteNumbers provided, returning all data, ",
              "but akqdecay() is **not** itself so vectorized and will issue a stop() on these data")
   }
   tmp <- strsplit(as.character(siteNumber), "")
   zz <- sapply(1:length(tmp), function(i) length(tmp[[i]]))
   if(any(zz < 8)) {
      stop("a site number must be at least 8 digits long according to https://waterservices.usgs.gov")
   }
   if(message != "") {
      if(! silent) message(" mnget() for ", siteNumber, " --- ", message)
   }
   if(is.na(sdate)) sdate <- ""
   if(is.na(edate)) edate <- ""
   zz <- NULL
   try(zz <- dataRetrieval::readNWISstat(siteNumber, startDate=sdate, endDate=edate,
                         parameterCd=pCode, statReportType="monthly", ...), silent=drsilent)
   if(! is.null(zz)) {
     zz <- dataRetrieval::renameNWISColumns(zz)
     if(drget) return(zz)
   } else {
     warning("mnget() --> siteNumber ",siteNumber," appears to be bad or Internet error")
     return(list()) # empty list, akqdecay is to use length(YY$year) as the test
     # for data presence as well as emptiness if a bad gage id has been used.
   }
   zz$site <- as.character(zz$site_no)
   zz$ts_id <- NULL
   zz$loc_web_ds <- NULL
   zz$parameter_cd <- NULL
   dt <- strsplit(as.character(zz$Date), split="-")
   zz$year   <- zz$year_nu; zz$year_nu <- NULL
   zz$month  <- zz$month_nu; zz$month_nu <- NULL
   zz$decade <- as.integer(as.numeric(sub("\\d$", "", zz$year))*10)
   if(! exists("mean_va", zz)) {
      warning("** mean_va is missing for ", siteNumber,
              ", emergency inclusion of 'mean_va <- NA'")
      zz$mean_va <- rep(NA, length(zz$site))
   }

   # though akqdecay() also checks for this, one station have more than 2,000 different
   # station tests causes a breakage. See comments below for "02310000" and note the
   # lines above
   if(length(unique(zz$site)) != 1) {
      warning("though only one site pursued on dvget() retrieval, processing yields ",
              "either zero or > one, still returning the data.frame")
   }

   zz$Flow <- zz$mean_va; zz$mean_va <- NULL
   if(! is.null(flowlo))  zz$Flow[ zz$Flow > flowlo] <- NA
   if(! is.null(flowhi))  zz$Flow[ zz$Flow < flowhi] <- NA
   #print(head(zz))
   # mnget("02413210", edate="2016-09") # caused problems because of some
   # weird Flow == NA issues so the length test below is for that.
   if(length(zz$Flow[zz$Flow == -999999]) > 0) { # station 07040000 for year 2015 had -999999
      message("  at least one -999999 discharge") # I did not know this was possible
      zz$Flow[zz$Flow == -999999] <- NA
   }
   if(message != "") {
      if(! silent) message("         found ",length(zz$Flow)," months of flow")
   }

   return(zz)
}
