"dvget" <-
function(siteNumber, sdate="", edate="", flowlo=NULL, flowhi=NULL, date2s=NA, date2e=NA,
                     ignore.working=TRUE, ignore.provisional=TRUE, silent=FALSE,
                     drsilent=TRUE, drget=FALSE, pCode="00060", sCode = "00003", message="", ...) {
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
      if(! silent) message(" dvget() for ", siteNumber, " --- ", message)
   }
   if(is.na(sdate)) sdate <- ""
   if(is.na(edate)) edate <- ""
   zz <- NULL
   try(zz <- dataRetrieval::readNWISdv(siteNumber, startDate=sdate, endDate=edate,
                         parameterCd=pCode, statCd=sCode, ...), silent=drsilent)
   if(! is.null(zz)) {
     zz <- dataRetrieval::renameNWISColumns(zz)
     if(drget) return(zz)
   } else {
     warning("dvget() --> siteNumber ",siteNumber," appears to be bad or Internet error")
     return(list()) # empty list, akqdecay is to use length(YY$year) as the test
     # for data presence as well as emptiness if a bad gage id has been used.
   }
   zz$site <- as.character(zz$site_no)
   if(! is.na(date2s)) {
      tmp <- NULL
      try(tmp <- as.Date(as.character(date2s)), silent=TRUE)
      if(! is.null(tmp)) {
         if(! silent) message(" filtering on secondary start date >= ",tmp)
         zz <- zz[zz$Date >= tmp, ]
      }
   }
   if(! is.na(date2e)) {
      tmp <- NULL
      try(tmp <- as.Date(as.character(date2e)), silent=TRUE)
      if(! is.null(tmp)) {
         if(! silent) message(" filtering on secondary end date <= ",tmp)
         zz <- zz[zz$Date <= tmp, ]
      }
   }

   dt <- strsplit(as.character(zz$Date), split="-")
   zz$year   <- sapply(dt, function(i) return(i[1]))
   zz$month  <- as.numeric(sapply(dt, function(i) return(i[2])))
   zz$decade <- as.numeric(sub("\\d$", "", zz$year))*10
   zz$wyear  <- zz$year <- as.numeric(zz$year)
   zz$wyear[zz$month >= 10] <- zz$wyear[zz$month >= 10] + 1
   zz$wyear  <- as.integer(zz$wyear )
   zz$year   <- as.integer(zz$year  )
   zz$month  <- as.integer(zz$month )
   zz$decade <- as.integer(zz$decade)
   if(! exists("Flow", zz)) {
      # Flow test dam releases  H <- dvget("08025360")
      warning("** Flow is missing for ", siteNumber,
              ", emergency inclusion of 'Flow <- NA'")
      zz$Flow <- rep(NA, length(zz$site))
   }
   if(exists("Flow_cd", zz)) {
     # Flow test dam releases  H <- dvget("08025360")
     # another Flow_cd test    H <- dvget("02313230")
      zz$Flow_cd[is.na(zz$Flow_cd)] <- "NA"    # LOOK AT THIS
      if(ignore.working)     zz <- zz[zz$Flow_cd != "W", ] # is this "seen" by the public?
      if(ignore.provisional) zz <- zz[zz$Flow_cd != "P", ] # Provisional record certainly is.
      zz$Flow_cd[zz$Flow_cd == "NA"] <- NA    # LOOK AT THIS
   } else {
      warning("** Flow_cd is missing for ", siteNumber,
              ", emergency inclusion of 'Flow_cd <- NA'")
      zz$Flow_cd <- rep(NA, length(zz$site))
   }

   # though akqdecay() also checks for this, one station have more than 2,000 different
   # station tests causes a breakage. See comments below for "02310000" and note the
   # lines above
   if(length(unique(zz$site)) != 1) {
      warning("though only one site pursued on dvget() retrieval, processing yields ",
              "either zero or > one, still returning the data.frame")
   }
   if(! is.null(flowlo))  zz$Flow[ zz$Flow > flowlo] <- NA
   if(! is.null(flowhi))  zz$Flow[ zz$Flow < flowhi] <- NA
   return(zz)
}

# From waterdata link for discharge, note two discharges and the missingness
# If I don't switch zz$Flow_cd[is.na(zz$Flow_cd)] <- "NA" and then switch
# it back to NA after subsetting for "W" and "P", I get the 13 additional rows added.
# This station as a "backup" discharge and code associated with it. (1) I don't know
# what that means and (2) having difficulty seeing why the data frame inflates.
#USGS	02310000	2016-10-27	9.28	P	9.28	P
#USGS	02310000	2016-10-28	8.39	P	8.51	P
#USGS	02310000	2016-10-29	7.73	P
#USGS	02310000	2016-10-30	7.23	P
#USGS	02310000	2016-10-31	6.89	P
#USGS	02310000	2016-11-01	6.42	P
#USGS	02310000	2016-11-02	5.96	P
#USGS	02310000	2016-11-03	5.58	P
#USGS	02310000	2016-11-04	5.30	P
#USGS	02310000	2016-11-05	5.13	P
#USGS	02310000	2016-11-06	5.10	P
#USGS	02310000	2016-11-07	4.89	P
#USGS	02310000	2016-11-08	4.65	P
#USGS	02310000	2016-11-09	4.61	P
#USGS	02310000	2016-11-10	4.56	P
#USGS	02310000	2016-11-11	4.45	P
#USGS	02310000	2016-11-12
#USGS	02310000	2016-11-13
#USGS	02310000	2016-11-14
#USGS	02310000	2016-11-15
#USGS	02310000	2016-11-16
#USGS	02310000	2016-11-17	3.86	P	4.04	P


#25709      USGS 02310000 2016-10-20   18.3       A          18.2                P 02310000 2016    10   2010  2017
#NA         <NA>     <NA>       <NA>     NA    <NA>            NA             <NA>     <NA>   NA    NA     NA    NA
#NA.1       <NA>     <NA>       <NA>     NA    <NA>            NA             <NA>     <NA>   NA    NA     NA    NA
#NA.2       <NA>     <NA>       <NA>     NA    <NA>            NA             <NA>     <NA>   NA    NA     NA    NA
#NA.3       <NA>     <NA>       <NA>     NA    <NA>            NA             <NA>     <NA>   NA    NA     NA    NA
#NA.4       <NA>     <NA>       <NA>     NA    <NA>            NA             <NA>     <NA>   NA    NA     NA    NA
#NA.5       <NA>     <NA>       <NA>     NA    <NA>            NA             <NA>     <NA>   NA    NA     NA    NA
#NA.6       <NA>     <NA>       <NA>     NA    <NA>            NA             <NA>     <NA>   NA    NA     NA    NA
#NA.7       <NA>     <NA>       <NA>     NA    <NA>            NA             <NA>     <NA>   NA    NA     NA    NA
#NA.8       <NA>     <NA>       <NA>     NA    <NA>            NA             <NA>     <NA>   NA    NA     NA    NA
#NA.9       <NA>     <NA>       <NA>     NA    <NA>            NA             <NA>     <NA>   NA    NA     NA    NA
#NA.10      <NA>     <NA>       <NA>     NA    <NA>            NA             <NA>     <NA>   NA    NA     NA    NA
#NA.11      <NA>     <NA>       <NA>     NA    <NA>            NA             <NA>     <NA>   NA    NA     NA    NA
#NA.12      <NA>     <NA>       <NA>     NA    <NA>            NA             <NA>     <NA>   NA    NA     NA    NA
#NA.13      <NA>     <NA>       <NA>     NA    <NA>            NA             <NA>     <NA>   NA    NA     NA    NA

