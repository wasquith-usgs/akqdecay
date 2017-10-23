"akq_globe" <- function(akqenv, silent=TRUE, ...) {
  if(is.null(akqenv)) {
     warning(" need to specify the akqenv environment")
     return()
  }
  CN  <- akq_counts(akqenv, silent=silent, ...)
   n  <- length(CN$site)
  tc  <- sum(CN$total_count,     na.rm=TRUE)
  de  <- sum(CN$decreases,       na.rm=TRUE)
  nn  <- sum(CN$increases,       na.rm=TRUE)
  nc  <- sum(CN$nochanges,       na.rm=TRUE)
  na  <- sum(CN$NAs,             na.rm=TRUE)
  tz  <- sum(CN$rawDV_zeros,     na.rm=TRUE)
  rna <- sum(CN$rawDV_NAs,       na.rm=TRUE)
  dd  <- sum(CN$delDates_ne_lag, na.rm=TRUE)
  # deliberate decision to not include tz, rna, and dd in the data.frames below
  zza <- data.frame(num_sites=n, total_count=tc, decreases=de, increases=nn,
                                 no_change=nc, NAs=na,
                                 stringsAsFactors=FALSE)
  zzb <- data.frame(num_sites=n, pct_total_count=100*tc/tc, pct_decreases=100*de/tc,
                                 pct_increase=100*nn/tc, pct_nochange=100*nc/tc,
                                 pct_NAs=100*na/tc,
                                 stringsAsFactors=FALSE)
  row.names(zza) <- ""; row.names(zzb) <- ""
  return(list(counts=zza, percents=zzb))
}
