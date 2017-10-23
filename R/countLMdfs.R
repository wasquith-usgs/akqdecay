"countLMdfs" <- function(DECR, INCR, NC, silent=FALSE, ...) {
    whd <- attr(DECR, "which"); whi <- attr(INCR, "which"); whn <- attr(NC, "which")
    which <- unique(c(whd,whi,whn))
    if(length(which) != 1) {
       warning(" attribute 'which' is not consistent between the arguments")
       return()
    }
    if(which == "year") {
      D <- data.frame(site=DECR$site, year=DECR$year,     decreases=DECR$count)
      I <- data.frame(site=INCR$site, year=INCR$year,     increases=INCR$count)
      N <- data.frame(site=NC$site,   year=NC$year,       nochanges=NC$count  )
    } else if(which == "decade") {
      D <- data.frame(site=DECR$site, decade=DECR$decade, decreases=DECR$count)
      I <- data.frame(site=INCR$site, decade=INCR$decade, increases=INCR$count)
      N <- data.frame(site=NC$site,   decade=NC$decade,   nochanges=NC$count  )
    } else {
      stop("should not be here in logic")
    }
    DIN <- merge(D,   I, all=TRUE); DIN <- merge(DIN, N, all=TRUE)
    suppressWarnings(DIN$decreases[is.na(DIN$decreases) & ! is.na(DIN$year)  ] <- 0)
    suppressWarnings(DIN$decreases[is.na(DIN$decreases) & ! is.na(DIN$decade)] <- 0)
    suppressWarnings(DIN$increases[is.na(DIN$increases) & ! is.na(DIN$year)  ] <- 0)
    suppressWarnings(DIN$increases[is.na(DIN$increases) & ! is.na(DIN$decade)] <- 0)
    suppressWarnings(DIN$nochanges[is.na(DIN$nochanges) & ! is.na(DIN$year)  ] <- 0)
    suppressWarnings(DIN$nochanges[is.na(DIN$nochanges) & ! is.na(DIN$decade)] <- 0)
    DIN$total_count_minusNAs <- DIN$decreases+DIN$increases+DIN$nochanges
    DIN <- DIN[,c(1,2,6,3,4,5)]
    DIN$pct_decreases <- 100*(DIN$decreases/DIN$total_count_minusNAs)
    DIN$pct_increases <- 100*(DIN$increases/DIN$total_count_minusNAs)
    DIN$pct_nochanges <- 100*(DIN$nochanges/DIN$total_count_minusNAs)
    DIN <- DIN[! is.na(DIN$decade), ] # a leak can still occur
    sumd <- sum(DIN$decreases, na.rm=TRUE)
    sumi <- sum(DIN$increases, na.rm=TRUE)
    sumn <- sum(DIN$nochanges, na.rm=TRUE)
    if(! silent) message("countLMdfs: remember that counts of NAs (record gaps or zero flows) ",
                         "are not included.\n",  "    Totals: decreases=",sumd,
                         ", increases=",sumi, ", and nochanges=", sumn, appendLF=FALSE)
    if(sumd > sumi & sumd > sumn & sumi > sumn) {
       if(! silent) message(" and data.frame argument ordering seems consistent.")
    } else {
       warning("ordering of the data frame arguments seems inconsistent, ",
               "check again for DECR (decreasing), INCR (increasing), and NC (nochange)")
    }
    tmp <- DIN[! complete.cases(DIN),]
    attr(DIN, "skipped_sites") <- as.character(tmp$site)
    attr(DIN, "which") <- which
    return(DIN)
}


