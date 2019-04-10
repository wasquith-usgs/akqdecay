"fdclmr" <-
function(akdvtable, missing.days=7, site="", decade=FALSE,
                    minyear=NA, maxyear=NA,
                    log=FALSE, subzero=NULL, plusit=0, verbose=FALSE, ...) {
  if(length(unique(akdvtable$site_no)) > 1) {
    warning("can not have more than one streamgage in the daily value table, please ",
            "consult fill_dvenv() for multiple streamgage processing, ",
            "returning NA immediately")
    return(NA)
  }
  if(is.null(subzero) & is.null(plusit)) {
    warning("subzero and plusit both set (! NULL), not certain if a good idea")
  }
  site[1] <- as.character(site[1]) # site is a special override on the site id

  if(! is.na(minyear)) akdvtable <- akdvtable[akdvtable$year >= minyear,]
  if(! is.na(maxyear)) akdvtable <- akdvtable[akdvtable$year <= maxyear,]
  if(length(akdvtable$year) == 0) return(NA)

  # probabilities from USGS SIR 2014--5231, but modified for 0.03 and 99.97 end points
  # and not 0.02 and 99.98 because (1/(10*365.25 + 1))*100 ===> 0.03 and
  # (1 - 1/(10*365.25 + 1))*100 == 99.97
  probs <- c(0.03, 0.05, 0.1, 0.2, 0.5, 1, 2, 5, 10, 20, 25, 30, 40, 50,
             60, 70, 75, 80, 90, 95, 98, 99, 99.5, 99.8, 99.9, 99.95, 99.97)
  probs <- probs/100 # make them fractions
  if(decade) {
     yds <- unique(akdvtable$decade) # note yds in a loop could be years or decades
     avail <- 365*10 - missing.days*10 # missing record allowance
     empty <- list(fdc_quantiles=rep(NA, length(probs)),  min=NA, max=NA, pplo=NA,
                   lambdas=rep(NA, 8), ratios=rep(NA, 8), median_nonzero=NA, source="not valid")
     zz <- data.frame(site=site, decade=NA, n=NA, nzero=NA, pplo=NA,
          min=NA,  f0.03=NA, f0.05=NA, f0.1=NA, f0.2=NA, f0.5=NA, f01=NA,
          f02=NA, f05=NA, f10=NA, f20=NA, f25=NA, f30=NA, f40=NA, f50=NA, f60=NA,
          f70=NA, f75=NA, f80=NA, f90=NA, f95=NA, f98=NA, f99=NA, f99.5=NA,
          f99.8=NA, f99.9=NA, f99.95=NA, f99.97=NA, max=NA,
          L1=NA, L2=NA, T3=NA, T4=NA, T5=NA, T6=NA, T7=NA, T8=NA, median_nonzero=NA)
     zz <- zz[0,]
  } else {
     yds <- unique(akdvtable$year) # note yds in a loop could be years or decades
     avail <- 365 - missing.days
     empty <- list(median=NA, min=NA, max=NA, min7day=NA, max7day=NA, pplo=NA,
                   lambdas=rep(NA, 8), ratios=rep(NA, 8), median_nonzero=NA, source="not valid")
     zz <- data.frame(site=site, year=NA, n=NA, nzero=NA, pplo=NA,
                      min=NA, median=NA, max=NA, min7day=NA, max7day=NA,
                      L1=NA, L2=NA, T3=NA, T4=NA, T5=NA, T6=NA, T7=NA, T8=NA, median_nonzero=NA)
     zz <- zz[0,]
  }

  # The akdvtable must has a decade, year, and Flow (daily mean flow) column.
  # All other information in that table is ignored (not used). NA protection
  # exists in the loop that follows.

  for(yd in yds) {  # yd means year_or_decade
     if(verbose) message("    ", site," -- ",yd)
     ifelse(decade, fdc <- akdvtable$Flow[akdvtable$decade == yd],
                    fdc <- akdvtable$Flow[akdvtable$year   == yd])
     nzero <- length(fdc[fdc <= 0])
     n <- length(fdc[! is.na(fdc)]); fdc <- fdc[! is.na(fdc)]
     if(log) { # if a user needs logarithms
        if(! is.null(subzero)) fdc[fdc <= 0] <- subzero
        if(! is.null(plusit )) fdc <- fdc + plusit
        nzero <- length(fdc[fdc <= 0])
        opts <- options(warn=-1)
          fdc <- log10(fdc)
          if(length(fdc[is.nan(fdc)]) > 0) message("  NaN -- ", site, " for ", yd)
          fdc <- fdc[is.finite(fdc)]
        options(opts)
     }
     if(any(is.na(fdc))) {
        message("a least one missing value for year or decade ",yd)
        lmr <- empty
     } else if(length(unique(fdc)) == 1) {
        lmr <- empty
     } else {
        if(n <= avail) {
           lmr <- empty
        } else {
           fdclo <- lmomco::x2xlo(fdc) # L-moments of zero left-truncation
           #print(yd); print(fdclo$xin) # The xin are the values "left in" or nonzero.
           lmr <-         lmomco::lmoms(fdclo$xin, nmom=8, no.stop=TRUE)
           lmr$median_nonzero <- median(fdclo$xin) # alert median of the nonzero part
           if(! lmomco::are.lmom.valid(lmr)) { # bailout to probability weighted moment
              lmr <- lmomco::pwm2lmom(lmomco::pwm.pp(fdclo$xin, nmom=8)) # by plotting
           } # position L-moments. This ensures maximal availability of the L-moments.
           lmr$pplo <- fdclo$pp # The percentage of no flow
           lmr$min <- min(fdc); lmr$max <- max(fdc)
           if(decade) {
             lmr$fdc_quantiles <- quantile(fdc, probs=probs, type=6) # Weibull type
           } else {
             lmr$median <- median(fdc)
             fdc7 <- sapply(1:(n-6), function(i) { mean(fdc[i:(i+6)]) })
             lmr$min7day <- min(fdc7); lmr$max7day <- max(fdc7)
           }
        }
     }
     if(decade) { #print(lmr)
            q <- lmr$fdc_quantiles
            tmp <- data.frame(site=site, decade=yd, n=n, nzero=nzero, pplo=lmr$pplo,
                      min=lmr$min, f0.03=q[1], f0.05=q[2], f0.1=q[3], f0.2=q[4],
                      f0.5=q[5], f01=q[6],  f02=q[7],  f05=q[8],  f10=q[9],
                      f20=q[10], f25=q[11], f30=q[12], f40=q[13], f50=q[14],
                      f60=q[15], f70=q[16], f75=q[17], f80=q[18], f90=q[19],
                      f95=q[20], f98=q[21], f99=q[22], f99.5=q[23], f99.8=q[24],
                      f99.9=q[25], f99.95=q[26], f99.97=q[27], max=lmr$max,
                      L1=lmr$lambdas[1], L2=lmr$lambdas[2], T3=lmr$ratios[3],
                      T4=lmr$ratios[4],  T5=lmr$ratios[5],  T6=lmr$ratios[6],
                      T7=lmr$ratios[7], T8=lmr$ratios[8], median_nonzero=lmr$median_nonzero,
                      stringsAsFactors=FALSE)
     } else {
      tmp <- data.frame(site=site, year=yd, n=n, nzero=nzero, pplo=lmr$pplo,
                       min=lmr$min, median=lmr$median, max=lmr$max,
                       min7day=lmr$min7day, max7day=lmr$max7day,
                       L1=lmr$lambdas[1], L2=lmr$lambdas[2], T3=lmr$ratios[3],
                       T4=lmr$ratios[4], T5=lmr$ratios[5], T6=lmr$ratios[6],
                       T7=lmr$ratios[7], T8=lmr$ratios[8], median_nonzero=lmr$median_nonzero,
                       stringsAsFactors=FALSE)
     }
     zz <- rbind(zz, tmp)
     row.names(zz) <- NULL
  }
  return(zz) # return a one to many rowed data.frame depending on years or decades available
}
