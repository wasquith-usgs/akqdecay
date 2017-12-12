"fdclmr" <-
function(akdvtable, missing.days=7, site="", decade=FALSE,
                    log=FALSE, subzero=NULL, plusit=1, verbose=FALSE, ...) {
  if(length(unique(akdvtable$site_no)) > 1) {
    warning("can not have move than one streamgage in the daily value table, please ",
            "consult fill_dvenv() for multiple streamgage processing, ",
            "returning NA immediately")
    return(NA)
  }
  if(is.null(subzero) & is.null(plusit)) {
    warning("subzero and plusit both set (! NULL), not certain if a good idea")
  }
  site[1] <- as.character(site[1])

  if(length(akdvtable$year) == 0) return(NA)

  probs <- c(0.02, 0.05, 0.1, 0.2, 0.5, 1, 2, 5, 10, 20, 25, 30, 40, 50,
             60, 70, 75, 80, 90, 95, 98, 99, 99.5, 99.8, 99.9, 99.95, 99.98)
  probs <- probs/100
  if(decade) {
     yds <- unique(akdvtable$decade)
     avail <- 365*10 - missing.days*10
     empty <- list(fdc_quantiles=rep(NA, length(probs)),  min=NA, max=NA, pplo=NA,
                   lambdas=rep(NA, 6), ratios=rep(NA, 6), source="not valid")
     zz <- data.frame(site=site, decade=NA, n=NA, nzero=NA, pplo=NA,
          min=NA,  f0.02=NA, f0.05=NA, f0.1=NA, f0.2=NA, f0.5=NA, f01=NA,
          f02=NA, f05=NA, f10=NA, f20=NA, f25=NA, f30=NA, f40=NA, f50=NA, f60=NA,
          f70=NA, f75=NA, f80=NA, f90=NA, f95=NA, f98=NA, f99=NA, f99.5=NA,
          f99.8=NA, f99.9=NA, f99.95=NA, f99.98=NA, max=NA,
          L1=NA, L2=NA, T3=NA, T4=NA, T5=NA, T6=NA)
     zz <- zz[0,]
  } else {
     yds <- unique(akdvtable$year)
     avail <- 365 - missing.days
     empty <- list(median=NA, min=NA, max=NA, min7day=NA, max7day=NA, pplo=NA,
                   lambdas=rep(NA, 6), ratios=rep(NA, 6), source="not valid")
     zz <- data.frame(site=site, year=NA, n=NA, nzero=NA, pplo=NA,
                      min=NA, median=NA, max=NA, min7day=NA, max7day=NA,
                      L1=NA, L2=NA, T3=NA, T4=NA, T5=NA, T6=NA)
     zz <- zz[0,]
  }

  for(yd in yds) {  # yd means year_or_decade
     if(verbose) message("    ", site," -- ",yd)
     ifelse(decade, fdc <- akdvtable$Flow[akdvtable$decade == yd],
                    fdc <- akdvtable$Flow[akdvtable$year   == yd])
     nzero <- length(fdc[fdc == 0])
     n <- length(fdc[! is.na(fdc)]); fdc <- fdc[! is.na(fdc)]
     if(log) {
        nzero <- length(fdc[fdc == 0])
        if(! is.null(subzero)) fdc[fdc == 0] <- subzero
        if(! is.null(plusit )) fdc <- fdc + plusit
        opts <- options(warn=-1)
          fdc <- log10(fdc)
          if(length(fdc[is.nan(fdc)]) > 0) message("  NaN -- ", site, " for ", yd)
          fdc <- fdc[is.finite(fdc)]
        options(opts)
     }
     if(any(is.na(fdc))) {
        message("a least one missing value for year or decade ",yd)
        lmr <- empty
     } else if (length(unique(fdc)) == 1) {
        lmr <- empty
     } else {
        if(n <= avail) {
           lmr <- empty
        } else {
           fdclo <- lmomco::x2xlo(fdc)
           #print(yd); print(fdclo$xin)
           lmr <- lmomco::lmoms(fdclo$xin, nmom=6, no.stop=TRUE)
           if(! lmomco::are.lmom.valid(lmr)) {
              lmr <- lmomco::pwm2lmom(lmomco::pwm.pp(fdclo$xin, nmom=6))
           }
           lmr$pplo <- fdclo$pp
           lmr$min <- min(fdc); lmr$max <- max(fdc)
           if(decade) {
             lmr$fdc_quantiles <- quantile(fdc, probs=probs, type=6)
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
                      min=lmr$min, f0.02=q[1], f0.05=q[2], f0.1=q[3], f0.2=q[4],
                      f0.5=q[5], f01=q[6],  f02=q[7],  f05=q[8],  f10=q[9],
                      f20=q[10], f25=q[11], f30=q[12], f40=q[13], f50=q[14],
                      f60=q[15], f70=q[16], f75=q[17], f80=q[18], f90=q[19],
                      f95=q[20], f98=q[21], f99=q[22], f99.5=q[23], f99.8=q[24],
                      f99.9=q[25], f99.95=q[26], f99.98=q[27], max=lmr$max,
                      L1=lmr$lambdas[1], L2=lmr$lambdas[2], T3=lmr$ratios[3],
                      T4=lmr$ratios[4],  T5=lmr$ratios[5],  T6=lmr$ratios[6])
     } else {
      tmp <- data.frame(site=site, year=yd, n=n, nzero=nzero, pplo=lmr$pplo,
                       min=lmr$min, median=lmr$median, max=lmr$max,
                       min7day=lmr$min7day, max7day=lmr$max7day,
                       L1=lmr$lambdas[1], L2=lmr$lambdas[2], T3=lmr$ratios[3],
                       T4=lmr$ratios[4], T5=lmr$ratios[5], T6=lmr$ratios[6])
     }
     zz <- rbind(zz, tmp)
     row.names(zz) <- NULL
  }
  return(zz)
}


# from USGS SIR 2014--5231
#
#probs <- c(0.02, 0.05, 0.1, 0.2, 0.5, 1, 2, 5, 10, 20, 25, 30, 40, 50,
#           60, 70, 75, 80, 90, 95, 98, 99, 99.5, 99.8, 99.9, 99.95, 99.98)

