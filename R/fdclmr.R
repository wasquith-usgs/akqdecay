"fdclmr" <-
function(akdvtable, missing.days=7, site="",
                    log=TRUE, subzero=0.001, verbose=FALSE, ...) {
  if(length(unique(akdvtable$site_no)) > 1) {
    warning("can not have move than one streamgage in the daily value table, please ",
            "consult fill_dvenv() for multiple streamgage processing, ",
            "returning NA immediately")
    return(NA)
  }
  site[1] <- as.character(site[1])

  if(length(akdvtable$year) == 0) return(NA)

  empty <- list(lambdas=rep(NA, 6), ratios=rep(NA, 6), source="not valid")
  avail <- 365 - missing.days
  akdvtable <- akdvtable[order(akdvtable$Date),]
  year      <- akdvtable$year; years <- unique(year)
  zz <- data.frame(site=site, year=NA, n=NA, nzero=NA,
                   L1=NA, L2=NA, T3=NA, T4=NA, T5=NA, T6=NA)
  zz <- zz[0,]
  for(y in years) {
     if(verbose) message("    ", site," -- ",y)
     fdc  <- akdvtable$Flow[year == y]; nzero <- length(fdc[fdc == 0])
     if(log) {
        nzero <- length(fdc[fdc == 0])
        if(subzero) fdc[fdc == 0] <- subzero
        fdc <- log10(fdc); fdc <- fdc[is.finite(fdc)]
     }
     n <- length(fdc[! is.na(fdc)])
     if(any(is.na(fdc))) {
        message("a least one missing value for year ",y)
        lmr <- empty
     } else if (length(unique(fdc)) == 1) {
        lmr <- empty
     } else {
        if(n <= avail) {
           lmr <- empty
        } else {
           lmr <- lmomco::lmoms(fdc, nmom=6, no.stop=TRUE)
           if(! lmomco::are.lmom.valid(lmr)) {
              lmr <- lmomco::pwm2lmom(lmomco::pwm.pp(fdc, nmom=6))
           }
        }
     }
     tmp <- data.frame(site=site, year=y, n=n, nzero=nzero,
                       L1=lmr$lambdas[1], L2=lmr$lambdas[2], T3=lmr$ratios[3],
                       T4=lmr$ratios[4], T5=lmr$ratios[5], T6=lmr$ratios[6])
     zz <- rbind(zz, tmp)
  }
  return(zz)
}




