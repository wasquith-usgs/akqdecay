"fdctrend" <-
function(akdvtable, missing.days=7, type=5, ...) {
  if(length(unique(akdvtable$site_no)) > 1) {
    warning("can not have move than one streamgage in the daily value table, please ",
            "consult fill_dvenv() and fill_akqenv() for multiple streamgage processing, ",
            "returning NA immediately")
    return(NA)
  }

  if(length(akdvtable$year) == 0) return(NA)

  if(type < 4 | type > 9) {
     warning("invalid type, must be [4,9] for quantile functionJ")
     return(NA)
  }
  dd <- 1:365
  if(type == 4) FF <-  dd       / 365
  if(type == 5) FF <- (dd - 0.5)/ 365
  if(type == 6) FF <-  dd       /(365 + 1)
  if(type == 7) FF <- (dd - 1  )/(365 - 1)
  if(type == 8) FF <- (dd - 1/3)/(365 - 1/3)
  if(type == 9) FF <- (dd - 3/8)/(365 + 1/4)

  empty <- rep(NA, 365); avail <- 365 - missing.days
  zz <- data.frame(day=dd, stringsAsFactors=FALSE)
  akdvtable <- akdvtable[order(akdvtable$Date),]
  year      <- akdvtable$year; years <- unique(year)

  for(y in years) {
     fdc  <- akdvtable$Flow[year == y]; n <- length(fdc[! is.na(fdc)])
     if(any(is.na(fdc))) {
        message("a least one missing value for year ",y)
        ppos <- empty
     } else {
        ifelse(n <= avail, ppos <- empty,
                           ppos <- quantile(fdc, probs=FF, type=type))
     }
     zz <- cbind(zz,ppos)
  }
  names(zz) <- c("DAY", years)

  empty <- list(statistic =NA, parameter  =NA, p.value=NA, estimate =NA,
                null.value=NA, alternative=NA,  method=NA, data.name=NA)
  nys <- length(years)
  opts <- options(warn=-1)
  ktau <- sapply(1:365, function(i) { y <- t(zz[i,2:(nys+1)])
                                    if(length(y[! is.na(y)]) <= 2) return(empty)
                                    cor.test(1:nys, y, method="kendall", ...) })
  options(opts)
  ktau <- as.data.frame(ktau); names(ktau) <- row.names(zz)
  tau <- t(as.vector(ktau[4,])); pv <- t(as.vector(ktau[3,]))

  k <- NA # tracking the indices of all NA columns for removal
  for(i in 2:length(zz[1,])) if(all(is.na(zz[,i]))) k[i] <- i
  zz[,k[! is.na(k)]] <- NULL # remove them all at once

  zz$prob <- FF; zz$estimate <- unlist(tau); zz$p.value <- unlist(pv)
  attr(zz, "site") <- akdvtable$site_no
  return(zz)
}




