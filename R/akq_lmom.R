"akq_lmom" <- function(akqenv, silent=FALSE, ...) {
  if(is.null(akqenv)) {
     warning(" need to specify the akqenv environment")
     return()
  } else if(! is.environment(akqenv)) {
     return(akqenv$lmoments$por)
  }
  skips <- 0; SKIPS <- NA
  SITES <- sort(ls(akqenv)); n <- length(SITES)
  NC <- L1 <- L2 <- T3 <- T4 <- T5 <- T6 <- rep(NA, n)
  yr_range_str <- MED <- L1L2 <- GF <- GFE <- NC
  for(i in 1:n) {
    if(! silent) message("L-moments (POR) for ", SITES[i], " (",i,"/",n,")", appendLF = FALSE)
    Z <- get(SITES[i], envir=akqenv)
    if(length(Z) == 1) {
       if(! silent) message(" -- none available"); next
    } else {
       if(! silent) message(" -- got'em")
    }
    lmr <- Z$lmoments$por
    if(length(lmr) == 1) {
       if(! silent) message("    skipping L-moment table because it is NA")
       skips <- skips + 1; SKIPS[skips] <-SITES[i]
       next
    }
    NC[i] <- lmr$count
    yr_range_str[i] <- lmr$yr_range_str
    MED[i] <- lmr$median;  L1L2[i] <- lmr$L1L2
    GF[i]  <- lmr$gfactor;  GFE[i] <- lmr$gfactor_emp
    L1[i]  <- lmr$L1; L2[i] <- lmr$L2
    T3[i]  <- lmr$T3; T4[i] <- lmr$T4; T5[i] <- lmr$T5; T6[i] <- lmr$T6
  }
  zz <- data.frame(site=SITES, yr_range_str=yr_range_str,
              count=NC, median=MED, L1L2=L1L2,
              gfactor=GF, gfactor_emp=GFE,
              L1=L1, L2=L2, T3=T3, T4=T4, T5=T5, T6=T6, stringsAsFactors=FALSE)
  if(! silent) {
    if(skips > 0) {
      message("  SKIPPED ", skips, " sites with NA lmoments$por table (degenerate circumstances)")
    }
  }
  attr(zz, "skipped_sites") <- SKIPS
  return(zz)
}


"akq_lmom_yeardecade" <-
function(akqenv, which=c("year", "decade"), silent=FALSE, tauenv=NULL, ...) {
  which <- match.arg(which)
  lmr <- NULL
  if(is.null(akqenv)) {
     warning(" need to specify the akqenv environment")
     return()
  } else if(! is.environment(akqenv)) {
     if(which == "year") {
       lmr <- akqenv$lmoments$by_year
     } else if(which == "decade") {
       lmr <- akqenv$lmoments$by_decade
     } else {
       stop("should not be here in logic")
     }
  }
  if(! is.null(lmr)) {
     nm <- names(lmr); m <- length(nm); nm <- nm[3:m]
     opts <- options(warn=-1)
     ktau <- lapply(3:m, function(j) {
                 k <- length(lmr[,2])
                 if(k <= 2) return(list(estimate=NA, p.value=NA, sample.size=k))
                 zz <- NULL # the try() is used to capture problems in complete lmr tables that k <= 2 will not catch
                 try(zz <- cor.test(lmr[,2], lmr[,j], method="kendall"))
                 if(is.null(zz)) return(list(estimate=NA, p.value=NA, sample.size=k))
                 zz$sample.size <- k; return(zz)
             })
     options(opts)
     taus <- sapply(1:length(ktau), function(j) ktau[[j]]$estimate)
     pvs  <- sapply(1:length(ktau), function(j) ktau[[j]]$p.value)
     sam  <- sapply(1:length(ktau), function(j) ktau[[j]]$sample.size)
     names(taus) <- nm; names(pvs) <- nm; names(sam) <- nm
     attr(lmr, "tau") <- taus; attr(lmr, "tau_p.value") <- pvs; attr(lmr, "tau_sample.size") <- sam
     return(lmr)
  }
  skips <- 0; SKIPS <- NA; t <- 0
  SITES <- ls(akqenv); n <- length(SITES); TAUSITES <- NA
  TrendT <- TrendP <- TrendS <- NULL;
  for(i in 1:n) {
    if(! silent) message(" L-moments (",which,") for ", SITES[i], " (",i,"/",n,")", appendLF = FALSE)
    Z <- get(SITES[i], envir=akqenv)
    if(length(Z) == 1) {
       if(! silent) message(" -- none available"); next
    } else {
       if(! silent) message(" -- got'em")
       t <- t + 1; TAUSITES[t] <- SITES[i]
    }
    if(which == "year") {
      lmr <- Z$lmoments$by_year
    } else if(which == "decade") {
      lmr <- Z$lmoments$by_decade
    } else {
      stop("should not be here in logic")
    }
    if(length(lmr) == 1) {
       if(! silent) message("    skipping L-moment table because it is NA")
       skips <- skips + 1; SKIPS[skips] <-SITES[i]
       if(which == "year") {
          lmr <- data.frame(site=SITES[i], year=NA, count=NA, median=NA, L1L2=NA,
                            gfactor=NA, gfactor_emp=NA, L1=NA, L2=NA, T3=NA, T4=NA, T5=NA, T6=NA)
       } else if(which == "decade") {
          lmr <- data.frame(site=SITES[i], decade=NA, count=NA, median=NA, L1L2=NA,
                            gfactor=NA, gfactor_emp=NA, L1=NA, L2=NA, T3=NA, T4=NA, T5=NA, T6=NA)
       }
    }
    if(! is.null(tauenv)) {
       nm <- names(lmr); m <- length(nm); nm <- nm[3:m]
       opts <- options(warn=-1)
       ktau <- lapply(3:m, function(j) {
                 k <- length(lmr[,2])
                 if(k <= 2) return(list(estimate=NA, p.value=NA, sample.size=k))
                 zz <- NULL # the try() is used to capture problems in complete lmr tables that k <= 2 will not catch
                 try(zz <- cor.test(lmr[,2], lmr[,j], method="kendall"))
                 if(is.null(zz)) return(list(estimate=NA, p.value=NA, sample.size=k))
                 zz$sample.size <- k
                 return(zz)
               })
       options(opts)
       taus <- sapply(1:length(ktau), function(j) ktau[[j]]$estimate)
       pvs  <- sapply(1:length(ktau), function(j) ktau[[j]]$p.value)
       sam  <- sapply(1:length(ktau), function(j) ktau[[j]]$sample.size)
       names(taus) <- nm; names(pvs) <- nm; names(sam) <- nm
       #if(length(TrendT[,1])+1 != i) {
       #  print(tail(TrendT)); message(" broken:", SITES[i]); stop()
       #} else {
       #  message(SITES[i])
       #}
       TrendT <- rbind(TrendT, taus); TrendP <- rbind(TrendP, pvs); TrendS <- rbind(TrendS, sam)
    }
    if(i == 1) {
      BIG <- lmr
    } else {
      BIG <- rbind(BIG, lmr)
    }
  }
  if(! silent) message("  SKIPPED ", skips, " sites with NA lmoment (year or decade) table (degenerate circumstances)")
  if(! is.null(tauenv)) {
    rownames(TrendT) <- NULL;        rownames(TrendP) <- NULL;        rownames(TrendS) <- NULL
    TrendT <- as.data.frame(TrendT); TrendP <- as.data.frame(TrendP); TrendS <- as.data.frame(TrendS)
    TrendT$type <- "tau";            TrendP$type <- "tau_p.value";    TrendS$type <- "tau_sample.size"
    TrendT$site <- TAUSITES;         TrendP$site <- TAUSITES;         TrendS$site <- TAUSITES
    w <- length(colnames(TrendT))
    TrendT <- TrendT[,c(w,w-1,1:(w-2))]
    TrendP <- TrendP[,c(w,w-1,1:(w-2))]
    TrendS <- TrendS[,c(w,w-1,1:(w-2))]
    if(! is.environment(tauenv)) {
       warning(" tauenv is not an environment, skipping insertion of Kendall Tau test")
    } else {
       assign("tau",             TrendT, envir=tauenv)
       assign("tau_p.value",     TrendP, envir=tauenv)
       assign("tau_sample.size", TrendS, envir=tauenv)
    }
  }
  attr(BIG, "skipped_sites") <- SKIPS
  ifelse(which == "year", attr(BIG, "which") <- "year", attr(BIG, "which") <- "decade")
  return(BIG)
}

"akq_lmom_year" <- function(akqenv, ...) {
   akq_lmom_yeardecade(akqenv, which="year", ...)
}

"akq_lmom_decade" <- function(akqenv, ...) {
   akq_lmom_yeardecade(akqenv, which="decade", ...)
}
