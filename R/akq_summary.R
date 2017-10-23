"akq_summary" <- function(akqenv, silent=FALSE, ...) {
    if(is.null(akqenv)) {
      warning(" need to specify the akqenv environment")
      return()
   } else if(! is.environment(akqenv)) {
      return(akqenv$summary)
   }
  skips <- 0
  SITES <- sort(ls(akqenv)); n <- length(SITES)
  STR <- vector(mode="character")
  BEG <- END <- STR <- TN <- N <- KT <- SR <- MED <- L1L2 <- GF <- GFE <- rep(NA, n)
  for(i in 1:n) {
    if(! silent) message(" Summaries for ", SITES[i], " (",i,"/",n,")", appendLF = FALSE)
    Z <- get(SITES[i], envir=akqenv)
    if(length(Z) == 1) {
       if(! silent) message(" -- none available"); next
    } else {
       if(! silent) message(" -- got'em")
    }

    SUM <- Z$summary
    if(is.na(SUM[1])) {
       if(! silent) message("    skipping summary table because it is NA")
       skips <- skips + 1
       next
    }
    BEG[i] <- SUM$beg_year; END[i] <- SUM$end_year; STR[i] <- SUM$yr_range_str
    TN[i]  <- as.integer(SUM$total_count)
    N[i]   <- as.integer(SUM$count)
    KT[i]  <- SUM$kendall_tau;    SR[i] <- SUM$spearman_rho
    MED[i] <- SUM$median;       L1L2[i] <- SUM$L1L2
    GF[i]  <- SUM$gfactor;       GFE[i] <- SUM$gfactor_emp
  }
  zz <- data.frame(site=SITES, beg_year=BEG, end_year=END, yr_range_str=STR,
               total_count=TN, count=N, kendall_tau=KT, spearman_rho=SR,
               median=MED, L1L2=L1L2, gfactor=GF, gfactor_emp=GFE, stringsAsFactors=FALSE)
  if(! silent) message("  SKIPPED ", skips, " sites with NA summary tables (degenerate circumstances)")
  return(zz)
}
