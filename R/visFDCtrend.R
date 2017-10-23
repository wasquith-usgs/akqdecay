"visFDCtrend" <- function(fdcenv=NULL, file=NA, alpha=0.05, fast=FALSE, ...) {
   if(is.null(fdcenv)) {
      warning(" need to specify the fdcenv environment")
      return()
   } else if(is.data.frame(fdcenv)) {
      df <- fdcenv; fdcenv <- new.env()
      assign("site", df, envir=fdcenv)
   } else if(! is.environment(fdcenv)) {
      warning(" fdcenv is not testing as an environment")
      return()
   }
   LargeSampleVarKendallTau <- function(n) 2*(2*n+5)/(9*n*(n-1))
   if(! fast) {
      ALLz <- NULL
      MX <- data.frame(site=NA, years=NA, min=NA, max=NA)
   }
   if(! is.na(file)) pdf(file, useDingbats=FALSE)
   pars <- par(...)
   for(station in sort(ls(fdcenv))) {
      message(station, appendLF=FALSE)
      Z <- get(station, envir=fdcenv)
      if(length(Z) == 1) {
         message("\n Missing daily values for ", station)
         Z <- data.frame(DAY=NA, prob=NA, estimate=NA, p.value=NA)
      }
      nm <- names(Z); yrs <- nm[2:(length(nm)-3)]; n <- length(yrs)
      txt  <- paste0(station,": ",yrs[1],"-",yrs[n]," (",n," yrs)")
      site <- rep(station, 365); count <- rep(n, 365)

      if(! fast) {
         w  <- length(Z[1,]); tmp <- cbind(site, count, Z[,(w-2):w])
         ifelse(is.null(ALLz), ALLz <- tmp, ALLz <- rbind(ALLz, tmp))
         tmp <- Z$estimate[! is.na(Z$estimate)]
         if(length(tmp) != 0) { # a suppressWarnings() wrapper on the min/max does not silence
            mn <- min(tmp, na.rm=TRUE)
            mx <- max(tmp, na.rm=TRUE)
         } else {
            message("\n No 'complete' FDCs for ", station)
            mn <- mx <- Inf
         }
         if(! is.finite(mn)) mn <- NA; if(! is.finite(mx)) mx <- NA
         MX <- rbind(MX, c(station, w-4, mn, mx))
      }

      col <- as.numeric(Z$p.value < alpha)
      col[col == 0] <- 4; col[col == 1] <- 2

      # na.rm=TRUE traps on cases where zeros are the only obvserved values
      # Extract the smallest magnitude of significant estimates. The presence
      # of ties weakens the approach for drawing the upper and lower threshold
      # because there exists a chance that some points will be plotted blue
      # but be outside of the drawn limits. The p-value for each estimate
      # should be used to measure significance as is done here.
      suppressWarnings(sigTau <- min(abs(Z$estimate[col == 2]), na.rm=TRUE))

      xlim <- c(0,1); ylim <- c(-1,1)
      plot(0, 0, xlim=xlim, ylim=ylim, xaxs="i", yaxs="i", type="n",
           xlab="Annual daily-flow-duration nonexceedance probability",
           ylab="Kendall's tau, dimensionless")
      abline(0,0, lty=2, col=8, lwd=1.1)
      if(! is.finite(sigTau)) { # note use of alpha/2 here but alpha above
         sigTau <- qnorm(1-alpha/2, mean=0,
                         sd=sqrt(LargeSampleVarKendallTau(n)))
      }
      lines(c(0,1), rep(-sigTau, 2), col=6, lty=2, lwd=1.1)
      lines(c(0,1), rep(+sigTau, 2), col=6, lty=2, lwd=1.1)
      points(Z$prob, Z$estimate, col=col)
      text(0,0.94, txt, pos=4, lwd=0.80, cex=0.85)
      legend(0.02,-.75, c(paste0("Upper and lower Kendall's tau at significance ",
                             "level alpha=", alpha), "Origin line (no association)",
                      paste0("Kendall's tau for the corresponding probability ",
                             "(red if statistically significant)")),
             lwd=c(1.1, 1.1, 0.80), pch=c(NA,NA,1), lty=c(2,2,0), col=c(6,8,4),
             cex=0.85, bty="n")
      message(", ", appendLF=FALSE)
   }
   message("done")
   if(! is.na(file)) dev.off()
   suppressWarnings(par(pars))

   if(! fast) {
      MX <- MX[-1,]; MX$years <- as.numeric(MX$years);
      MX$max <- as.numeric(MX$max); MX$min <- as.numeric(MX$min)
      row.names(MX) <- 1:length(MX$site)

      tmp <- MX[complete.cases(MX),]
      tmp <- tmp[tmp$min == min(tmp$min, na.rm=TRUE),]; signstr <- "+"
      if(! is.na(tmp$min[1]) & tmp$min[1] < 0)          signstr <- ""
      message("Global: Least minimum estimate is for ", paste(tmp$site, collapse=", "),
              " at Tau=", signstr, round(tmp$min[1], digits=4), " (",tmp$years," yrs)")

      tmp <- MX[complete.cases(MX),]
      tmp <- tmp[tmp$max == max(tmp$max, na.rm=TRUE),]; signstr <- "+"
      if(! is.na(tmp$max[1]) & tmp$max[1] < 0)          signstr <- ""
      message("Global:  Most maximum estimate is for ", paste(tmp$site, collapse=", "),
              " at Tau=", signstr, round(tmp$max, digits=4), " (",tmp$years," yrs)")
      return(ALLz)
   } else {
     return("fast option used")
   }
}

