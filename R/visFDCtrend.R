"visFDCtrend" <- function(fdcenv=NULL, file=NA, alpha=0.05,
                           fast=FALSE, site=NA,
                           notitle=FALSE, showflowregime=TRUE,
                           width=7, height=5, ...) {
   was.data.frame <- FALSE
   if(is.null(fdcenv)) {
      warning(" need to specify the fdcenv environment")
      return()
   } else if(is.data.frame(fdcenv)) {
      was.data.frame <- TRUE
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
   if(! is.na(file)) pdf(file, useDingbats=FALSE, width=width, height=height)
   pars <- par(..., mgp=c(2,0.25,0)) #"parOFmine" <- function() par(las=1, mgp=c(2,0.25,0), tcl=0.5)

   for(station in sort(ls(fdcenv))) {
      message(station, appendLF=FALSE)
      Z <- get(station, envir=fdcenv)
      if(length(Z) == 1) {
         message("\n Missing daily values for ", station)
         Z <- data.frame(DAY=NA, prob=NA, estimate=NA, p.value=NA)
      }
      nm <- names(Z); yrs <- nm[2:(length(nm)-3)]; n <- length(yrs)
      station.text <- station
      if(was.data.frame && ! is.na(site)) station.text <- site
      txt <- paste0(station.text,": ",yrs[1],"-",yrs[n],
                    " (",n," complete calendar years)")
      site <- rep(station.text, 365); count <- rep(n, 365)

      if(! fast) {
         w  <- length(Z[1,]); tmp <- cbind(site, count, Z[,(w-2):w])
         ifelse(is.null(ALLz), ALLz <- tmp, ALLz <- rbind(ALLz, tmp))
         tmp <- Z$estimate[! is.na(Z$estimate)]
         if(length(tmp) != 0) { # a suppressWarnings() wrapper on the min/max does not silence
            mn <- min(tmp, na.rm=TRUE)
            mx <- max(tmp, na.rm=TRUE)
         } else {
            message("\n No 'complete' calendar year FDCs for ", station)
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
           xaxt="n", yaxt="n",
           xlab="Annual daily-flow-duration nonexceedance probability",
           ylab="Kendall's tau, dimensionless", las=1, tcl=0.5)
      abline(0,0, lty=2, col=grey(0.25), lwd=1.1)
      if(! is.finite(sigTau)) { # note use of alpha/2 here but alpha above
         sigTau <- qnorm(1-alpha/2, mean=0,
                         sd=sqrt(LargeSampleVarKendallTau(n)))
      }
      lines(c(0,1), rep(-sigTau, 2), col=grey(0.25), lty=4, lwd=1.1)
      lines(c(0,1), rep(+sigTau, 2), col=grey(0.25), lty=4, lwd=1.1)
      points(Z$prob, Z$estimate, col=col)
      if(! notitle) mtext(txt, lwd=0.80, cex=0.95)
      legend(0.02,-.70, c(paste0("Upper and lower Kendall's tau at significance ",
                                 "level of alpha=", alpha), "Origin line (no association)",
                          paste0("Kendall's tau for the corresponding probability ",
                                 "(red if statistically significant)")),
             lwd=c(1.1, 1.1, 0.80), pch=c(NA,NA,1), lty=c(4,2,0), col=c(grey(0.25),grey(0.25),4),
             cex=0.85, bty="n")
      axis(1, labels=sprintf("%0.1f",axTicks(1)), at=axTicks(1), tcl=0.5, las=1, col=NA, col.ticks=1) # add ticks to left
      axis(3, labels=NA, at=axTicks(1), tcl=0.5, las=2, col=NA, col.ticks=1) # add ticks to top
      axis(2, labels=sprintf("%0.1f",axTicks(2)), at=axTicks(2), tcl=0.5, las=2, col=NA, col.ticks=1) # add ticks to left
      axis(4, labels=NA, at=axTicks(4), tcl=0.5, las=2, col=NA, col.ticks=1) # add ticks to top
      if(showflowregime) {
        text(c(0.1,0.5,0.9), rep(0.90,3), c("low flow\nregime",
                                            "median flow\nregime",
                                            "high flow\nregime"),
             cex=0.85, col=grey(0.10))
      }
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

