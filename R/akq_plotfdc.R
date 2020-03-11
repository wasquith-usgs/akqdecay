"akq_plotfdc" <-
function(gagefdc, site="", file=NA, showtitle=TRUE, ylim=NULL, ...) {
  if(! is.data.frame(gagefdc)) {
    message("ALERT: empty 'gagefdc' for site=",site)
    return(NULL)
  }
  fdc  <- as.data.frame(gagefdc[,c(7:33)])        # extracting the "f50" (median) etc column names
  ff   <- as.numeric(gsub("f","",names(fdc)))/100 # stripping the leading "f" and converted from percent
  ff   <- qnorm(ff)                               # transformation to standard normal variates
  fdc  <- as.data.frame(t(as.matrix(fdc)))        # some data manipulation
  names(fdc) <- as.character(gagefdc$decade) # and finally naming the flow-durations curves by decade
  for(i in rev(1:length(fdc[1,]))) {
    if(is.na(fdc[1,i])) fdc[,i] <- NULL
  }
  decades <- names(fdc)
  if(length(decades) == 0) {
    if(! is.na(file)) pdf(file, useDingbats=FALSE, height=6.5, width=7)
      plot(0:2,0:2, xlab="", ylab="", xaxt="n", yaxt="n", type="n")
      text( 1,  1,  "EMPTY DECADAL FDC INFORMATION")
      if(showtitle) mtext(paste0("STREAMGAGE: ",site))
    if(! is.na(file)) dev.off()
  } else {

  if(is.null(ylim)) {
    ylim <- range(fdc)
    flr <- 10^(floor(log10(ylim[1]))-1)
    flr <- ifelse(flr == 0, 0.01, flr)
    ylim <- c(flr, 10^ceiling(log10(ylim[2]))); rm(flr)
  }

  if(! is.na(file)) pdf(file, useDingbats=FALSE, height=6.5, width=7)
    opts <- par(no.readonly=TRUE); par(las=1, mgp=c(3,0.5,0))

    plot(1,1, xlim=qnorm(c(0.0001, 0.9999)), ylim=ylim,
              xaxt="n", yaxt="n", xlab="", log="y", type="n",
              xaxs="i", yaxs="i", ylab="Streamflow, cfs")
    lmomco::add.log.axis(side=2,    tcl=0.8*abs(par()$tcl), two.sided=TRUE)
    lmomco::add.log.axis(logs=c(1), tcl=+1.3*abs(par()$tcl), side=2, two.sided=TRUE)
    lmomco::add.log.axis(logs=c(1, 2, 4, 6), side=2, make.labs=TRUE, las=1, label="")
    lmomco::add.lmomco.axis(las=2,  tcl=0.5, side.type="NPP", cex=0.8, case="lower")

    ks <- seq(0.3,0.15*length(decades)+0.3, by=0.15)
    cols <- rev(ks/max(ks)) - 0.05
    cols[cols > 0.85] <- 0.85
    cols[cols < 0.10] <- 0.10
    for(i in 1:length(decades)) {
      decade <- decades[i]
      gmp <- fdc[,i]; gmp[gmp == 0] <- 0.001
      lty <- ifelse(decade <= 1950, 2, 1)
      lines(ff,  gmp, col=grey(cols[i]), lty=lty)
      points(ff, gmp, pch=21, bg=grey(cols[i]), lwd=0.7, cex=ks[i])
    }

    legend(2,10^mean(par()$usr[3:4]),
                  paste(decades,"s", sep=""), cex=0.7, bty="n", pch=21,
                  pt.cex=ks, lty=0, pt.bg=grey(cols),
                  title="Flow-duration\ncurve (FDC)\nquantile")


    txt <- c(paste0("Decadal FDC with circle size and greyness increasing\n",
                    "by decade from ", decades[       1       ],"s to ",
                                       decades[length(decades)],"s,",
                    "where dashed lines,\n",
                    "if present, indicate 1950 decade and earlier"))
    legend(-3.3, 10^(par()$usr[4] - 0.05*(diff(par()$usr[3:4]))), txt,
           pt.bg=grey(0.7), bty="n", lwd=0.7, pch=21, cex=0.8, pt.cex=0.9)
    if(showtitle) mtext(paste0("STREAMGAGE: ",site))
    par(opts)
  if(! is.na(file)) dev.off()
  }
}

#site <- "08167000"
#gage  <- fdclmr(dvget(site, ignore.provisional=FALSE), decade=TRUE)
#akq_plotfdc(gage, site=site)
