"dvpart" <-
function(akdvtable, sdate="", edate="", cda=NA, site_no=NA,
                    fillgaps=FALSE, na.negflow=FALSE, ...) {
  if(is.null(akdvtable)) {
    warning("akdvtable is NULL, returning NULL"); return(NULL)
  }
  # BEGIN EMBEDMENT (utility functions from other sources)
  "na2miss" <- # from smwrBase::na2miss()
  function(x, to=-99999) { # WHA removed the special factor handling
    x[is.na(x)] <- to; return(x) # because not needed in part()
  } # end na2miss() function
  "shiftData" <- # from smwrBase::shiftData()
  function(x, k=1, fill=NA, circular=FALSE) { # circular not needed but leave as is
    # Offset a vector by an amount equal to k
    # if k is positive the data are shifted down (fill at the beginning)
    # otherwise, they are shifted up (fill at the end)
    # unless circular is TRUE, then the data are treated as a circular
    # buffer, special handling of factors has been stripped for dvpart.
    #
    # Required to paste NAs at the beginning, this logic works for most data types
    fill.temp <- fill; fill <- x[1L]; fill[1L] <- fill.temp
    k <- as.integer(k)
    if(k == 0L) return(x)
    N <- length(x)
    if(k > 0L) {
      skip <- seq(k-1L, 0L) - N
      ifelse(circular, x <- c(x[-skip],     x[skip]),
                       x <- c(rep(fill, k), x[skip]))
    } else {
      skip <- seq(-1L, k)
      ifelse(circular, x <- c(x[skip], x[-skip]),
                       x <- c(x[skip], rep(fill, -k)))
    }
    return(x)
  } # end shiftData() function

  "eventNum" <- # from smwrBase::eventNum()
  function(event, reset=FALSE, na.fix=FALSE) {
    event[is.na(event)] <- na.fix
    event.rle <- rle(c(event, FALSE)) # Run Length Encoding
    number <- 0L
    ret.val <- rep(0L, length(event) + 1L)
    i <- 1L; beg <- 1L
    while(i < length(event.rle$values)) {
      if(event.rle$values[i]) {
        number <- number + 1L
        end <- beg + event.rle$lengths[i] + event.rle$lengths[i+1] - 1L
        ret.val[beg:end] <- number
        beg <- end + 1L; i <- i + 2L
      } else {
        beg <- event.rle$lengths[i] + 1L # can only be 1
        i <- i + 1L
      }
    }
    ret.val <- ret.val[seq(along=event)] # remove the last value
    if(reset)  ret.val <- ifelse(event, ret.val, 0L)
    return(ret.val)
  } # end eventNum function
  # END EMBEDMENT

  SMALL <- 1E-6 # new standardization from use by DVstats::part()
  if(is.na(site_no)) { # working on defaults etc
    if(length(unique(akdvtable$site_no)) != 1) {
      warning("multiple sites on the akdvtable, returning NULL")
      return(NULL)
    }
    site_no <- akdvtable$site_no[1]
  } else {
    if(length(site_no) != 1) {
      warning("multiple sites in site_no provided, returning NULL")
      return(NULL)
    }
  }

  dates <- akdvtable$Date # daily dates
  if(sdate == "") { # working on defaults etc
    sdate <- dates[1L]
  } else {
    sdate <- as.Date(as.character(sdate))
  }
  if(edate == "") {
    edate <- dates[length(dates)]
  } else {
    edate <- as.Date(as.character(edate))
  }

  akdvtable <- akdvtable[akdvtable$site_no == site_no,]
  sel <- (dates >= sdate) & (dates <= edate)
  akdvtable <- akdvtable[sel,]

  flows <- akdvtable$Flow # streamflows
  dates <- akdvtable$Date # daily dates

  flows <- pmax(flows, 1e-99) # Convert 0 to a small number (original PART logic)
  # Note that the above operation is destructive on negative flows by turning them
  # also to the 1e-99 number. Some streamgages do have occasional negative
  # streamflow --- not directly referring to tidal gages --- what exactly should
  # happen to hydrograph separation when negatives exist? No clear, so the
  # na.negflow argument to this function was added.

  original_number_of_flows <- length(flows)  # if desired someday to report changes
  if(fillgaps) { # Asquith hacking magic
    nova_core <- seq(sdate, edate, by=1) # every day needed
    number_of_nova_core <- length(nova_core) # if desired someday to report changes
    gap_infill_flow <- stats::approx(      dates[! is.na(flows)],
                                     log10(flows[! is.na(flows)]), xout=nova_core)$y
    # log-linear interpolated flows, connecting the dots across the gap
    dates <- nova_core # revise the dates we are going to PART
    flows <- 10^gap_infill_flow # return to real space
    dates <- dates[! is.na(flows)] # now drop NAs
    flows <- flows[! is.na(flows)] # now drop NAs
    # The NA check could result if say the beginning or ending of the
    # record is NA, the sdate and edate will have these
  } # end the algorithm for daily-continuous data processing

  num_flows <- length(flows)
  ixs <- 1:num_flows
  if(any(diff(as.double(dates)) != 1)) { # if fillgaps should cause this to not trigger
    dffs <- c(0,diff(as.double(dates)))
    gxs <- ixs[dffs > 1]; total_days <- sum(dffs[gxs])
    gaps <- paste(dates[gxs],"[",dffs[gxs],"days]", sep="", collapse=", ")
    message("          for ",site_no," noncontinuous data between ",
            sdate," to ",edate, "\n          gaps about: ",gaps,
            " : total=",total_days)
    return(NULL)
  }
  if(any(is.na(flows))) { # if fillgaps should cause this to not trigger
    message("          for ",site_no," NA flows between ",
            sdate, " to ", edate)
    return(NULL)
  }

  # BEGIN THE PART ALGORITHM
  Nact <- max(cda[1]^0.2, 1, na.rm=TRUE) # index 1 if cda vector
                                         # na.rm needed if cda=NA
  N    <- as.integer(ceiling(Nact))
  NF   <- max(N-1L, 1L); NC <- max(N, 2L); NC1  <- NC + 1L
  # From the flow chart in Rutledge, with additions for 0 flows
  # The variable suffixes are F is the floor of Nact, C is the
  # ceiling of Nact, and C1 is the ceiling plus 1. These correspond
  # to the three values of N in Rutledge.
  #
  # Step 1 set up data (flows already done)
  # ALLGW is set to logical: TRUE (*) and FALSE (0)
  ALLGWF <- ALLGWC <- ALLGWC1 <- rep(FALSE,    num_flows)
  BaseQF <- BaseQC <- BaseQC1 <- rep(NA_real_, num_flows)

  # Step 2 Recored all GW flow where antecendent recession OK
  DiffQ   <- c(0, diff(flows))
  AnteF   <- na2miss(stats::filter(DiffQ <= 0, rep(1, NF ), sides=1), to=0)
  AnteC   <- na2miss(stats::filter(DiffQ <= 0, rep(1, NC ), sides=1), to=0)
  AnteC1  <- na2miss(stats::filter(DiffQ <= 0, rep(1, NC1), sides=1), to=0)
  ALLGWF  <- ifelse(AnteF == NF,   TRUE, ALLGWF )
  BaseQF  <- ifelse(ALLGWF,  flows, BaseQF)
  ALLGWC  <- ifelse(AnteC == NC,   TRUE, ALLGWC )
  BaseQC  <- ifelse(ALLGWC,  flows, BaseQC)
  ALLGWC1 <- ifelse(AnteC1 == NC1, TRUE, ALLGWC1)
  BaseQC1 <- ifelse(ALLGWC1, flows, BaseQC1)

  # Step 3 Revise all GW where necessary
  CkQ <- (flows > 1e-9) & (flows/shiftData(flows, k=-1, fill=1) > 1.258925)
  ALLGWF  <- ifelse(ALLGWF  & CkQ, FALSE, ALLGWF )
  ALLGWC  <- ifelse(ALLGWC  & CkQ, FALSE, ALLGWC )
  ALLGWC1 <- ifelse(ALLGWC1 & CkQ, FALSE, ALLGWC1)

  # Step 4 Interpolate Baseflows
  BaseQF  <- exp(stats::approx(ixs[ALLGWF],  log(flows[ALLGWF]),  xout=ixs, rule=2)$y)
  BaseQC  <- exp(stats::approx(ixs[ALLGWC],  log(flows[ALLGWC]),  xout=ixs, rule=2)$y)
  BaseQC1 <- exp(stats::approx(ixs[ALLGWC1], log(flows[ALLGWC1]), xout=ixs, rule=2)$y)

  # Steps 5, 6, and 4 for each F, C, C1
  while(any(CkQ <- (BaseQF > flows + SMALL))) { # Avoid rounding problems
    CkQ <- CkQ & ! ALLGWF # The trouble makers
    Ck0 <- eventNum(! ALLGWF, reset=TRUE) # Each block of ! ALLGW
    CkE <- unique(Ck0[CkQ])
    ## Find the largest ratio (log difference) in each block of ! ALLGW
    for(i in CkE) {
      ixset <- which(Ck0 == i) # Select from this group
      pck  <- which.max(BaseQF[ixset]/flows[ixset])
      ALLGWF[ixset[pck]] <- TRUE
      BaseQF[ixset[pck]] <- flows[ixset[pck]]
    }
    ## Redo 4
    BaseQF <- exp(stats::approx(ixs[ALLGWF], log(flows[ALLGWF]), xout=ixs, rule=2)$y)
    BaseQF <- ifelse(BaseQF < SMALL, 0, BaseQF) # Clean up
  }

  while(any(CkQ <- (BaseQC > flows + SMALL))) { # Avoid rounding problems
    CkQ <- CkQ & ! ALLGWC # The trouble makers
    Ck0 <- eventNum(! ALLGWC, reset=TRUE) # Each block of !ALLGW
    CkE <- unique(Ck0[CkQ])
    ## Find the largest ratio (log difference) in each block of !ALLGW
    for(i in CkE) {
      ixset <- which(Ck0 == i) # Select from this group
      pck <- which.max(BaseQC[ixset]/flows[ixset])
      ALLGWC[ixset[pck]] <- TRUE
      BaseQC[ixset[pck]] <- flows[ixset[pck]]
    }
    ## Redo 4
    BaseQC <- exp(stats::approx(ixs[ALLGWC], log(flows[ALLGWC]), xout=ixs, rule=2)$y)
    BaseQC <- ifelse(BaseQC < SMALL, 0, BaseQC)
  }

  while(any(CkQ <- (BaseQC1 > flows + SMALL))) { # Avoid rounding problems
    CkQ <- CkQ & ! ALLGWC1 # The trouble makers
    Ck0 <- eventNum(! ALLGWC1, reset=TRUE) # Each block of !ALLGW
    CkE <- unique(Ck0[CkQ])
    ## Find the largest ratio (log difference) in each block of !ALLGW
    for(i in CkE) {
      ixset <- which(Ck0 == i) # Select from this group
      pck <- which.max(BaseQC1[ixset]/flows[ixset])
      ALLGWC1[ixset[pck]] <- TRUE
      BaseQC1[ixset[pck]] <- flows[ixset[pck]]
    }
    ## Redo 4
    BaseQC1 <- exp(stats::approx(ixs[ALLGWC1], log(flows[ALLGWC1]), xout=ixs, rule=2)$y)
    BaseQC1 <- ifelse(BaseQC1 < SMALL, 0, BaseQC1)
  }
  ## Wrap up
  ## Compute the linear interpolation of baseflow
  Ffact <- NC - Nact # Must be between 0 and 1
  BaseQ <- BaseQF*Ffact + BaseQC*(1-Ffact)

  #Flow      <- round(flows,   digits=3L) # why was original algorithm touching this?
  # descision here is to not touch the Flow.
  FlowBase  <- round(BaseQ,   digits=3L) # part of original PART
  FlowPart1 <- round(BaseQF,  digits=4L) # part of original PART
  FlowPart2 <- round(BaseQC,  digits=4L) # part of original PART
  FlowPart3 <- round(BaseQC1, digits=4L) # part of original PART
  # END THE PART ALGORITHM

  if(fillgaps) { # Asquith hacking magic, use env to track all dates
    .dvp.env <- new.env() # then switch around only get dates we WANT then build zz
    for(k in ixs) assign(as.character(dates[k]), FlowBase[k], envir=.dvp.env)
    FlowBase <- sapply(as.character(akdvtable$Date),
                     function(k) ifelse(exists(k, .dvp.env), get(k, .dvp.env), NA))
    .dvp.env <- new.env() # insurance policy to recreate
    for(k in ixs) assign(as.character(dates[k]), FlowPart1[k], envir=.dvp.env)
    FlowPart1 <- sapply(as.character(akdvtable$Date),
                     function(k) ifelse(exists(k, .dvp.env), get(k, .dvp.env), NA))
    .dvp.env <- new.env() # insurance policy to recreate
    for(k in ixs) assign(as.character(dates[k]), FlowPart2[k], envir=.dvp.env)
    FlowPart2 <- sapply(as.character(akdvtable$Date),
                     function(k) ifelse(exists(k, .dvp.env), get(k, .dvp.env), NA))
    .dvp.env <- new.env() # insurance policy to recreate
    for(k in ixs) assign(as.character(dates[k]), FlowPart3[k], envir=.dvp.env)
    FlowPart3 <- sapply(as.character(akdvtable$Date),
                     function(k) ifelse(exists(k, .dvp.env), get(k, .dvp.env), NA))
  }
  # Finally, in the event that the Flow is missing, we must ensure that all the
  FlowBase[ is.na(akdvtable$Flow)] <- NA # baseflows are set to missing as well
  FlowPart1[is.na(akdvtable$Flow)] <- NA
  FlowPart2[is.na(akdvtable$Flow)] <- NA
  FlowPart3[is.na(akdvtable$Flow)] <- NA

  if(na.negflow) {
    FlowBase[ ! is.na(akdvtable$Flow) & akdvtable$Flow < 0] <- NA # baseflows are set to missing
    FlowPart1[! is.na(akdvtable$Flow) & akdvtable$Flow < 0] <- NA # when the daily streamflow itself
    FlowPart2[! is.na(akdvtable$Flow) & akdvtable$Flow < 0] <- NA # is zero
    FlowPart3[! is.na(akdvtable$Flow) & akdvtable$Flow < 0] <- NA
  }
  zz <- data.frame(agency_cd = akdvtable$agency_cd,
                   site_no   = site_no,
                   Date      = akdvtable$Date,
                   Flow      = akdvtable$Flow,
                   Flow_cd   = akdvtable$Flow_cd,
                   site      = site_no,
                   year      = akdvtable$year,
                   decade    = akdvtable$decade,
                   wyear     = akdvtable$wyear,
                   month     = akdvtable$month,
                   FlowBase  = FlowBase,
                   FlowPart1 = FlowPart1,
                   FlowPart2 = FlowPart2,
                   FlowPart3 = FlowPart3,
                   stringsAsFactors=FALSE)
  # Following is explicitly not needed because dates and flows had already be trimmed from NAs
  #set <- is.na(zz$Flow); zz$FlowBase[ set] <- NA; zz$FlowPart1[set] <- NA
  #                       zz$FlowPart2[set] <- NA; zz$FlowPart3[set] <- NA
  #
  #plot(zz$Date, zz$Flow, type="l", col=grey(0.8))    # part of testing for Asquith
  #lines(zz$Date, zz$FlowBase, col=2); mtext(site_no) # part of testing for Asquith
  return(zz)
}


#dv <- dvget("08167000", sdate="1969-10-01", edate="1997-09-30")
#pdv <- dvpart(dv, cda=839)
#plot(pdv$Date,  pdv$Flow, log="y", type="l", col=8)
#lines(pdv$Date, pdv$FlowBase,  col=1)
#lines(pdv$Date, pdv$FlowPart1, col=2)
#lines(pdv$Date, pdv$FlowPart2, col=3)
#lines(pdv$Date, pdv$FlowPart3, col=4)

#dv <- dvget("08167000", sdate="1969-10-01", edate="")
#pdv <- dvpart(dv, cda=839)
#plot(pdv$Date,  pdv$Flow, log="y", type="l", col=8)
#lines(pdv$Date, pdv$FlowBase,  col=1)
#lines(pdv$Date, pdv$FlowPart1, col=2)
#lines(pdv$Date, pdv$FlowPart2, col=3)
#lines(pdv$Date, pdv$FlowPart3, col=4)
