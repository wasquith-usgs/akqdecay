"akqdecay" <-
function(akdvtable, as.list=TRUE, as.ra7=FALSE, f=0.90,
                    method=c("decreasing", "increasing", "nochange"),
                    lag=1, differences=1, site="", type="gpa",
                    notable=FALSE, setgfna=FALSE, ...) {
  if(length(unique(akdvtable$site_no)) > 1) {
    warning("can not have move than one streamgage in the daily value table, please ",
            "consult fill_dvenv() and fill_akqenv() for multiple streamgage processing, ",
            "returning NA immediately")
    return(NA)
  }
  method <- match.arg(method)
  site <- as.character(site)
  if(length(akdvtable$year) == 0) return(NA)
  if(setgfna) type <- NA
  if(as.ra7) {
    lag <- 1; differences <- 1
  }

  akdvtable <- akdvtable[order(akdvtable$Date),]
  Q <- akdvtable$Flow; dn <- (length(Q)+1)
  year   <- akdvtable$year; wyear <- akdvtable$wyear
  month  <- akdvtable$month; date <- akdvtable$Date
  ppos   <- rank(Q, ties.method="random", na.last="keep") / dn
  decade <- akdvtable$decade
  n.raw_count_zeros <- length(Q[Q <= 0])
  n.raw_count_NAs   <- length(Q[is.na(Q)])

  logQ <- suppressWarnings(log10(Q)); logQ[! is.finite(logQ)] <- NA
  del_logQ <- diff(logQ, lag=lag, differences=differences)/lag
  del_date <- diff(date, lag=lag, differences=differences)
  #print(del_logQ[1:730])
  #print(del_date[1:730])
  n.deltaDate_ne_lag <- sum(del_date != lag)
  del_logQ[del_date != lag] <- NA                                  # TODO, diffs + lag?
  #print(del_logQ)
  ns <- (length(year) - length(del_logQ)+differences):length(year) # TODO, diffs + lag?
  year   <-   year[ns]; wyear <- wyear[ns]
  month  <-  month[ns]; ppos  <-  ppos[ns]
  decade <- decade[ns]; date  <-  date[ns]
  tmp_Q_related_to_fdc_extraction <- Q[ns]


  total_count <- length(del_logQ) # note that NAs are being counted too!
  n.decreases <- sum(del_logQ <  0, na.rm=TRUE)
  n.increases <- sum(del_logQ >  0, na.rm=TRUE)
  n.nochange  <- sum(del_logQ == 0, na.rm=TRUE)
  n.na        <- sum(is.na(del_logQ))
  state.chng  <- data.frame(site=site, total_count=total_count,
                            decreases=n.decreases, increases=n.increases,
                            nochanges=n.nochange, NAs=n.na,
                            rawDV_zeros=n.raw_count_zeros,
                            rawDV_NAs=n.raw_count_NAs,
                            delDates_ne_lag=n.deltaDate_ne_lag,
                            stringsAsFactors=FALSE)
  row.names(state.chng) <- "" # strip it, confusing for data.frame with single row

  if(method == "decreasing") {
     subget <- del_logQ <  0
  } else if(method == "increasing") {
     subget <- del_logQ >  0
  } else {
     subget <- del_logQ == 0
  }
  subget[is.na(subget)] <- FALSE # this is kind of important if zero values of
  # discharge had been encountered. The subgets come in as NA which causes
  # subtle issues, best to to treat them as FALSE and drop out these days.

  fvalues <- del_logQ[subget]
  fwyears <- wyear[   subget]
  fyears  <- year[    subget]
  fmonths <- month[   subget]
  fppos   <- ppos[    subget]
  fqpos   <- tmp_Q_related_to_fdc_extraction[subget] # Flow-duration curve
  fdecade <- decade[  subget]
  fdate   <- date[    subget]
  tmp <- data.frame(fvalues=fvalues, fqpos=fqpos)
  tmp <- tmp[stats::complete.cases(tmp), ]
  correlations <- c(NA,NA)
  correlations[1] <- suppressWarnings(cor(tmp$fvalues,tmp$fqpos, method="kendall" ))
  correlations[2] <- suppressWarnings(cor(tmp$fvalues,tmp$fqpos, method="spearman"))

  # A negative correlation if declining streamflow conditions would be expected.
  # This would imply that small days per log cycle decline decrease as the
  # associated streamflow magnitude increases. For example, stormflows decay
  # much quicker than baseflow supported by groundwater. The correlations are
  # based on the raw days_per_log, which are negative if decreasing=TRUE! The
  # correlations should for almost any conceivable situation be negative!
  # NOTE THE ABS() THAT FOLLOWS IN THE NEXT LINE!
  fvalues <- abs(fvalues) # users keep track of call method for their own needs
  number_fvalues <- length(fvalues)

  if(as.ra7) {
     zz <- median(fvalues, na.rm=TRUE) # Return the RA7 statistic
     names(zz) <- paste0("natural_logarithm_median_",method)
     return(zz*log(10))
  }

  lambda.r <- 6
  fn <- function(x) {
       nv <- length(x); if(nv > lambda.r) nv <- lambda.r
       if(nv == 0) return(rep(NA, lambda.r))
       if(length(unique(x)) == 1) return(c(x[1], NA, NA, NA, NA, NA))
       lmr <- lmomco::lmoms(x, nmom=nv, no.stop=TRUE)
       if(is.null(lmr)) return(rep(NA, lambda.r))
       return(c(lmr$lambdas[1], lmr$lambdas[2],
                lmr$ratios[3],  lmr$ratios[4], lmr$ratios[5], lmr$ratios[6]))
  }

  fvalues <- 1/fvalues # convert to days but keep the decimals
  if(as.list) {
    if(number_fvalues == 0) {
       warning("no values for any processing for ",site, ", returning NA immediately")
       zz <- list(table=NA, counts=state.chng, summary=NA,
                  lmoments=list(por=NA, by_year=NA, by_decade=NA), ifail=1)
       attr(zz, "method"           ) <- method
       attr(zz, "lag"              ) <- lag
       attr(zz, "differences"      ) <- differences
       attr(zz, "probability"      ) <- f
       attr(zz, "distribution_type") <- type
       return(zz)
    }
    zz <- data.frame(site=site, wyear=fwyears, year=fyears, month=fmonths, decade=fdecade,
                  date=fdate, fdc=fppos, fqc=fqpos, days_per_log=fvalues,
                  pp_days_per_log=lmomco::pp(fvalues, sort=FALSE), stringsAsFactors=FALSE)
    zz <- zz[! is.na(zz$days_per_log), ] # purge the NAs and attendant row entirely
    if(length(zz$site) == 0) {
       warning("no values for any processing after NA check on logs for ",site,
               ", returning NA immediately")
       zz <- list(table=NA, counts=state.chng, summary=NA,
                  lmoments=list(por=NA, by_year=NA, by_decade=NA), ifail=2)
       attr(zz, "method"           ) <- method
       attr(zz, "lag"              ) <- lag
       attr(zz, "differences"      ) <- differences
       attr(zz, "probability"      ) <- f
       attr(zz, "distribution_type") <- type
       return(zz)
    }
    nv <- length(fvalues); if(nv > lambda.r) nv <- lambda.r
    lmrpor <- NULL
    if(nv == 0) {
      lmrpor <- list(lambdas=rep(NA, lambda.r), ratios=rep(NA,lambda.r))
    } else if(length(unique(fvalues)) == 1) {
      lmrpor <- list(lambdas=c(fvalues[1], NA, NA, NA, NA, NA),
                     ratios=rep(NA,lambda.r))
    } else {
      lmrpor <- lmomco::lmoms(zz$days_per_log, nmom=nv, no.stop=TRUE)
      if(is.null(lmrpor)) lmrpor <- list(lambdas=c(NA, NA, NA, NA, NA, NA),
                                         ratios=rep(NA,lambda.r))
    }

    # The next line removes four fields that are not germane to the
    # Asquith--Knight approach.
    lmrpor$trim <- lmrpor$leftrim <- lmrpor$rightrim <- lmrpor$source <- NULL

    yr_range <- range(zz$year)
    yr_range_str <- paste0(yr_range[1],"--",yr_range[2])
    # the next value is experimental
    mean_rtpi_lscale <- lmrpor$lambdas[1] + sqrt(pi)*lmrpor$lambdas[2]
    if(is.na(type) | is.na(lmrpor$ratios[3]) | is.na(lmrpor$ratios[4])) {
       gfactor <- NA
    } else {
       if(! is.na(type)) {
          #print("HERE ARE L-MOMENTS")
          #print(lmrpor)
          if(! lmomco::are.lmom.valid(lmrpor) | is.na(lmrpor$ratios[3]) |
                                                is.na(lmrpor$ratios[4])) {
             gfactor <- NA
          } else {
             gfactor <- lmomco::qlmomco(f,
                        lmomco::lmom2par(lmrpor, type=type, ...))
          }
       }
    }
    gfactor_emp <- quantile(zz$days_per_log, probs=f)

    lmrdf <- data.frame(site=site, yr_range_str=yr_range_str,
       count=length(zz$days_per_log), median=median(zz$days_per_log),
       L1L2=mean_rtpi_lscale, gfactor=gfactor, gfactor_emp=gfactor_emp,
       L1=lmrpor$lambdas[1], L2=lmrpor$lambdas[2], T3=lmrpor$ratios[3],
       T4= lmrpor$ratios[4], T5= lmrpor$ratios[5], T6=lmrpor$ratios[6],
                                                         stringsAsFactors=FALSE)
    lmrpor <- lmrdf # design changes late in the development process led to a
    # change of how the L-moments for the period of record are tabulated.
    # The safest decision is to manually rebuild the structure as a data.frame
    # and quietly substitute it for lmrpor
    #print(lmrpor)

    # BY YEAR
    gg1 <- aggregate(zz$days_per_log, by=list(zz$year), median)
    ggn <- aggregate(zz$days_per_log, by=list(zz$year), function(y) length(y))
      names(gg1) <- c("year", "median")
      names(ggn) <- c("year", "count" )
      gg1 <- merge(gg1, ggn, all=TRUE)
      tmp <- aggregate(zz$days_per_log, by=list(zz$year), simplify=FALSE, fn)
      tmp <- as.data.frame(matrix(unlist(tmp[2]), ncol=lambda.r, byrow=TRUE))
      tmp$year <- gg1$year
      gg1 <- merge(gg1, tmp, all=TRUE)
      names(gg1)  <- c("year","median","count","L1","L2","T3","T4","T5","T6")
      gg1$L1L2    <- gg1$L1 + sqrt(pi)*gg1$L2
      gg1$gfactor <- lmrdf2gfactor(gg1, f, type=type, ...)
      gg1$gfactor_emp <-
          aggregate(zz$days_per_log, by=list(zz$year), quantile, probs=f )$x
      gg1 <- gg1[,c(1,3,2,10:12,4:9)] # column reordering
      gg1$year <- as.integer(gg1$year)

    # BY DECADE
    gg2 <- aggregate(zz$days_per_log, by=list(zz$decade), median)
    ggn <- aggregate(zz$days_per_log, by=list(zz$decade), function(y) length(y))
      names(gg2) <- c("decade", "median")
      names(ggn) <- c("decade", "count" )
      gg2 <- merge(gg2, ggn, all=TRUE)
      tmp <- aggregate(zz$days_per_log, by=list(zz$decade), simplify=FALSE, fn)
      tmp <- as.data.frame(matrix(unlist(tmp[2]), ncol=lambda.r, byrow=TRUE))
      tmp$decade <- gg2$decade
      gg2 <- merge(gg2, tmp, all=TRUE)
      names(gg2)  <- c("decade","median","count","L1","L2","T3","T4","T5","T6")
      gg2$L1L2  <- gg2$L1 + sqrt(pi)*gg2$L2
      gg2$gfactor <- lmrdf2gfactor(gg2, f, type=type, ...)
      gg2$gfactor_emp <-
          aggregate(zz$days_per_log, by=list(zz$decade), quantile, probs=f )$x
      gg2 <- gg2[,c(1,3,2,10:12,4:9)] # column reordering
      gg2$decade <- as.integer(gg2$decade)

    #-----------------------------------------------------------------------------------
    xx <- data.frame(site=site,
                     beg_year=as.integer(yr_range[1]), end_year=as.integer(yr_range[2]),
                     yr_range_str=lmrpor$yr_range_str,
                     total_count=total_count, count=lmrpor$count,
                     kendall_tau=correlations[1], spearman_rho=correlations[2],
                     median=lmrpor$median, L1L2=mean_rtpi_lscale,
                     gfactor=gfactor, gfactor_emp=gfactor_emp, stringsAsFactors=FALSE)
    gg1$site <- site;                     gg2$site <- site
    gg1n     <- ncol(gg1);                gg2n     <- ncol(gg2)
    gg1      <- gg1[,c(gg1n,1:(gg1n-1))]; gg2      <- gg2[,c(gg2n,1:(gg2n-1))]

    row.names(xx) <-  row.names(lmrpor) <- "" # The will have inherent the probability
    # as percent from the quantile() call. Users have confused row names as confidence
    # limits, it is best to strip the row names out. The other dataframes are okay and
    # have sequence numbering for the row names.

    zz <- list(table=zz, counts=state.chng, summary=xx,
               lmoments=list(por=lmrpor, by_year=gg1, by_decade=gg2), ifail=0)
    attr(zz, "method"           ) <- method
    attr(zz, "lag"              ) <- lag
    attr(zz, "differences"      ) <- differences
    attr(zz, "probability"      ) <- f
    attr(zz, "distribution_type") <- type
    if(notable) zz$table <- data.frame(site=site, rest_of_it="not_requested",
                                       stringsAsFactors=FALSE)
    return(zz)
  } else {
    if(number_fvalues == 0) {
       zz <- NA
    } else {
       zz <- data.frame(site_no=site, year=fyears, wyears=fwyears, month=fmonths,
                        decade=fdecade, date=fdate, fdc=fppos, fqc=fqpos,
                        days_per_log=fvalues, pp_days_per_log=lmomco::pp(fvalues, sort=FALSE),
                        stringsAsFactors=FALSE)
    }
    attr(zz, "method"     ) <- method
    attr(zz, "lag"        ) <- lag
    attr(zz, "differences") <- differences
    return(zz)
  }
}




