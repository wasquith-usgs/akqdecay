"dvpart2mnpart" <- function(pdv, ...) {
    if(! is.data.frame(pdv)) {
      warning(" pdv is not testing as an data.frame")
      return()
    }
    "my_monthly_days" <- function(yy,mm) { # so we do not have to import a package
       if(length(yy) != length(mm)) stop("fatal error") # just for one function
       yy2 <- yy;   yy2[mm == 12] <- yy2[mm == 12]+1    # we define ourselves
       mm2 <- mm+1; mm2[mm == 12] <- 1
       return(as.numeric(difftime(as.Date(paste0(yy2,"-",mm2,"-01")),
                                  as.Date(paste0(yy, "-",mm, "-01")))))
    }
    suppressWarnings(zz <- aggregate(pdv, by=list(pdv$year, pdv$month), mean))
    ml <- data.frame(year=pdv$year, month=pdv$month) # subset for speed
    suppressWarnings(ml <- aggregate(ml, by=list(ml$year, ml$month), length))
    zz <- zz[order(zz$Group.1),]; ml <- ml[order(ml$Group.1),];
    zz$Group.1 <- zz$Group.2 <- NULL # delete these two columns
    zz$agency_cd <- pdv$agency_cd[1]  # add back the agency code
    zz$site_no   <- pdv$site_no[1]    # add back the streamgage number
    zz$month_count   <- ml$month; rm(ml)  # delete the ml (do not need further)
    zz$days_in_month <- my_monthly_days(zz$year,zz$month)
    zz <- zz[zz$month_count/zz$days_in_month == 1,]
    nm <- names(zz); nm[3] <- "DateMean"; names(zz) <- nm; rm(nm)
    zz$Flow_cd <- NULL; zz$Date <- NULL
    row.names(zz) <- NULL
    return(zz)
}
