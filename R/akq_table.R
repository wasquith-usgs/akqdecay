"akq_table" <- function(akqenv, NAline.insert=TRUE, type=c("akqdecay", "other"),
                                 silent=FALSE, ...) {
  if(is.null(akqenv)) {
     warning(" need to specify the akqenv, dv, or dvpart environment")
     return()
  } else if(! is.environment(akqenv)) {
     ifelse(type == "akqdecay", return(akqenv$table), return(akqenv))
  }
  type <- match.arg(type)

  SITES <- sort(ls(akqenv)); n <- length(SITES)
  TAB <- TMP <- BLANK <- NULL
  for(i in 1:n) {
    if(! silent) message(" Tables for ", SITES[i], " (",i,"/",n,")", appendLF = FALSE)
    Z <- get(SITES[i], envir=akqenv)
    if(length(Z) == 1) {
       if(! silent) message(" -- none available"); next
    } else {
       if(! silent) message(" -- got'em")
    }
    ifelse(type == "akqdecay", TMP <- Z$table, TMP <- Z) # not a legacy design difference
    if(length(TMP) == 2) {
       ifelse(i == 1, TAB <- TMP, TAB <- rbind(TAB, TMP))
    } else {
       # legacy design difference being accommodated here.
       ifelse(type == "akqdecay", TMP$date <- as.character(TMP$date),
                                  TMP$Date <- as.character(TMP$Date))
       if(i == 1) {
          BLANK <- TMP[1,]; BLANK[1,] <- NA
          if(i != n) TMP[length(TMP[,1])+1,] <- BLANK
          TAB <- TMP
       } else {
          if(i != n) TMP[length(TMP[,1])+1,] <- BLANK
          TAB <- rbind(TAB, TMP)
       }
    }
  }
  if(! NAline.insert) { # legacy design difference being accommodated here.
    ifelse(type == "akqdecay", TAB <- TAB[! is.na(TAB$site), ],
                               TAB <- TAB[! is.na(TAB$site_no), ])
  }
  if(length(TAB) != 2) { # legacy design difference being accommodated here.
    ifelse(type == "akqdecay", TAB$date <- as.Date(TAB$date),
                               TAB$Date <- as.Date(TAB$Date))
  }
  return(TAB)
}

# wolfriver <- c("07030392", "07030500", "07031650",
#               "07031660", "07031700", "07031740")
#wolf.env <- new.env() # the standard declaration of an environment
#fill_dvenv(wolfriver, envir=wolf.env, ignore.provisional=TRUE) # wolf.env now filled with six tables.
#partwolf.env <- new.env() # the standard declaration of an environment
#fill_dvpartenv(dvenv=wolf.env, envir=partwolf.env, fillgaps=TRUE)
#wolfdf <- akq_table(partwolf.env, type="other")
