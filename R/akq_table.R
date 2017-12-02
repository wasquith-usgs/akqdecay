"akq_table" <- function(akqenv, silent=FALSE, NAline.insert=TRUE, ...) {
  if(is.null(akqenv)) {
     warning(" need to specify the akqenv environment")
     return()
  } else if(! is.environment(akqenv)) {
     return(akqenv$table)
  }
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
    TMP <- Z$table
    if(length(TMP) == 2) {
       ifelse(i == 1, TAB <- TMP, TAB <- rbind(TAB, TMP))
    } else {
       TMP$date <- as.character(TMP$date)
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
  if(! NAline.insert) TAB <- TAB[! is.na(TAB$site), ]
  if(length(TAB) != 2) TAB$date <- as.Date(TAB$date)
  return(TAB)
}
