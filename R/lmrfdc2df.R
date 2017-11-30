"lmrfdc2df" <- function(envir, silent=FALSE, ...) {
  if(is.null(envir)) {
     warning(" need to specify the lmrfdcenv environment")
     return()
  } else if(! is.environment(envir)) {
     warning(" need to specify the lmrfdcenv environment")
     return()
  }
  SITES <- sort(ls(envir)); n <- length(SITES)
  TAB <- TMP <- BLANK <- NULL
  for(i in 1:n) {
    if(! silent) message(" Tables for ", SITES[i], " (",i,"/",n,")", appendLF = FALSE)
    Z <- get(SITES[i], envir=envir)
    if(length(Z) == 1) {
       if(! silent) message(" -- none available"); next
    } else {
       if(! silent) message(" -- got'em")
    }
    TMP <- Z
    if(length(TMP) == 2) {
       ifelse(i == 1, TAB <- TMP, TAB <- rbind(TAB, TMP))
    } else {
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
  return(TAB)
}
