"lmrfdc_table" <- function(lmrfdcenv, NAline.insert=TRUE, silent=FALSE, ...) {
  if(is.null(lmrfdcenv)) {
     warning(" need to specify the lmrfdcenv environment")
     return()
  } else if(! is.environment(lmrfdcenv)) {
     warning(" need to specify the lmrfdcenv environment")
     return()
  }
  SITES <- sort(ls(lmrfdcenv)); n <- length(SITES)
  TAB <- TMP <- BLANK <- NULL; j <- 0
  for(i in 1:n) {
    if(! silent) message(" Tables for ", SITES[i], " (",i,"/",n,")", appendLF = FALSE)
    Z <- get(SITES[i], envir=lmrfdcenv)
    if(length(Z) == 1) {
       if(! silent) message(" -- none available"); next
    } else {
       if(! silent) message(" -- got'em")
    }
    TMP <- Z; j <- j + 1 # the j counter deals with fact that the first sites could be NAs, themselves
    if(length(TMP) == 1) {
       ifelse(i == 1, TAB <- TMP, TAB <- rbind(TAB, TMP))
    } else {
       if(j == 1) {
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
  TAB$site <- as.character(TAB$site)
  return(TAB)
}
