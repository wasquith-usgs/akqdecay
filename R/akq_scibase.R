"akq_scibase" <-
function(akqenv, basename="SciBase_", extension=".txt",
                 dotable=FALSE, doyearlmr=FALSE, dodecadelmr=FALSE,
                 rm.gfactor=TRUE, rm.L1L2=TRUE,
                 is.nochange.env=FALSE, site2site_no=FALSE,
                 silent=FALSE, ...) {
  if(is.null(akqenv)) {
     warning(" need to specify the akqenv environment or list")
     return()
  }

  if(! silent) message("  Count extraction ...")
  CN <- akq_counts(     akqenv, silent=TRUE) # various count table
  if(site2site_no) { tmp <- names(CN); tmp[1] <- "site_no"; names(CN) <- tmp }
  akq_write(CN, file=paste0(basename,"counts",extension),     silent=silent,
     header="# Counts from Asquith--Knight discharge decay analysis", ...)
  SM <- summary(CN)
  print(SM)
  rm(CN)

  if(! silent) message("  L-moment extraction ...")
  LM <- akq_lmom(       akqenv, silent=TRUE) # L-moments for POR
  if(site2site_no) { tmp <- names(LM); tmp[1] <- "site_no"; names(LM) <- tmp }
  if(rm.gfactor) LM$gfactor <- NULL
  if(rm.L1L2)    LM$L1L2    <- NULL
  if(is.nochange.env) LM <- LM[,1:3]
  akq_write(LM, file=paste0(basename,"lmompor",extension),    silent=silent,
     header="# L-moments (period of record) of Asquith--Knight discharge decay analysis", ...)
  rm(LM)

  if(doyearlmr) {
  if(! silent) message("  L-moments by year extraction ...")
  LY <- akq_lmom_year(  akqenv, silent=TRUE) # L-moments by year
  if(site2site_no) { tmp <- names(LY); tmp[1] <- "site_no"; names(LY) <- tmp }
  if(rm.gfactor) LY$gfactor <- NULL
  if(rm.L1L2)    LY$L1L2    <- NULL
  if(is.nochange.env) LY <- LY[,1:3]
  akq_write(LY, file=paste0(basename,"lmomyear",extension),   silent=silent,
     header="# L-moments (by year) of Asquith--Knight discharge decay analysis", ...)
  rm(LY)
  }

  if(dodecadelmr) {
  if(! silent) message("  L-moments by decade extraction ...")
  LD <- akq_lmom_decade(akqenv, silent=TRUE) # L-moments by decade
  if(site2site_no) { tmp <- names(LD); tmp[1] <- "site_no"; names(LD) <- tmp }
  if(rm.gfactor) LD$gfactor <- NULL
  if(rm.L1L2)    LD$L1L2    <- NULL
  if(is.nochange.env) LD <- LD[,1:3]
  akq_write(LD, file=paste0(basename,"lmomdecade",extension), silent=silent,
     header="# L-moments (by decade) of Asquith--Knight discharge decay analysis", ...)
  rm(LD)
  }

  if(! silent) message("  Summary extraction ...")
  SU <- akq_summary(    akqenv, silent=TRUE) # basic summary table
  if(site2site_no) { tmp <- names(SU); tmp[1] <- "site_no"; names(SU) <- tmp }
  if(rm.gfactor) SU$gfactor <- NULL
  if(rm.L1L2)    SU$L1L2    <- NULL
  if(is.nochange.env) SU <- SU[,1:6]
  akq_write(SU, file=paste0(basename,"summary",extension),    silent=silent,
     header="# Summary of Asquith--Knight discharge decay analysis", ...)
  rm(SU)

  if(dotable) {
      if(! silent) message("  Table extraction ...")
      ST <- akq_table(      akqenv, silent=TRUE) # continuous raw table for all streamgages
      akq_write(ST, file=paste0(basename,"table",extension),      silent=silent,
                header="# Raw table Asquith--Knight discharge decay analysis", ...)
      rm(ST)
  }
}

