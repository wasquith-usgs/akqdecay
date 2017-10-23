"akq_scibase" <-
function(akqenv, basename="SciBase_", extension=".txt",
                 dotable=FALSE, doyearlmr=FALSE, dodecadelmr=FALSE,
                 silent=FALSE, ...) {
  if(is.null(akqenv)) {
     warning(" need to specify the akqenv environment or list")
     return()
  }

  if(! silent) message("  Count extraction ...")
  CN <- akq_counts(     akqenv, silent=TRUE) # various count table
  akq_write(CN, file=paste0(basename,"counts",extension),     silent=silent,
     header="# Counts from Asquith--Knight discharge decay analysis", ...)
  SM <- summary(CN)
  print(SM)
  rm(CN)

  if(! silent) message("  L-moment extraction ...")
  LM <- akq_lmom(       akqenv, silent=TRUE) # L-moments for POR
  akq_write(LM, file=paste0(basename,"lmompor",extension),    silent=silent,
     header="# L-moments (period of record) of Asquith--Knight discharge decay analysis", ...)
  rm(LM)

  if(doyearlmr) {
  if(! silent) message("  L-moments by year extraction ...")
  LY <- akq_lmom_year(  akqenv, silent=TRUE) # L-moments by year
  akq_write(LY, file=paste0(basename,"lmomyear",extension),   silent=silent,
     header="# L-moments (by year) of Asquith--Knight discharge decay analysis", ...)
  rm(LY)
  }

  if(dodecadelmr) {
  if(! silent) message("  L-moments by decade extraction ...")
  LD <- akq_lmom_decade(akqenv, silent=TRUE) # L-moments by decade
  akq_write(LD, file=paste0(basename,"lmomdecade",extension), silent=silent,
     header="# L-moments (by decade) of Asquith--Knight discharge decay analysis", ...)
  rm(LD)
  }

  if(! silent) message("  Summary extraction ...")
  SU <- akq_summary(    akqenv, silent=TRUE) # basic summary table
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

