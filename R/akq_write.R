"akq_write" <- function(df, file=NA, sep=";", digits=6, silent=FALSE,
                        header="# optional header string", ...) {
   if(! is.data.frame(df)) {
      warning("argument df is not a data.frame, returning NA")
      return(NA)
   }
   is.num <- sapply(df, is.numeric)
   df[is.num] <- lapply(df[is.num], round, digits)
   if(! is.na(file)) {
      file <- as.character(file)
      unlink(file)
      wd <- getwd()
      if(! silent) message("writing ",wd,"/",file)
      if(! is.null(header)) {
         sink(file=file); cat(paste0(as.character(header),"\n")); sink()
      }
      opts <- options(warn=-1) # to suppress the warning triggered by append=TRUE
      write.table(df, file=file, append=TRUE, quote=FALSE, row.names=FALSE, sep=sep, ...)
      options(opts)
   } else {
      message("file not specified, no writing to the operating system made")
   }
}
