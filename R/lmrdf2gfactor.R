"lmrdf2gfactor" <- function(df, f, type="gpa", ...) {
   if(! is.data.frame(df)) {
      warning("argument df is not a data.frame, returning NA")
      return(NA)
   }
   if(is.na(type)) return(rep(NA, nrow(df)))
   gfs <- sapply(1:nrow(df), function(i) {
             lmrs <- lmomco::vec2lmom(c(df$L1[i], df$L2[i], df$T3[i],
                                        df$T4[i], df$T5[i], df$T6[i]), checklmom = FALSE)
             if(! lmomco::are.lmom.valid(lmrs) | is.na(lmrs$ratios[3]) |
                                                 is.na(lmrs$ratios[4])) return(NA)
             para <- lmomco::lmom2par(lmrs, type=type, ...)
             if(is.null(para)) return(NA)
             return( lmomco::qlmomco(f, para)) }, ...)
   return(gfs)
}
