"gfredo" <- function(akqenv, f=0.90, type="gpa", which=c("por", "year", "decade"),
                             all=TRUE, showdiffsum=FALSE, setgfna=FALSE,
                             silent=FALSE, ...) {
  which <- match.arg(which)

  "get_empGF" <- function(df, probs=f) {
        gfpor <- quantile( df$days_per_log, probs=f)
       gfyear <- aggregate(df$days_per_log, by=list(df$year),   quantile, probs=f )$x
     gfdecade <- aggregate(df$days_per_log, by=list(df$decade), quantile, probs=f )$x
     zz <- list(por=gfpor, by_year=gfyear, by_decade=gfdecade)
     return(zz)
  }

  if(is.null(akqenv)) {
     warning(" need to specify the akqenv environment")
     return()
  } else if(! is.environment(akqenv)) {
     if(length(akqenv$table) == 1) {
        warning("whoops missing daily value table")
        return()
     }
     empGF <- get_empGF(akqenv$table) # recompute the empirical G-factors
     if(all) {
        akqenv$lmoments$by_year$gfactor_emp <- empGF$by_year
        if(setgfna) {
           akqenv$lmoments$by_year$gfactor <- NA
        } else {
           org_gf <- akqenv$lmoments$by_year$gfactor
               gf <- lmrdf2gfactor(akqenv$lmoments$by_year,   f, type=type, ...)
           akqenv$lmoments$by_year$gfactor <- gf
           if(showdiffsum) print(summary(org_gf-gf))
        }

        akqenv$lmoments$by_decade$gfactor_emp <- empGF$by_decade
        if(setgfna) {
           akqenv$lmoments$by_decade$gfactor <- NA
        } else {
           org_gf <- akqenv$lmoments$by_decade$gfactor
               gf <- lmrdf2gfactor(akqenv$lmoments$by_decade, f, type=type, ...)
           akqenv$lmoments$by_decade$gfactor <- gf
           if(showdiffsum) print(summary(org_gf-gf))
        }

        akqenv$lmoments$por$gfactor_emp <- empGF$por
        if(setgfna) {
           akqenv$lmoments$por$gfactor <- NA
        } else {
           org_gf <- akqenv$lmoments$por$gfactor
               gf <- lmrdf2gfactor(akqenv$lmoments$por,       f, type=type, ...)
           akqenv$lmoments$por$gfactor <- gf
           if(showdiffsum) print(summary(org_gf-gf))
        }
        attr(akqenv, "probability"      ) <- f
        attr(akqenv, "distribution_type") <- type
        return(akqenv)
     }
     if(which == "year") {
        akqenv$lmoments$by_year$gfactor_emp <- empGF$by_year
        if(setgfna) {
           akqenv$lmoments$by_year$gfactor <- NA
        } else {
           org_gf <- akqenv$lmoments$by_year$gfactor
               gf <- lmrdf2gfactor(akqenv$lmoments$by_year,   f, type=type, ...)
           akqenv$lmoments$by_year$gfactor <- gf
        }
     } else if(which == "decade") {
        akqenv$lmoments$by_decade$gfactor_emp <- empGF$by_decade
        if(setgfna) {
           akqenv$lmoments$by_decade$gfactor <- NA
        } else {
           org_gf <- akqenv$lmoments$by_decade$gfactor
               gf <- lmrdf2gfactor(akqenv$lmoments$by_decade, f, type=type, ...)
           akqenv$lmoments$by_decade$gfactor <- gf
        }
     } else {
        akqenv$lmoments$por$gfactor_emp <- empGF$por
        if(setgfna) {
           akqenv$lmoments$por$gfactor <- NA
        } else {
           org_gf <- akqenv$lmoments$por$gfactor
               gf <- lmrdf2gfactor(akqenv$lmoments$por,       f, type=type, ...)
           akqenv$lmoments$por$gfactor <- gf
        }
     }
     if(showdiffsum) print(summary(org_gf-gf))
     attr(akqenv, "probability"      ) <- f
     attr(akqenv, "distribution_type") <- type
     return(akqenv)
  }
  SITES <- sort(ls(akqenv)); n <- length(SITES)
  if(all) {
     witches <- c("por", "year", "decade")
  } else {
     witches <- which
  }
  for(which in witches) {
  for(i in 1:n) {
    if(! silent) message("Updating gfactors for ", SITES[i], " (",i,"/",n,") for ",which, appendLF = FALSE)
    Z <- get(SITES[i], envir=akqenv)
    if(length(Z) == 1) {
       if(! silent) message(" -- none available"); next
    } else {
       if(! silent) message(" -- got'em")
    }
    if(length(Z$table) == 1) {
      if(! silent) message(" -- whoops, no daily value table"); next
    }
    empGF <- get_empGF(Z$table) # recompute the empirical G-factors
    if(which == "year") {
       Z$lmoments$by_year$gfactor_emp <- empGF$by_year
       if(setgfna) {
          Z$lmoments$by_year$gfactor <- NA
       } else {
          org_gf <- Z$lmoments$by_year$gfactor
              gf <- lmrdf2gfactor(Z$lmoments$by_year,   f, type=type, ...)
          Z$lmoments$by_year$gfactor <- gf
       }
    } else if(which == "decade") {
       Z$lmoments$by_decade$gfactor_emp <- empGF$by_decade
       if(setgfna) {
          Z$lmoments$by_decade$gfactor <- NA
       } else {
          org_gf <- Z$lmoments$by_decade$gfactor
              gf <- lmrdf2gfactor(Z$lmoments$by_decade, f, type=type, ...)
          Z$lmoments$by_decade$gfactor <- gf
       }
    } else {
       Z$lmoments$by_decade$por <- empGF$por
       if(setgfna) {
          Z$lmoments$por$gfactor <- NA
       } else {
          org_gf <- Z$lmoments$por$gfactor
              gf <- lmrdf2gfactor(Z$lmoments$por,       f, type=type, ...)
          Z$lmoments$por$gfactor <- gf
       }
    }
    if(showdiffsum) print(summary(org_gf-gf))
    attr(Z, "probability"      ) <- f
    attr(Z, "distribution_type") <- type
    assign(SITES[i], Z, envir=akqenv)
  }
  }
}
