"sites_to_SpatialPointsDataFrame" <-
function(siteNumbers, latlongcrs=NA, proj4string=NA, usesp=TRUE, silent=FALSE, ...) {
   sitefile <- NULL; haveFirst <- TRUE; i <- 0; n <- length(siteNumbers)
   badsites <- vector("character")
   for(site in siteNumbers) {
      i <- i + 1
      if(! silent) message(" dataRetrieval::readNWISsite() for ",site," (",i,"/",n,")")
      asitefile <- NULL
      try(asitefile <- dataRetrieval::readNWISsite(site), silent=TRUE)
      if(is.null(asitefile)) {
         warning(" site ",site," does not 'exist' in NWISWeb")
         badsites <- c(badsites, site)
         next;
      }
      if(haveFirst) {
         sitefile <- asitefile
         haveFirst <- FALSE
      } else {
         #sitefile <- merge(sitefile, asitefile, all=TRUE)
         sitefile <- rbind(sitefile, asitefile)
      }
   }
   if(! usesp) {
      attr(sitefile, "bad sites") <- badsites
      return(sitefile)
   }

   if(is.na(latlongcrs)) {
      LATLONG <- paste0("+init=epsg:4269 +proj=longlat +ellps=GRS80 ",
                        "+datum=NAD83 +no_defs +towgs84=0,0,0")
      message("**using the default geographic coordinates CRS string:\n'", LATLONG,"'\n")
      LATLONG <- sp::CRS(LATLONG)
   } else {
      LATLONG <- sp::CRS(latlongcrs)
   }

   sitefile$CDA <- pmin(sitefile$drain_area_va,
                        sitefile$contrib_drain_area_va, na.rm=TRUE)

   sitefile$inventory_dt    <- NULL
   sitefile$instruments_cd  <- NULL
   sitefile$construction_dt <- NULL
   sitefile$gw_file_cd      <- NULL
   sitefile$nat_aqfr_cd     <- NULL
   sitefile$aqfr_cd         <- NULL
   sitefile$aqfr_type_cd    <- NULL
   sitefile$well_depth_va   <- NULL
   sitefile$hole_depth_va   <- NULL
   sitefile$depth_src_cd    <- NULL
   sitefile$project_no      <- NULL
 
   # The sp library is needed here
   coords  <- cbind(sitefile$dec_long_va, sitefile$dec_lat_va)
   spSites <- sp::SpatialPointsDataFrame(coords, sitefile, proj4string=LATLONG)

   if(is.na(proj4string)) {
      ALBEA <- paste0("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 ",
                      "+datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0")
      message("**unprojected data are being returned**\n",
              "**consider using the 'proj4string' argument with a CRS, which could look\n",
              "**something like this, say, for a Mississippi Embayment Albers-Equal Area:\n'",
              ALBEA,"'\n")
      return(spSites)
   } else {
      return(sp::spTransform(spSites, sp::CRS(proj4string)))
   }
}
