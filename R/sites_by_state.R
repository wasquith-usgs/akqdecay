sites_by_state <-
function(states, parameterCd="00060", site_tp_cd="ST",
                 agency_cd="USGS", ditch_15digit=TRUE,
                 rm_alt=TRUE, rm_coord=TRUE, rm_countystate=TRUE,
                 rm_huc=TRUE, rm_tz=TRUE,
                 rm_drain=TRUE, makeCDA=TRUE,
                 simplify=TRUE, strip_single_quote_mark=TRUE,
                 silent=FALSE, ...) {

  if(is.null(states)) states <- datasets::state.abb

  sites <- NULL
  for(state in states) {
    if(! any(state == datasets::state.abb)) {
      warning(state," is not in the 'datasets::state.abb' list, skipping")
      next
    }
    if(! silent) message(state,"-", appendLF=FALSE)
    st <- dataRetrieval::whatNWISsites(stateCd=state, parameterCd=parameterCd)
    st <- st[st$site_tp_cd == site_tp_cd,]
    if(ditch_15digit) {
      st <- st[as.numeric(st$site_no) <= 100000000000000,]
    }
    sites <- c(sites, st$site_no)
  }

  if(length(sites) == 0) {
    return(NULL)
  }
  if(! silent) message("done")

  sf <- dataRetrieval::readNWISsite(sites)
  sf <- sf[sf$agency_cd == agency_cd,]
  sf$station_nm <- sf$station_nm
  sf$station_nm <- unique(sf$station_nm)
  sf$station_nm <- stringr::str_to_upper(sf$station_nm)
  if(strip_single_quote_mark) {
    sf$station_nm <- gsub("'", "", sf$station_nm)
  }
  if(simplify & site_tp_cd == "ST") {
    sf$site_tp_cd      <- NULL
    sf$lat_va          <- NULL
    sf$long_va         <- NULL
    sf$coord_datum_cd  <- NULL
    sf$district_cd     <- NULL
    sf$land_net_ds     <- NULL
    sf$map_nm          <- NULL
    sf$map_scale_fc    <- NULL
    sf$basin_cd        <- NULL
    sf$topo_cd         <- NULL
    sf$instruments_cd  <- NULL
    sf$construction_dt <- NULL
    sf$inventory_dt    <- NULL
    sf$reliability_cd  <- NULL
    sf$local_time_fg   <- NULL
    sf$gw_file_cd      <- NULL
    sf$nat_aqfr_cd     <- NULL
    sf$aqfr_cd         <- NULL
    sf$aqfr_type_cd    <- NULL
    sf$well_depth_va   <- NULL
    sf$hole_depth_va   <- NULL
    sf$depth_src_cd    <- NULL
    sf$project_no      <- NULL
    sf$CDA <- pmin(sf$drain_area_va, sf$contrib_drain_area_va, na.rm=TRUE)
  }
  if(make_CDA) {
    sf$CDA <- pmin(sf$drain_area_va, sf$contrib_drain_area_va, na.rm=TRUE)
  }
  if(rm_alt) {
    sf$alt_va       <- NULL
    sf$alt_meth_cd  <- NULL
    sf$alt_acy_va   <- NULL
    sf$alt_datum_cd <- NULL
  }
  if(rm_coord) {
    sf$coord_meth_cd      <- NULL
    sf$coord_acy_cd       <- NULL
    sf$dec_coord_datum_cd <- NULL
  }
  if(rm_countystate) {
    sf$state_cd   <- NULL
    sf$county_cd  <- NULL
    sf$country_cd <- NULL
  }
  if(rm_drain) {
    sf$drain_area_va         <- NULL
    sf$contrib_drain_area_va <- NULL
  }
  if(rm_huc) {
    sf$huc_cd <- NULL
  }
  if(rm_tz) {
    sf$tz_cd <- NULL
  }
  sf
}
