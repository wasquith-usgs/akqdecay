.onAttach <- function(libname, pkgname) {
  packageStartupMessage(paste(
  "USGS Research Package:\n",
  "https://owi.usgs.gov/R/packages.html#research", sep=""))
}
