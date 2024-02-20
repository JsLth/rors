.onAttach <- function(libname, pkgname) {
  packageStartupMessage(paste(
    "\u00a9 openrouteservice.org by HeiGIT | Data \u00a9 OpenStreetMap",
    "contributors, ODbL 1.0. https://www.openstreetmap.org/copyright"
  ))
}
