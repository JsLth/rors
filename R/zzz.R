.onAttach <- function(libname, pkgname) {
  packageStartupMessage(paste(
    "© openrouteservice.org by HeiGIT | Data © OpenStreetMap contributors,",
    "ODbL 1.0. https://www.openstreetmap.org/copyright"
  ))
}
