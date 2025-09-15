.onAttach <- function(libname, pkgname) {
  packageStartupMessage(paste(
    "(c) openrouteservice.org by HeiGIT | Data (c) OpenStreetMap",
    "contributors, ODbL 1.0. https://www.openstreetmap.org/copyright"
  ))
}


.onLoad <- function(libname, pkgname) {
  ors_instance(server = "public")
}
