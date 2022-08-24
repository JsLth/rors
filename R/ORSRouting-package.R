#' @keywords internal
#'
#' @section Usage:
#' To learn more about the usage of this package, look into the two
#' vignettes for this package:
#' \preformatted{
#' vignette("ors-installation", package = "ORSRouting")
#' vignette("ors-routing", package = "ORSROuting")
#' }
#' @references
#' This package is powered by OpenRouteService. For problems concerning their
#' service, refer to <https://github.com/GIScience/openrouteservice>
#'
#' © openrouteservice.org by HeiGIT | Map data © OpenStreetMap contributors
#' @encoding UTF-8
"_PACKAGE"

# The following block is used by usethis to automatically manage
# roxygen namespace tags. Modify with care!
## usethis namespace: start
## usethis namespace: end
NULL

utils::globalVariables(c(
  "source_shape", "dest_shape", "cond_indices", "var", "obj_var", "set_var",
  "geom_type", "format", "port_chr", "nam", "val", "cur_ports_df", "verbose",
  "poly", "source_shape", "dest_shape", "url", "options", "cond_indices",
  "uneval_expr"
  
))
