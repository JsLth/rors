#' ORS accessibility maps
#' @description \code{ors_accessibility} returns isochrone or isodistance data
#' for a set of source points, either as polygons or as a raster.
#' @param range Character. If length-1, specifies the maximum range which
#' can be further broken down using the argument \code{interval}. If of length
#' larger than 1, specifies manual breaks. The unit can be controlled using the
#' argument \code{range_type}.
#' @param attributes Additional features to be included the output. Only relevant
#' if \code{rasterize = FALSE}. Can be one of \code{"area"}, \code{"reachfactor"}
#' and \code{"total_pop"}.
#' @param intersections Logical. Wether to return overlapping polygons.
#' @param interval Integer. If \code{range} is a length-1 vector, defines the
#' distance interval to break down the isochrones.
#' @param location_type Character. Whether to route from (\code{"start"}) or to
#' (\code{"destination"}) the points of the input data.
#' @param range_type Character. Type of distance that the calculations should be
#' based on. \code{"time"} produces isochrones while \code{"distance"} produces
#' isodistance polygons.
#' @param smoothing Integer. Smoothing factor between 0 and 100 to be applied to
#' the polygon geometries. For details, refer to the API playground.
#' @param area_units Character. Distance unit to use for the \code{area} attribute
#' from \code{attributes}.
#' @param units Character. Units to be used for the \code{range} argument if
#' \code{range_type = "distance"}.
#' @param rasterize Logical. If \code{FALSE}, simply returns the isochrone polygon
#' geometries. If \code{TRUE}, converts isochrones to a raster.
#' @param raster_resolution Numeric. Length-1 vector specifying the resolution of
#' the output raster in meters (x, y) if \code{rasterize = TRUE}.
#' @param ... Additional arguments passed to the ORS API. This includes all
#' options that modify the routing results. For details on each argument,
#' refer to the
#' \href{https://openrouteservice.org/dev/#/api-docs/v2/isochrones/{profile}/post}{API playground}
#' and
#' \href{https://giscience.github.io/openrouteservice/documentation/routing-options/Routing-Options.html}{documentation}.
#' \describe{
#'  \item{\strong{avoid_borders}}{Length-1 character vector specifying whether
#'                                to avoid, all borders, only controlled ones
#'                                or none. Only available for \code{driving-*}.}
#'  \item{\strong{avoid_countries}}{Numeric vector listing countries to avoid.
#'                                  Each country is assigned a numeric value.
#'                                  Refer to the ORS documentation. Only
#'                                  available for \code{driving-*}.}
#'  \item{\strong{avoid_features}}{Character vector containing traffic features
#'                                 to avoid (e.g. highways or tunnels).}
#'  \item{\strong{avoid_polygons}}{\code{sf} or \code{sfc} object describing
#'                                 areas to avoid.}
#'  \item{\strong{profile_params}}{Nested list containing restrictions and
#'                                 weightings for \code{driving-hgv},
#'                                 \code{cycling-*}, \code{walking},
#'                                 \code{hiking} and \code{wheelchair}.}
#'  \item{\strong{vehicle_type}}{Length-1 character vector specifying the type
#'                               of heavy goods vehicle. Needed to set
#'                               restrictions for \code{driving-hgv}.}
#' }
#' 
#' @returns If \code{rasterize = FALSE}, returns an \code{sf} object containing
#' the isochrone or isodistance polygon geometries as well as additional
#' attributes. Each polygon's outer lines represent places of equal distance or
#' travel time from or to the respective point in the source dataset. If
#' \code{rasterize = FALSE}, returns a \code{raster} object with a discrete
#' value classification.
#' 
#' @examples
#' \dontrun{
#' set.seed(111)
#' source_sf <- ors_sample(10, as_sf = TRUE)
#' source_df <- ors_sample(10)
#' 
#' # Returns a polygon sf dataframe divided by four 15 minute time breaks
#' # Also contains information on the area and population inside the isochrones
#' ors_accessibility(
#'     source_sf,
#'     profile = "driving-car",
#'     range = c(900, 1800, 2700, 3600),
#'     attributes = c("area", "total_pop"),
#'     location_type = "start",
#'     range_type = "time",
#'     rasterize = TRUE
#' )
#' 
#' # Returns a raster image classified by 3 distance intervals
#' ors_accessibility(
#'     source_df,
#'     profile = "driving-car",
#'     range = c(1000, 5000, 10000),
#'     location_type = "destination",
#'     range_type = "distance"
#' )
#' 
#' # Returns a polygon sf dataframe that is broken down every 50 meters up until
#' # the maximum distance of 500 meters.
#' ors_accessibility(
#'    source_df,
#'    profile = "cycling-regular",
#'    range = 500,
#'    location_type = "start",
#'    interval = 50,
#'    range_type = "distance"
#' )
#' }
#' 
#' @inheritParams ors_distances
#' 
#' @export

ors_accessibility <- function(source,
                              profile = get_profiles(force = TRUE),
                              range = c(300L, 200L),
                              attributes = "area",
                              intersections = FALSE,
                              interval = 30L,
                              location_type = c("start", "destination"),
                              range_type = c("time", "distance"),
                              smoothing = 25L,
                              area_units = c("m", "km", "mi"),
                              units = c("m", "km", "mi"),
                              rasterize = FALSE,
                              raster_resolution = c(100L, 100L),
                              ...) {
  profile <- match.arg(profile)
  location_type <- match.arg(location_type)
  range_type <- match.arg(range_type)
  area_units <- match.arg(area_units)
  units <- match.arg(units)
  
  source <- format_input_data(source)
  
  options <- format_ors_options(list(...), profile)
  
  url <- get_ors_url()

  res <- query_isochrone(
    source = source,
    profile = profile,
    range = range,
    attributes = attributes,
    intersections = intersections,
    interval = interval,
    location_type = location_type,
    options = options,
    range_type = range_type,
    smoothing = smoothing,
    area_units = area_units,
    units = units,
    url = url
  )
  
  handle_ors_conditions(res, abort_on_error = TRUE, warn_on_warning = TRUE)

  isochrones <- ors_polygon(res)
  
  if (isTRUE(rasterize)) {
    isochrones <- rasterize_isochrones(isochrones, resolution = raster_resolution)
  }
  
  isochrones
}
