#' ORS accessibility maps
#' @description \code{ors_accessibility} returns isochrone or isodistance data
#' for a set of source points, either as polygons or as a raster.
#' @param range \code{[character]}
#'
#' If length-1, specifies the maximum range which
#' can be further broken down using the argument \code{interval}. If of length
#' larger than 1, specifies manual breaks. The unit can be controlled using the
#' argument \code{range_type}.
#' @param attributes \code{[character]}
#'
#' Additional features to be included the output. Only relevant
#' if \code{rasterize = FALSE}. Can be one of \code{"area"}, \code{"reachfactor"}
#' and \code{"total_pop"}.
#' @param intersections \code{[logical]}
#'
#' Whether to return the overlapping area of seperate isochrones. If
#' \code{TRUE}, the response will not contain the actual isochrones but only
#' their intersections. See below for details. Defaults to \code{FALSE}.
#' @param interval \code{[integer]}
#'
#' If \code{range} is a length-1 vector, defines the distance interval to break
#' down the isochrones. For example, if \code{range} is 300 and \code{interval}
#' is 50, creates 6 isochrones where each isochrone is 50 units further from
#' the center. If \code{length(range) > 1}, this argument is ignored. Defaults
#' to 30.
#' @param location_type \code{[character]}
#'
#' Whether to route from (\code{"start"}) or to (\code{"destination"}) the
#' points of the input data. Defaults to \code{"start"}.
#' @param range_type \code{[character]}
#'
#' Type of distance that the calculations should be based on. \code{"time"}
#' produces isochrones while \code{"distance"} produces isodistance polygons.
#' Defaults to \code{"time"}.
#' @param smoothing \code{[integer]}
#'
#' Smoothing factor between 0 and 100 to be applied to the polygon geometries.
#' For details, refer to the API playground. Defaults to 25.
#' @param area_units \code{[character]}
#'
#' Distance unit to use for the \code{area} attribute from \code{attributes}.
#' Defaults to \code{"m"}.
#' @param units \code{[character]}
#'
#' Units to be used for the \code{range} argument if \code{range_type = "distance"}.
#' Defaults to \code{"m"}.
#' @param rasterize \code{[logical]}
#'
#' If \code{FALSE}, returns the isochrone polygon geometries. If \code{TRUE},
#' aggregates polygon distance values onto a vector grid and then rasterizes
#' the vector grid to a \code{SpatRaster} object created using
#' \code{\link[terra]{rast}}. When rasterizing, only the distance value is
#' preserved while all variables specified in \code{attributes} are discarded.
#' Requires the \code{terra} package to be installed. Defaults to
#' \code{FALSE}.
#' @param raster_resolution \code{[numeric]}
#'
#' If \code{rasterize = TRUE}, specifies the resolution of the template raster.
#' Corresponds to the number of cells in each direction (x, y). This argument is
#' passed on to \code{\link[sf]{st_make_grid}} and \code{\link[terra]{rast}}.
#' Defaults to 100x100.
#' @param ... Additional arguments passed to the ORS API. This includes all
#' options that modify the routing results. For details on each argument,
#' refer to the
#' \href{https://openrouteservice.org/dev/#/api-docs/v2/isochrones/{profile}/post}{API playground}
#' and
#' \href{https://giscience.github.io/openrouteservice/api-reference/endpoints/directions/routing-options}{documentation}.
#' Reasonable arguments include \code{avoid_borders}, \code{avoid_countries},
#' \code{avoid_features}, \code{avoid_polygons}, profile parameters, and
#' \code{vehicle_type}.
#' @inheritParams ors_pairwise
#'
#' @returns If \code{rasterize = FALSE} and \code{intersections = FALSE},
#' returns an \code{sf} object containing the isochrone or isodistance polygon
#' geometries as well as additional attributes. Each polygon's outer lines
#' represent places of equal distance or travel time from or to the respective
#' point in the source dataset.
#'
#' If \code{rasterize = TRUE}, returns a
#' \code{raster} object with a discrete value classification.
#'
#' If \code{intersections = TRUE}, returns an \code{sf} dataframe containing
#' the intersections between two respective isochrones and their indices.
#' The response contains four indices. Index names starting \code{a_*}
#' specify the first isochrone of the intersection and names starting with
#' \code{b_*} specify the second isochrone. Index names ending with
#' \code{*_index} correspond to the \code{group_index} of the isochrones.
#' Index names ending with \code{*_range} correspond to the index of values
#' in the \code{range} argument. For example, if \code{range = c(200, 300)},
#' a \code{a_range} of 1 would correspond to 300. Taken together, each
#' intersection can be assigned to two isochrone and each isochrone can be
#' identified using their group index and their respective range.
#'
#' @examples
#' \dontrun{
#' data("pharma")
#'
#' # Returns a polygon sf dataframe divided by four 15 minute time breaks
#' # Also contains information on the area and population inside the isochrones
#' ors_accessibility(
#'   pharma,
#'   profile = "driving-car",
#'   range = c(900, 1800, 2700, 3600),
#'   attributes = c("area", "total_pop"),
#'   location_type = "start",
#'   range_type = "time",
#'   rasterize = TRUE
#' )
#'
#' # Returns a raster image classified by 3 distance intervals
#' ors_accessibility(
#'   pharma,
#'   profile = "driving-car",
#'   range = c(1000, 5000, 10000),
#'   location_type = "destination",
#'   range_type = "distance"
#' )
#'
#' # Returns a polygon sf dataframe that is broken down every 50 meters up until
#' # the maximum distance of 500 meters.
#' ors_accessibility(
#'   pharma,
#'   profile = "cycling-regular",
#'   range = 500,
#'   location_type = "start",
#'   interval = 50,
#'   range_type = "distance"
#' )}
#'
#'
#' @export
ors_accessibility <- function(src,
                              profile = get_profiles(force = FALSE),
                              range = c(200, 300),
                              attributes = "area",
                              intersections = FALSE, # to-do: check intersections
                              interval = 30,
                              location_type = c("start", "destination"),
                              range_type = c("time", "distance"),
                              smoothing = 25,
                              area_units = c("m", "km", "mi"),
                              units = c("m", "km", "mi"),
                              rasterize = FALSE, # to-do: revise rasterize
                              raster_resolution = c(100, 100),
                              instance = NULL,
                              ...,
                              params = NULL) {
  instance <- instance %||% get_instance()
  url <- get_ors_url(instance)
  ts <- timestamp()
  assert_endpoint_available(url, "isochrones")

  if (rasterize && !loadable("terra")) {
    cli::cli_abort(
      "The {.pkg raster} package is needed to rasterize isochrones.",
      class = "ors_loadable_error"
    )
  }

  # Check if ORS is ready to use
  ors_ready(force = FALSE, error = TRUE, url = url)

  profile <- match.arg(profile)
  location_type <- match.arg(location_type)
  range_type <- match.arg(range_type)
  area_units <- if ("area" %in% attributes) match.arg(area_units)
  units <- if ("distance" %in% range_type) match.arg(units)

  src <- prepare_input(src)
  params <- params %||% prepare_ors_params(list(...), profile)

  res <- call_ors_isochrones(
    src = src,
    profile = profile,
    range = range,
    attributes = attributes,
    intersections = intersections,
    interval = interval,
    location_type = location_type,
    params = params,
    range_type = range_type,
    smoothing = smoothing,
    area_units = area_units,
    units = units,
    url = url,
    token = needs_token(instance$token)
  )

  handle_ors_conditions(res, ts, abort_on_error = TRUE, warn_on_warning = TRUE)

  if (intersections) {
    res <- ors_contours(res)
  } else {
    res <- ors_polygon(res)
  }

  if (isTRUE(rasterize)) {
    res <- rasterize_isochrones(res, raster_resolution)
  }

  res
}
