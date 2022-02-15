# Title     : Route inspection functions
# Objective : Acquire more in-depth information about single routes
# Created by: Jonas Lieth
# Created on: 22.10.2021


#' Route inspection
#' @description Calls the directions service once to get a closer look at route
#' characteristics and attributes.
#'
#' \code{inspect_route} returns all line segments of a route along with a set
#' of additional attributes
#' @param source Any kind of numeric vector containing \code{x/y} values of a
#' route segment that should be routed from. Refer to
#' \code{\link{ors_distances}}.
#' @param destination Any kind of numeric vector containing \code{x/y} values
#' of a route segment that should be routed to.
#' @param attributes List of attributes that summarize route characteristics.
#' This includes two values: \code{avgspeed} states the average vehicle speed
#' along the route, \code{detourfactor} indicates how much the route deviates
#' from a straight line. If \code{TRUE}, both values are included.
#' @param elevation If \code{TRUE}, elevation data is included in the output.
#' @param extra_info List of keywords that add extra information regarding each
#' linestring segment of the output. Possible values include:
#' \itemize{
#'  \item steepness
#'  \item suitability
#'  \item surface
#'  \item waycategory
#'  \item waytype
#'  \item tollways
#'  \item traildifficulty
#'  \item osmid
#'  \item roadaccessrestrictions
#'  \item countryinfo
#'  \item green
#'  \item noise
#' }
#' If \code{TRUE}, all values are included.
#' @param elev_as_z If \code{TRUE}, elevation data is stored as z-values in the
#' geometry of the output \code{sf} dataframe. If \code{FALSE}, elevation is
#' stored as a distinct dataframe column. Ignored if \code{elevation = FALSE}.
#' @inheritParams ors_distances
#' @returns A dataframe containing linestrings and additional information for each
#' route segment
#' @details Refer to the
#' \href{https://github.com/GIScience/openrouteservice-docs#routing-response}{routing response documentation}
#' and the
#' \href{https://openrouteservice.org/dev/#/api-docs/v2/directions/{profile}/post}{API playground}
#' for more information on the route inspection arguments and their response
#' behavior.
#' @seealso ors_distances
#'
#' @export
#' 
#' @examples
#' \dontrun{
#' sample_source <- ors_sample(1)
#' sample_dest <- ors_sample(1)
#' profile = get_profiles()[1]
#' 
#' # Basic inspection without extra information
#' insp <- inspect_route(sample_source, sample_dest, profile)
#' 
#' # Advanced inspection with extra information
#' insp_adv <- inspect_route(sample_source,
#'                           sample_dest,
#'                           profile,
#'                           extra_info = TRUE)
#'                           
#' # Inspection of route elevation data
#' insp_elev <- inspect_route(sample_source,
#'                            sample_dest,
#'                            profile,
#'                            elevation = TRUE,
#'                            elev_as_z = FALSE)
#' 
#' # Inspection of route summary attributes
#' insp_attr <- inspect_route(sample_source,
#'                            sample_ dest,
#'                            profile,
#'                            attributes = "detourfactor")
#' attr(insp_attr, "detourfactor")
#' 
#' # Altering the route by passing further arguments
#' insp_opts <- inspect_route(sample_source,
#'                            sample_dest,
#'                            profile,
#'                            continue_straight = TRUE,
#'                            preference = "shortest",
#'                            maximum_speed = 80)
#' }

inspect_route <- function(source,
                          destination,
                          profile = get_profiles(),
                          attributes = list(),
                          elevation = TRUE,
                          extra_info = list(),
                          ...,
                          elev_as_z = FALSE) {
  # Check if ORS is ready to use
  ors_ready(force = TRUE, error = TRUE)

  # Bring input data into shape
  source <- format_input_data(source)
  destination <- format_input_data(destination)

  verify_crs(source, crs = 4326L)
  verify_crs(destination, crs = 4326L)

  profile <- match.arg(profile)

  url <- get_ors_url()

  features <- list(attributes = attributes,
                   elevation = elevation,
                   extra_info = extra_info)
  options <- format_ors_options(append(features, list(...)), profile)

  res <- query_ors_directions(source = source,
                              destination = destination,
                              profile = profile,
                              units = "m",
                              geometry = TRUE,
                              options = options,
                              url = url)

  handle_ors_conditions(res, abort_on_error = TRUE, warn_on_warning = TRUE)

  geometry <- ors_multiple_linestrings(res, elev_as_z)
  elevation <- data.frame(elevation = attr(geometry, "elevation"))

  waypoints <- res$features$properties$segments[[1L]]$steps[[1L]]$way_points

  distances <- calculate_distances(geometry)
  durations <- calculate_durations(res, distances$distance)
  speeds <- calculate_avgspeed(distances$distance, durations$duration)

  if (is.element("avgspeed", options$attributes)) {
    avgspeed <- res$features$properties$segments[[1L]]$avgspeed
  } else avgspeed <- NULL

  if (is.element("detourfactor", options$attributes)) {
    detourfactor <- res$features$properties$segments[[1L]]$detourfactor
  } else detourfactor <- NULL

  names_wp <- res$features$properties$segments[[1L]]$steps[[1L]]$name
  expanded_names <- expand_by_waypoint(names_wp, waypoints)
  names <- gsub(pattern = "-", replacement = NA, expanded_names)
  names <- data.frame(names = names)

  ascent <- res$features$properties$segments[[1L]]$ascent
  descent <- res$features$properties$segments[[1L]]$descent

  extra_info <- vapply(options$extra_info,
                       function(x) format_extra_info(res, x),
                       data.frame(1L))

  elements <- list(
    names,
    distances,
    durations,
    speeds,
    elevation,
    extra_info,
    geometry,
    stringsAsFactors = TRUE
  )

  elements <- elements[lengths(elements) != 0L]
  route <- do.call(data.frame, elements)

  route_sf <- structure(
    sf::st_as_sf(route),
    avgspeed = avgspeed,
    detourfactor = detourfactor,
    ascent = ascent,
    descent = descent
  )
  route_sf
}


#' Route inspection
#' @description \code{summarize_route} generates a range of summary tables and
#' values that provide an overview of a route.
#' @returns Object of type \code{route_summary} that contains information on
#' distances, durations, speed, elevation, detour factors as well as all
#' available extra information for the requested route.
#' @rdname inspect_route
#'
#' @export
#' 
#' @examples
#' \dontrun{
#' route_summary <- summarize_route(sample_source, sample_dest, profile)
#' }

summarize_route <- function(source,
                            destination,
                            profile = get_profiles(),
                            ...) {
  # Check if ORS is ready to use
  ors_ready(force = TRUE, error = TRUE)

  # Bring input data into shape
  source <- format_input_data(source)
  destination <- format_input_data(destination)

  verify_crs(source, crs = 4326L)
  verify_crs(destination, crs = 4326L)

  profile <- match.arg(profile)

  url <- get_ors_url()

  features <- list(attributes = TRUE, elevation = TRUE, extra_info = TRUE)
  options <- format_ors_options(append(features, list(...)), profile)

  res <- query_ors_directions(source = source,
                              destination = destination,
                              profile = profile,
                              units = "m",
                              geometry = TRUE,
                              options = options,
                              url = url)

  handle_ors_conditions(res, abort_on_error = TRUE, warn_on_warning = FALSE)

  # Custom summary tables
  geometry <- ors_multiple_linestrings(res, elev_as_z = FALSE)
  elevation <- attr(geometry, "elevation")

  distances <- calculate_distances(geometry)
  durations <- calculate_durations(res, distances$distance)
  speeds <- calculate_avgspeed(distances$distance, durations$duration)$avgspeed

  speeds_summary <- make_summary_table(unlist(speeds), distances)
  elevation_summary <- make_summary_table(elevation, distances)

  # ORS summary tables
  get_ors_summaries <- function(info_type) {
    summary <- extras[[info_type]]$summary[[1L]]
    summary$value <- fill_extra_info(summary$value, info_type, profile)
    summary
  }

  extras <- res$features$properties$extras
  summaries <- sapply(names(extras), get_ors_summaries, simplify = FALSE)

  summaries <- sapply(summaries, function(summary) {
    summary <- stats::aggregate(summary[, c(2L, 3L)],
                         by = summary["value"],
                         sum)
    row.names(summary) <- summary$value
    summary$value <- NULL
    summary
  }, simplify = FALSE)

  # Append summary tables and set units
  elev_speeds <- list(elevation = elevation_summary, avgspeed = speeds_summary)
  summaries <- append(summaries,
                      elev_speeds,
                      after = 0L)

  if (requireNamespace("units")) {
    summaries <- lapply(summaries, function(s) {
      s["distance"] <- units::as_units(s$distance, "m")
      s
    })
  }

  output <- list(
    distance = extract_ors_attribute(res, "distance"),
    duration = extract_ors_attribute(res, "duration"),
    detourfactor = extract_ors_attribute(res, "detourfactor"),
    ascent = extract_ors_attribute(res, "ascent"),
    descent = extract_ors_attribute(res, "descent"),
    speed = summary(speeds),
    elevation = summary(elevation),
    tables = summaries
  )

  if (requireNamespace("units")) {
    units(output$distance) <- "m"
    units(output$duration) <- "s"
  }

  class(output) <- "route_summary"
  output
}
