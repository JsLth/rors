#' Route inspection
#' @description Calls the directions service once to get a closer look at route
#' characteristics and attributes. Produces a dataframe containing an elaborate
#' perspective on route sections and their contextual properties.
#' \code{ors_inspect} can also be used as a relatively low-level interface to
#' the directions service with the ability to return responses as parsed or
#' unparsed JSONs.
#'
#' The summary function computes summary statistics and interval tables for
#' a given route.
#' @param src \code{[sf]}
#'
#' Source dataset containing at least two point geometries for which routes are
#' to be computed. If \code{round_trip} is specified, \code{source} must contain
#' a single coordinate pair from which a round trip is generated.
#' @param level \code{[character]}
#'
#' Level of route aggregation. Must be one of \code{waypoint}, \code{step} or
#' \code{segment}. See details.
#' @param attributes \code{[list]}
#'
#' List of attributes that summarize route characteristics.
#' This includes three values: \code{avgspeed} states the average vehicle speed
#' along the route, \code{detourfactor} indicates how much the route deviates
#' from a straight line. \code{percentage} shows the share of a segment compared
#' to the entire route. If \code{TRUE}, all values are included.
#' @param elevation \code{[logical]}
#'
#' If \code{TRUE}, elevation data is included in the output.
#' @param navigation \code{[logical]}
#'
#' If \code{TRUE}, navigation information is included in the output, i.e.,
#' instructions and road exits. If \code{level == "segment"}, navigation data
#' is dropped from the output.
#'
#' @param alternative_routes \code{[list]}
#'
#' Named list that specifies options for alternative routes and accepts up to
#' three parameters. \code{target_count} is the maximum number of routes to
#' compute (including the recommended route). Must be an integer between 1 and 3
#' and defaults to 1. The output can contain less routes than specified if no
#' other alternatives can be computed from the \code{src} coordinates.
#' \code{share_factor} denotes the maximum share of identical paths between
#' routes. \code{weight_factor} is the maximum factor that a route can deviate
#' (i.e. be longer) from the original route. If specified, and
#' \code{target_count} is larger than 1, the output is wrapped in a list of up
#' to three dataframes. If \code{NULL}, no alternative routes are computed.
#' @param round_trip \code{[list]}
#'
#' Named list that specifies options for round trips and accepts up to three
#' parameters. \code{length} is the approximate length of the round trip.
#' \code{points} denotes the number of route points from which to derive a
#' round trip (the higher the rounder). \code{seed} controls the randomisation
#' of the route direction. If specified, \code{src} must contain a single
#' coordinate pair from which a round trip is to be generated. If \code{NULL},
#' no round trip is computed.
#' @param extra_info List of keywords that add extra information regarding each
#' linestring segment of the output. If \code{TRUE}, all values are included.
#' See details for more information.
#' @param as \code{[character]}
#'
#' How to format the output. If \code{"string"}, returns the entire JSON
#' response string. If \code{"list"}, returns a parsed JSON list. If
#' \code{"tidy"}, performs lots of data preparation to shape the response into a
#' tibble.
#' @param elev_as_z \code{[logical]}
#'
#' If \code{TRUE}, elevation data is stored as z-values in the
#' geometry of the output \code{sf} dataframe. If \code{FALSE}, elevation is
#' stored as a distinct dataframe column. Ignored if \code{elevation = FALSE}.
#' @inheritParams ors_pairwise
#' @returns Returns an sf dataframe containing detailed sections of all routes
#' between the coordinates specified in \code{src}, aggregated according to
#' the level of aggregation stated in \code{level}. If \code{alternative_routes}
#' is specified, returns a list of sf dataframes instead with each element
#' containing a route alternative.
#'
#' @details OpenRouteService distinguishes between three
#' types of route aggregation: Segments, steps and waypoints. A segment is a
#' single route between \code{src[i, ]} and \code{src[i + 1, ]}. A step
#' is a route section as relevant for a navigation system. A waypoint is a
#' straight connection between two geographical points on a route.
#'
#' Depending on the chosen level of aggregation, the output has to be adjusted
#' through interpolation and aggregation. For all levels below \code{"segment"},
#' ORS attributes are stored as R attributes and are not included in the
#' dataframe. For \code{"waypoint"}, distances and durations are derived from the
#' geometry lengths and do not take into account elevation (due to restrictions
#' in the s2 package). Extra information (using the \code{extra_info} argument)
#' and street names do not perfectly overlap with steps and segments. In these
#' cases, the value with the highest overlap is adopted causing some information
#' loss. Navigation information is dropped on \code{"segment"} level.
#'
#' Extra information can be requested as additional context for each waypoint on
#' a route. Possible values include:
#' \describe{
#'  \item{steepness}{Ordered factor describing how steep a part of a route is.}
#'  \item{suitability}{Ordinal numeric describing how suitable a part of a
#'                     route is (1 - unsuitable; 10 - suitable).}
#'  \item{surface}{Unordered factor describing the surface material covering a
#'                 part of a route.}
#'  \item{waycategory}{Unordered factor describing special parts of a route.}
#'  \item{waytype}{Unordered factor containing different types of roads.}
#'  \item{tollways}{For \code{driving-*} profiles, specifies whether a part of a
#'                  route is a tollway.}
#'  \item{traildifficulty}{For walking and driving profiles, specifies the OSM
#'                         trail difficulty.}
#'  \item{osmid}{For the wheelchair profile, contains the OSM IDs of used ways.}
#'  \item{roadaccessrestrictions}{Unordered factor describing road access
#'                                restriction.}
#'  \item{countryinfo}{Nominal numeric containing country codes of a part of a
#'                     route.}
#'  \item{green}{For walking profiles, describes the amount of green on a route
#'               (1 - little; 10 - much).}
#'  \item{noise}{For walking profiles, describes the amount of noise on a route
#'               (1 - little; 10 - much).}
#' }
#'
#' @seealso \code{\link{ors_pairwise}},
#' \code{\link{plot_section}}
#'
#' @export
#'
#' @examples
#' \dontrun{
#' sample_source <- ors_sample(1)
#' sample_dest <- ors_sample(1)
#' profile <- get_profiles()[1]
#'
#' # Basic inspection without extra information
#' insp <- ors_inspect(sample_source, sample_dest, profile)
#'
#' # Advanced inspection with extra information
#' insp_adv <- ors_inspect(
#'   sample_source,
#'   sample_dest,
#'   profile,
#'   extra_info = TRUE
#' )
#'
#' # Inspection of route elevation data
#' insp_elev <- ors_inspect(
#'   sample_source,
#'   sample_dest,
#'   profile,
#'   elevation = TRUE,
#'   elev_as_z = FALSE
#' )
#'
#' # Inspection of route summary attributes
#' insp_attr <- ors_inspect(
#'   sample_source,
#'   sample_dest,
#'   profile,
#'   attributes = "detourfactor"
#' )
#' attr(insp_attr, "detourfactor")
#'
#' # Altering the route by passing further arguments
#' insp_opts <- ors_inspect(
#'   sample_source,
#'   sample_dest,
#'   profile,
#'   continue_straight = TRUE,
#'   preference = "shortest",
#'   maximum_speed = 80
#' )
#'
#' # Summarizing route specifics
#' route_summary <- summary(insp_adv)
#' }
ors_inspect <- function(src,
                        profile = get_profiles(),
                        level = c("waypoint", "step", "segment"),
                        attributes = NULL,
                        extra_info = NULL,
                        elevation = TRUE,
                        navigation = FALSE,
                        alternative_routes = NULL,
                        round_trip = NULL,
                        as = c("tidy", "list", "string"), # todo: revise tidy approach
                        elev_as_z = FALSE,
                        instance = NULL,
                        ...) {
  assert_that(
    is_sf(src),
    is_true_or_false(elev_as_z)
  )
  profile <- match.arg(profile)
  level <- match.arg(level)
  as <- match.arg(as)
  instance <- check_instance(instance)
  iid <- get_id(instance = instance)

  # Check if ORS is ready to use
  ors_ready(force = TRUE, error = TRUE, id = iid)

  # Bring input data into shape
  src <- prepare_input(src)
  url <- get_ors_url(id = iid)

  features <- list(
    attributes = attributes,
    elevation = elevation,
    extra_info = extra_info,
    alternative_routes = alternative_routes,
    round_trip = round_trip
  )
  features <- features[lengths(features) > 0]
  params <- format_ors_params(c(features, list(...)), profile)

  res <- call_ors_directions(
    src = src,
    profile = profile,
    units = "m",
    geometry = TRUE,
    params = params,
    url = url,
    token = needs_token(instance$token),
    parse = as != "raw"
  )

  if (as %in% c("list", "tidy")) {
    handle_ors_conditions(res, abort_on_error = TRUE, warn_on_warning = TRUE)

    if (as == "tidy") {
      res <- route_to_df(
        res,
        level = level,
        elevation = elevation,
        navigation = navigation,
        elev_as_z = elev_as_z,
        params = params
      )
    }
  }

  class(res) <- c("ors_route", class(res))
  res
}


#' Plot route cross-sections
#'
#' @description Plot the segments of a route as an elevation profile or
#' cross-section. A geographic cross-section describes a two-dimensional cut
#' through a route where the x-axis denotes the distance and the y-axis denotes
#' the elevation. The area below the elevation profile can be used to plot an
#' additional feature.
#'
#' @param x \code{[sf]}
#'
#' An \code{sf} data.frame describing segments of a linestring. The data.frame
#' is expected to have multiple rows (representing the segments) and at least
#' three columns, \code{"elevation"}, \code{"distance"} and an additional
#' feature. Preferably, this is a result of \code{ors_inspect}, but other
#' \code{sf} data.frames might work as well.
#'
#' @param dist,elev,feat \code{[character]}
#'
#' Column names of the distance, elevation and feature values inside \code{x}.
#' @param palette \code{[character]}
#'
#' Color palette to be used for plotting. Passed on to
#' \code{\link[ggplot2]{scale_fill_manual}} or
#' \code{\link[ggplot2]{scale_fill_gradientn}}, depending on the data type of
#' \code{feat}. Defaults to the Cividis colormap.
#' @param scale_elevation \code{[logical]}
#'
#' Whether to scale the elevation axis based on the lowest elevation. If
#' \code{FALSE}, the y-axis is not scaled and fixed at sea level. Defaults to
#' \code{TRUE}.
#' @param size \code{[numeric]}
#'
#' Size of the line plot. Defaults to \code{1.5}.
#' @param xlab,ylab,scale_title,title,subtitle,caption \code{[character]}
#'
#' Arguments to change label names, legend, title, subtitle and caption of the
#' ggplot object.
#' @param ... Further arguments passed to
#' \code{\link[ggplot2]{scale_fill_manual}} or
#' \code{\link[ggplot2]{scale_fill_gradientn}} depending on the data type of
#' \code{feat}.
#'
#' @returns A \code{ggplot} object.
#'
#' @examples
#' \dontrun{
#' sample <- ors_sample(2)
#' route <- ors_inspect(sample[1, ], sample[2, ], extra_info = TRUE)
#'
#' plot_section(route, feat = "waytype")
#'
#' plot_section(route, feat = "steepness", scale_elevation = FALSE)
#'
#' zissou_pal <- hcl.colors(11, palette = "Zissou 1")
#' plot_section(route, "steepness", palette = zissou_pal)
#'
#' terrain_pal <- hcl.colors(10, palette = "Terrain")
#' plot_section(route, "elevation", palette = terrain_pal, scale_elevation = FALSE)
#' }
#'
#' @references
#' \url{https://giscience.github.io/openrouteservice-r/articles/openrouteservice.html}
#'
#' @export
plot_section <- function(x,
                         feat = "avgspeed",
                         dist = "distance",
                         elev = "elevation",
                         palette = NULL,
                         scale_elevation = TRUE,
                         size = 1.5,
                         xlab = dist,
                         ylab = elev,
                         scale_title = feat,
                         title = NULL,
                         subtitle = NULL,
                         caption = NULL,
                         ...) {
  if (!loadable("ggplot2")) {
    cli::cli_abort("The {.pkg ggplot2} package is necessary to create cross-sections.")
  }

  distance <- if (dist %in% names(x)) {
    x[[dist]]
  } else {
    cli::cli_abort("No distance data found in {.var x}.")
  }
  if (inherits(distance, "units")) {
    distance <- units::drop_units(units::set_units(distance, "km"))
  }

  elevation <- if (elev %in% names(x)) {
    x[[elev]]
  } else if (is_sf(x)) {
    sf::st_geometry(x)[, 3]
  } else {
    cli::cli_abort("No elevation data found in {.var x}.")
  }
  if (inherits(elevation, "units")) {
    elevation <- units::drop_units(units::set_units(elevation, "m"))
  }

  val <- if (feat %in% names(x)) {
    x[[feat]]
  } else {
    cli::cli_abort("No feature data found in {.var x}.")
  }
  if (inherits(val, "units")) {
    val <- units::drop_units(val)
  }

  n <- nrow(x)

  seg <- data.frame(
    xstart = cumsum(c(0, distance[-(n - 1)])),
    xend = cumsum(distance),
    ystart = elevation,
    yend = c(elevation[-1], utils::tail(elevation, 1))
  )
  y_range <- range(elevation) * c(0.9, 1.1)
  y_range[1] <- y_range[1] * scale_elevation

  ids <- seq(1, n)
  zip_x <- unlist(Map(
    FUN = c,
    xmin1 = seg$xstart,
    xmax1 = seg$xend,
    xmax2 = seg$xend,
    xmin2 = seg$xstart
  ))

  zip_y <- unlist(Map(
    FUN = c,
    ymin1 = rep(y_range[1], n),
    ymin2 = rep(y_range[1], n),
    ymax1 = seg$yend,
    ymay2 = seg$ystart
  ))

  poly <- data.frame(x = zip_x, y = zip_y, id = rep(ids, each = 4))

  val <- lapply(seq_along(val), function(i) if (is.na(val[i])) val[i - 1] else val[i])
  val <- data.frame(
    value = unlist(val),
    id = ids
  )

  poly <- merge(poly, val, by = "id")

  p <- ggplot2::ggplot() +
    ggplot2::geom_segment(
      data = seg,
      mapping = do.call(
        ggplot2::aes,
        lapply(list(x = "xstart", y = "ystart", xend = "xend", yend = "yend"), as.name)
      ),
      size = 1.5,
      lineend = "round",
      na.rm = TRUE
    ) +
    ggplot2::geom_polygon(
      data = poly,
      mapping = do.call(
        ggplot2::aes,
        lapply(list(x = "x", y = "y", group = "id", fill = "value"), as.name)
      ),
      na.rm = TRUE
    ) +
    units::scale_x_units(unit = "km", expand = c(0, 0)) +
    units::scale_y_units(unit = "m", expand = c(0, 0), limits = y_range) +
    ggplot2::labs(
      x = xlab,
      y = ylab,
      title = title,
      subtitle = subtitle,
      caption = caption
    ) +
    ggplot2::theme_bw()

  if (is.factor(val$value)) {
    if (is.null(palette)) {
      p <- p + ggplot2::scale_fill_viridis_d(option = "E", name = feat)
    } else {
      p <- p +
        ggplot2::scale_fill_manual(
          values = palette,
          drop = FALSE,
          name = scale_title,
          ...
        )
    }
  } else {
    if (is.null(palette)) {
      p <- p + ggplot2::scale_fill_viridis_c(option = "E", name = feat)
    } else {
      p <- p +
        ggplot2::scale_fill_gradientn(
          colors = palette,
          name = scale_title,
          ...
        )
    }
  }

  p
}


# ors_summary <- function(src,
#                         profile = get_profiles(),
#                         instance = NULL,
#                         ...) {
#   assert_that(is_sf(src))
#   instance <- check_instance(instance)
#   iid <- get_id(instance = instance)
#
#   # Check if ORS is ready to use
#   ors_ready(force = TRUE, error = TRUE, id = iid)
#
#   # Bring input data into shape
#   src <- prepare_input(src)
#
#   profile <- match.arg(profile)
#
#   url <- get_ors_url(id = iid)
#
#   features <- list(attributes = TRUE, elevation = TRUE, extra_info = TRUE)
#   options <- format_ors_params(append(features, list(...)), profile)
#
#   res <- call_ors_directions(
#     src = src,
#     profile = profile,
#     units = "m",
#     geometry = TRUE,
#     params = options,
#     url = url,
#     token = instance$token
#   )
#
#   handle_ors_conditions(res, abort_on_error = TRUE, warn_on_warning = FALSE)
#
#   # Custom summary tables
#   geometry <- ors_multiple_linestrings(res)
#   elevation <- attr(geometry, "elevation")
#
#   distances <- calculate_distances(geometry)
#   durations <- calculate_durations(res, distances$distance)
#   speeds <- calculate_avgspeed(distances$distance, durations$duration)$avgspeed
#
#   speeds_summary <- make_summary_table(unlist(speeds), distances)
#   elevation_summary <- make_summary_table(elevation, distances)
#
#   # ORS summary tables
#   get_ors_summaries <- function(info_type) {
#     summary <- extras[[info_type]]$summary[[1L]]
#     summary$value <- fill_extra_info(summary$value, info_type, profile)
#     summary
#   }
#
#   extras <- res$features$properties$extras
#   summaries <- sapply(names(extras), get_ors_summaries, simplify = FALSE)
#
#   summaries <- sapply(summaries, function(summary) {
#     summary <- stats::aggregate(summary[, c(2L, 3L)],
#       by = summary["value"],
#       sum
#     )
#     row.names(summary) <- summary$value
#     summary$value <- NULL
#     summary
#   }, simplify = FALSE)
#
#   # Append summary tables and set units
#   elev_speeds <- list(elevation = elevation_summary, avgspeed = speeds_summary)
#   summaries <- append(summaries, elev_speeds, after = 0L)
#
#   if (requireNamespace("units")) {
#     summaries <- lapply(summaries, function(s) {
#       s["distance"] <- units::as_units(s$distance, "m")
#       s
#     })
#   }
#
#   output <- list(
#     distance = extract_ors_attribute(res, "distance"),
#     duration = extract_ors_attribute(res, "duration"),
#     detourfactor = extract_ors_attribute(res, "detourfactor"),
#     ascent = extract_ors_attribute(res, "ascent"),
#     descent = extract_ors_attribute(res, "descent"),
#     speed = summary(speeds),
#     elevation = summary(elevation),
#     tables = summaries
#   )
#
#   if (requireNamespace("units")) {
#     units(output$distance) <- "m"
#     units(output$duration) <- "s"
#   }
#
#   class(output) <- "route_summary"
#   output
# }


#' @rdname ors_inspect
#' @export
#'
#' @param object A route object of class \code{ors_inspect}.
summary.ors_route <- function(object, ...) {
  if (is.data.frame(x)) {
    x <- list(x)
  }

  lapply(seq_along(x), function(i) {
    route <- x[[i]]

  })
}
