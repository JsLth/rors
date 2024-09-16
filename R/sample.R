#' ORS-based sampling
#' @description Sample point geometries within the boundaries of the running
#' ORS extract. This function works by reading in the mounted extract file and
#' can thus only be run with a local instance.
#'
#' @param size \code{[integer]}
#'
#' Number of points to be sampled.
#' @param ... Passed to \code{\link[sf]{st_sample}}.
#' @param force \code{[logical]}
#'
#' If \code{TRUE}, forces the cached extract geometries to be overwritten.
#' Defaults to \code{FALSE} to increase speed. Set this to \code{TRUE} if the
#' extract has changed and a new one needs to be loaded.
#' @param poly \code{[sf/sfc]}
#'
#' Boundary polygon used to sample points. If \code{NULL}, the default, boundary
#' polygons are extracted from the current instance as set by
#' \code{\link{ors_instance}}.
#' @inheritParams get_extract_boundaries
#'
#' @returns \code{ors_sample} returns an \code{sfc} object containing the
#' sampled \code{POINT} geometries. \code{get_extract_boundaries} returns an
#' \code{sfc} object of the currently mounted extract boundaries.
#'
#' @details The function vectortranslates the extract file to an \code{sf} object.
#' Depending on the file, this can take a while. The unionized boundaries are
#' then cached making subsequent function calls a lot faster.
#'
#' If \code{instance} is not local, it is more difficult to derive the
#' extract boundaries. There is thus far no way of accessing an OSM extract file
#' knowing only the server address. We can, however, make use of some
#' heuristics: \code{\link{ors_export}} can export the built graphs from an ORS
#' server if it allows it. However, it does not work on the public API and it
#' requires knowledge about the approximate area of an extract.
#' \code{\link{ors_guess}} can make an approximation of an extract area. It
#' accesses the snap endpoint which also does not work on the public API and
#' needs to be enabled on other servers. \code{ors_guess} can make a lot of
#' requests and might not be feasible in many situations.
#'
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # ORS sampling only works with a local instance
#' monaco <- system.file("setup/monaco.pbf", package = "rors")
#' ors <- ors_instance()
#' ors$set_extract(file = monaco)
#' ors$up()
#'
#' # reads in boundaries of monaco pbf file
#' bounds <- get_extract_boundaries()
#' plot(bounds)
#'
#' # subsequent calls do not need to read the pbf file
#' ors_sample(5)
#'
#' # ... except when they are forced to
#' ors_sample(force = TRUE)
#' }
ors_sample <- function(size,
                       ...,
                       force = FALSE,
                       instance = NULL,
                       poly = NULL,
                       verbose = TRUE) {
  assert_that(
    assertthat::is.count(size),
    is_true_or_false(force),
    is_true_or_false(verbose)
  )

  poly <- poly %||% get_extract_boundaries(instance, force, verbose)
  sf::st_sample(poly, size, ...)
}


#' @rdname ors_sample
#' @export
get_extract_boundaries <- function(instance = NULL,
                                   force = FALSE,
                                   verbose = TRUE) {
  recover_from_cache("extract_boundaries", force = force)
  instance <- instance %||% get_instance()

  if (ors_is_local(instance)) {
    extract_path <- identify_extract(instance$paths$top)
    if (is.null(extract_path)) {
      abort(
        "Cannot identify current extract file.",
        class = "extract_not_found_error"
      )
    }

    ors_cli(progress = list(
      "step",
      msg = "Reading and processing extract file...",
      msg_done = "Extract file successfully read in!",
      msg_failed = "Extract file could either not be read or not converted.",
      spinner = TRUE
    ))

    proc <- callr::r_bg(admin_from_osm, args = list(extract_path))

    while (proc$is_alive()) {
      ors_cli(progress = "update")
    }

    ors_cli(progress = c(
      progress = "done",
      result = ifelse(nzchar(proc$read_error()), "failed", "done")
    ))

    poly <- proc$get_result()
  } else {
    tip <- paste(
      "{.code get_extract_boundaries} is not usable for unkown remote",
      "servers as the extract boundaries cannot easily be determined.",
      "Consider using {.fn ors_guess}."
    )
    abort(
      c("Cannot get extract from a server URL.", "i" = tip),
      class = "remote_sample_error"
    )
  }

  assign("extract_boundaries", poly, envir = ors_cache)
  poly
}


admin_from_osm <- function(path) {
  sql <- paste(
    "SELECT geometry FROM \"multipolygons\"",
    "WHERE boundary = \"administrative\"",
    "AND admin_level IS NOT NULL"
  )
  admin <- suppressWarnings(
    osmextract::oe_read(
      path,
      layer = "multipolygons",
      query = sql,
      quiet = TRUE
    )
  )
  # handle weird OSM files
  admin <- admin[sf::st_is_valid(admin), ]

  # extract only outer boundaries
  sf::st_union(sf::st_geometry(admin))
}
