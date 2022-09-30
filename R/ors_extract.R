#' Mount an extract to ORS
#'
#' @description Add an extract to an existing OpenRouteService instance.
#'
#' @param instance \code{[ors_instance]}
#'
#' Object created by \code{\link{ors_instance}}.
#' @param place Place description of desired OpenStreetMap extract. This
#' argument is passed to \code{\link[osmextract]{oe_match}} which will try to
#' match it with an existing extract from a provider specified in \code{provider}.
#' @param provider OpenStreetMap extract provider that is supported by
#' \code{\link[osmextract]{oe_match}}
#' @param file Path or URL to an extract to be mounted.
#' @param ... Further arguments passed to \code{\link[osmextract]{oe_match}}.
#'
#' @returns Nested list of class \code{ors_instance}.
#'
#' @family ORS setup functions
#'
#' @export
ors_extract <- function(instance, place = NULL, provider = "geofabrik", file = NULL, ...) {
  verbose <- attr(instance, "verbose")

  if (!is.null(file)) {
    assert(file, file = TRUE, len = 1L)
    extract_path <- file
  }

  if (!is.null(place)) {
    assert(place, class = "character", len = 1L)
    extract_path <- get_extract(
      place = place,
      provider = provider,
      paths = instance$paths,
      verbose = verbose,
      ...
    )
  }

  if (is.null(file) && is.null(place)) {
    extract_path <- NULL
  }

  if (!is.null(extract_path)) {
    dir <- instance$paths$dir
    graphs_dir <- file.path(dir, "docker/graphs")
    relative <- relative_path(extract_path, file.path(dir, "docker"))
    compose <- instance$compose$parsed
    if (length(dir(graphs_dir))) {
      compose <- set_graphbuilding("change", compose, relative)
    } else {
      compose <- set_graphbuilding("build", compose, relative)
    }
    write_dockercompose(compose, dir)
    instance[["compose"]] <- NULL
  }

  instance <- .instance(instance, extract_path = extract_path, verbose = verbose)

  assign("instance", instance, envir = ors_cache)
  invisible(instance)
}


get_extract <- function(place, provider, paths, verbose, ...) {
  if (!requireNamespace("osmextract")) {
    cli::cli_abort("{.pkg osmextract} required to match extracts by name.")
  }
  data_dir <- file.path(paths$dir, "docker/data")
  ok <- TRUE
  i <- 0L

  if (is.null(provider)) {
    providers <- osmextract::oe_providers(quiet = TRUE)$available_providers
  } else {
    providers <- provider
  }

  if (!interactive() && length(providers) > 1L) {
    cli::cli_abort(paste(
      "In batch mode, explicitly pass",
      "a single provider name to {.fun ors_extract}."
    ))
  }

  if (length(providers) > 1) {
    ors_cli(info = c("i" = "Trying different extract providers..."))
  }

  # While there are providers left to try out, keep trying until
  # an extract provider is chosen
  while (ok && i < length(providers)) {
    i <- i + 1L
    place_match <- osmextract::oe_match(
      place = place,
      provider = providers[i],
      quiet = TRUE,
      ...
    )

    file_name <- basename(place_match$url)
    file_size <- round(place_match$file_size / 1024L / 1024L)

    ors_cli(info = "Provider : {cli::col_green(providers[i])}")
    ors_cli(info = "Name \u00a0\u00a0\u00a0\u00a0: {cli::col_green(file_name)}")
    ors_cli(info = "Size \u00a0\u00a0\u00a0\u00a0: {cli::col_green(file_size)} MB")

    if (providers[i] == "bbbike") {
      ors_cli(warn = paste(
        "bbbike extracts are known to cause issues with",
        "memory allocation. Use with caution."
      ))
    }

    ors_cli(line = TRUE)

    if (length(providers) > 1) {
      input <- readline("Do you want to try another provider? (y/N/Cancel) ")
    } else {
      input <- "N"
    }

    # If neither yes or no is given as input, cancel the function
    if (!input %in% c("y", "N")) {
      ors_cli(info = c("x" = "Function cancelled."))
      invokeRestart("abort")
    }
    ok <- input == "y"
  }

  # If the while loop exits and the last answer given is yes, exit
  if (ok) {
    ors_cli(info = c(
      "!" = "All providers have been searched. Please download the extract manually."
    ))
    return(invisible())
  }

  # If a file with the same name already exists, skip the download
  file_occurences <- grepl(file_name, dir(data_dir))
  if (sum(file_occurences) == 1L) {
    ors_cli(info = c(
      "i" = paste(
        "The extract already exists in {.path ~/docker/data}.",
        "Download will be skipped."
      )
    ))

    path <- paste(data_dir,
      dir(data_dir)[file_occurences],
      sep = "/"
    )

    ors_cli(info = c("i" = paste(
      "Download path: {.file {relative_path(path, paths$dir, pretty = TRUE)}}"
    )))
    # If no file exists, remove all download a new one
  } else {
    path <- file.path(data_dir, paste0(providers[i], "_", file_name))

    rel_path <- relative_path(path, paths$dir, pretty = TRUE)
    ors_cli(
      progress = "step",
      msg = "Downloading OSM extract...",
      msg_done = "The extract was successfully downloaded to the following path: {.file {rel_path}}",
      msg_failed = "Extract could not be downloaded.",
      spinner = TRUE
    )

    proc <- callr::r_bg(
      function(place_match, providers, data_dir) {
        osmextract::oe_download(place_match$url,
          provider = providers[i],
          download_directory = data_dir,
          quiet = TRUE
        )
      },
      args = list(place_match, providers, data_dir),
      package = TRUE
    )

    while (proc$is_alive()) {
      ors_cli(progress = "update")
    }
  }

  # If the size is over 6 GB in size, give out a warning
  size <- file.info(path)$size / 1024L / 1024L
  if (size >= 6000L) {
    ors_cli(info = paste(c("i" =
      "The OSM extract is very large. Make sure that you have enough",
      "working memory available."
    )))
  }

  invisible(path)
}


get_current_extract <- function(obj, compose, dir) {
  if (!missing(compose) && !missing(dir)) {
    obj <- list(compose = list(parsed = compose), paths = list(dir = dir))
  }

  current_extract <- identify_extract(obj)

  if (!is.null(current_extract) && !file.exists(current_extract)) {
    pretty_path <- relative_path(current_extract, obj$paths$dir, pretty = TRUE)
    cli::cli_warn(c(
      "The current extract {.path {pretty_path}} could not be found.",
      "Consider mounting a different extract."
    ))
  }

  current_extract
}


validate_extract <- function(extract_path) {
  if (!is.null(extract_path) && file.exists(extract_path)) {
    extract_path
  }
}
