#' Mount an extract to ORS
#' 
#' @description Add an extract to an existing OpenRouteService instance.
#' 
#' @param instance ORS instance object created by
#' \code{\link[ORSRouting]{ors_instance}}.
#' @param place Place description of desired OpenStreetMap extract. This
#' argument is passed to \code{\link[osmextract]{oe_match}} which will try to
#' match it with an existing extract from a provider specified in \code{provider}.
#' @param provider OpenStreetMap extract provider that is supported by
#' \code{\link[osmextract]{oe_match}}
#' @param file Path or URL to an extract to be mounted.
#' @param ... Further arguments passed to \code{\link[osmextract]{oe_match}}.
#' 
#' @returns Nested list of class \code{ors_constructor}.
#' 
#' @family ORS setup functions
#' 
#' @export
ors_extract <- function(instance, place = NULL, provider = "geofabrik", file = NULL, ...) {
  if (is.null(place) && is.null(file)) {
    extract_path <- get_current_extract()
  }
  
  if (!is.null(file) && file.exists(file)) {
    extract_path <- file
  }
  
  if (!is.null(place)) {
    extract_path <- get_extract(place, provider, instance$paths, ...)
  }
  
  relative_extract_path <- relativePath(extract_path, instance$paths$dir)
  if (isTRUE(attr(instance, "built"))) {
    set_graphbuilding("change", instance$compose$parsed, relative_extract_path)
  } else {
    
    set_graphbuilding("build", instance$compose$parsed, relative_extract_path)
  }

  instance <- .instance(instance, extract_path = extract_path)
  
  assign("instance", instance, envir = ors_cache)
  invisible(instance)
}


get_extract <- function(place, provider, paths, ...) {
  if (!requireNamespace("osmextract")) {
    cli::cli_abort("{.pkg osmextract} required to match extracts by name.")
  }
  data_dir <- file.path(paths$dir, "docker/data")
  ok <- TRUE
  i <- 0L
  
  if (is.null(provider)) {
    providers <- suppressMessages({
      osmextract::oe_providers()$available_providers
    })
  } else {
    providers <- provider
  }
  
  if (!interactive() && length(providers) > 1L) {
    cli::cli_abort(paste("In batch mode, explicitly pass",
                         "a single provider name."))
  }
  
  if (length(providers) > 1) cli::cli_alert_info("Trying different extract providers...")
  
  # While there are providers left to try out, keep trying until
  # an extract provider is chosen
  while (ok && i < length(providers)) {
    i <- i + 1L
    place_match <- osmextract::oe_match(place = place,
                                        provider = providers[i],
                                        quiet = TRUE,
                                        ...)
    
    file_name <- basename(place_match$url)
    file_size <- round(place_match$file_size / 1024L / 1024L)
    
    cli::cli_text("Provider : {cli::col_green(providers[i])}")
    cli::cli_text("Name \u00a0\u00a0\u00a0\u00a0: {cli::col_green(file_name)}")
    cli::cli_text("Size \u00a0\u00a0\u00a0\u00a0: {cli::col_green(file_size)} MB")
    
    if (providers[i] == "bbbike") {
      cli::cli_alert_warning(paste(
        "bbbike extracts are known to cause issues with memory allocation. Use with caution."
      ))
    }
    
    cli::cat_line()
    
    if (length(providers) > 1) {
      input <- readline("Do you want to try another provider? (y/N/Cancel) ")
    } else {
      input <- "N"
    }
    
    # If neither yes or no is given as input, cancel the function
    if (!input %in% c("y", "N")) {
      cli::cli_alert_danger("Function cancelled.")
      invokeRestart("abort")
    }
    ok <- input == "y"
  }
  
  # If the while loop exits and the last answer given is yes, exit
  if (ok) {
    cli::cli_alert_warning("All providers have been searched. Please download the extract manually.")
    return(invisible())
  }
  
  # If a file with the same name already exists, skip the download
  file_occurences <- grepl(file_name, dir(data_dir))
  if (sum(file_occurences) == 1L) {
    cli::cli_alert_info(
      "The extract already exists in {.path ~/docker/data}. Download will be skipped."
    )
    
    path <- paste(data_dir,
                  dir(data_dir)[file_occurences],
                  sep = "/")
    
    cli::cli_text("Download path: {.file {relativePath(path, paths$dir, pretty = TRUE)}}")
    
    # If no file exists, remove all download a new one
  } else {
    path <- file.path(data_dir, paste0(providers[i], "_", file_name))
    
    if (interactive()) {
      rel_path <- relativePath(path, paths$dir, pretty = TRUE)
      cli::cli_progress_step(
        "Downloading OSM extract...",
        msg_done = "The extract was successfully downloaded to the following path: {.file {rel_path}}",
        msg_failed = "Extract could not be downloaded.",
        spinner = TRUE
      )
    }
    
    proc <- callr::r_bg(
      function(place_match, providers, data_dir) {
        osmextract::oe_download(place_match$url,
                                provider = providers[i],
                                download_directory = data_dir,
                                quiet = TRUE)
      },
      args = list(place_match, providers, data_dir),
      package = TRUE
    )
    
    while(proc$is_alive()) {
      if (interactive()) cli::cli_progress_update()
    }
    
    if (interactive()) cli::cli_progress_done()
  }
  
  # If the size is over 6 GB in size, give out a warning
  size <- file.info(path)$size / 1024L / 1024L
  if (size >= 6000L) {
    cli::cli_alert_warning(paste("The OSM extract is very large.",
                                 "Make sure that you have enough",
                                 "working memory available."))
  }

  invisible(path)
}


get_current_extract <- function(obj, compose, dir) {
  if (!missing(compose) && !missing(dir)) {
    obj <- list(compose = list(parsed = compose), paths = list(dir = dir))
  }

  build <- obj$compose$parsed$services$`ors-app`$build
  if (is.null(build)) {
    return()
  }

  current_extract <- build$args$OSM_FILE
  current_extract <- gsub("./", "", current_extract, fixed = TRUE)
  current_extract <- file.path(obj$paths$dir, current_extract)
  
  if (!file.exists(current_extract)) {
    pretty_path <- relativePath(current_extract, obj$paths$dir, pretty = TRUE)
    cli::cli_warn(c(
      "The current extract {.path {pretty_path}} could not be found.",
      "Consider mounting a different extract."
    ))
  }
  
  current_extract
}
