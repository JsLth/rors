get_extract <- function(self,
                        private,
                        place,
                        provider,
                        timeout,
                        ...) {
  data_dir <- self$paths$top
  if (private$.is_type("docker")) {
    data_dir <- file.path(data_dir, "files")
  }
  verbose <- private$.verbose
  prompts <- private$.prompts
  ok <- TRUE
  i <- 0L

  if (is.null(provider)) {
    provider <- osmextract::oe_providers(quiet = TRUE)$available_providers # nocov
  }

  if ((!interactive() || !prompts) && length(provider) > 1L) {
    abort(paste( # nocov start
      "In batch or non-prompt mode, explicitly pass",
      "a single provider name to {.fun ors_extract}."
    )) # nocov end
  }

  if (length(provider) > 1) {
    ors_cli(info = list(c("i" = "Trying different extract providers...")))
  }

  # While there are providers left to try out, keep trying until
  # an extract provider is chosen
  while (ok && i < length(provider)) {
    i <- i + 1L
    place_match <- osmextract::oe_match(
      place = place,
      provider = provider[i],
      quiet = TRUE,
      ...
    )

    file_name <- basename(place_match$url)
    file_size <- round(place_match$file_size / 1024L / 1024L)

    ors_cli(
      text = "Provider : {cli::col_green(provider[i])}",
      text = "Name \u00a0\u00a0\u00a0\u00a0: {cli::col_green(file_name)}",
      text = "Size \u00a0\u00a0\u00a0\u00a0: {cli::col_green(file_size)} MB",
      cat = "line"
    )

    if (provider[i] == "bbbike") {
      ors_cli(warn = list(c("!" = paste( # nocov start
        "bbbike extracts are known to cause issues with",
        "memory allocation. Use with caution."
      )))) # nocov end
    }

    if (length(provider) > 1) {
      ok <- yes_no("Do you want to try another provider?")
    }
  }

  # If the while loop exits and the last answer given is yes, exit
  if (ok && length(provider) > 1) {
    ors_cli(info = list(c( # nocov start
      "!" = "All providers have been searched. Please download the extract manually."
    )))
    return(invisible()) # nocov end
  }

  # If a file with the same name already exists, skip the download
  file_occurences <- grepl(file_name, dir(data_dir))
  if (sum(file_occurences) == 1L) {
    ors_cli(info = list(c(
      "i" = paste("The extract already exists. Download will be skipped.")
    )))

    path <- paste(data_dir,
      dir(data_dir)[file_occurences],
      sep = "/"
    )

    rel_path <- relative_path(path, self$paths$top, pretty = TRUE)

    ors_cli(info = list(c("i" = "Download path: {rel_path}")))
    # If no file exists, remove all download a new one
  } else {
    path <- file.path(data_dir, paste0(provider[i], "_", file_name))
    rel_path <- relative_path(path, self$paths$top, pretty = TRUE)

    ors_cli(progress = list(
      "step",
      msg = "Downloading OSM extract...",
      msg_done = "The extract was successfully downloaded to the following path: {rel_path}",
      msg_failed = "Extract could not be downloaded.",
      spinner = verbose
    ))

    req <- httr2::request(place_match$url)
    req <- httr2::req_method(req, "GET")
    req <- httr2::req_timeout(req, timeout %||% getOption("timeout"))
    if (verbose) req <- httr2::req_progress(req)
    httr2::req_perform(req, path = file.path(
      data_dir, paste0("geofabrik_", basename(place_match$url))
    ))
  }

  # If the size is over 6 GB in size, give out a warning
  size <- file.size(path)
  if (size >= 5.5e+9) {
    ors_cli(info = list(c("i" = paste( # nocov start
      "The OSM extract is very large ({.field {round(size, 2)}} GB).",
      "Make sure that you have enough working memory available."
    )))) # nocov end
  }

  invisible(path)
}


set_extract <- function(self, private, file) {
  data_dir <- self$paths$top
  if (private$.is_type("docker")) {
    data_dir <- file.path(data_dir, "files")
  }
  filename <- basename(file)
  if (!identical(filename, file) || file.exists(filename)) {
    file <- normalizePath(file, "/")

    if (!file.exists(file)) {
      abort("Extract file does not exist.", class = "extract_not_found_error")
    }

    copied <- suppressWarnings(
      file.copy(file, file.path(data_dir, filename), overwrite = TRUE)
    )

    if (!copied) {
      msg <- c(
        "!" = "Extract file was not set.",
        "i" = "Is the ORS directory properly initialized?"
      )
      abort(msg,class = "extract_copy_error")
    }
  } else {
    if (!filename %in% list.files(data_dir)) {
      msg <- c(
        "!" = "Extract file does not exist in the data directory.",
        "i" = "Did you mean to pass an absolute file path?"
      )
      abort(msg, class = "extract_relative_error")
    }
  }
  file.path(data_dir, filename)
}


get_current_extract <- function(dir) {
  cur_extract <- identify_extract(dir)

  if (!is.null(cur_extract) && !file.exists(cur_extract)) {
    cli::cli_warn(c(
      "The current extract {.val {basename(cur_extract)}} could not be found.",
      "i" = "Consider mounting a different extract."
    ))
  }

  cur_extract
}


is_pbf <- function(x) {
  exts <- c(".pbf", ".osm.gz", ".osm.zip", ".osm")
  vapply(x, \(x) any(endsWith(x, exts)), FUN.VALUE = logical(1))
}
