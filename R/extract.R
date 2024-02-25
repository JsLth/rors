get_extract <- function(self, place, provider, timeout, verbose, ...) {
  data_dir <- file.path(self$paths$top, "docker/data")
  ok <- TRUE
  i <- 0L

  if (is.null(provider)) {
    providers <- osmextract::oe_providers(quiet = TRUE)$available_providers # nocov
  } else {
    providers <- provider
  }

  if ((!interactive() || verbose <= 1) && length(providers) > 1L) {
    cli::cli_abort(paste( # nocov start
      "In batch or non-verbose mode, explicitly pass",
      "a single provider name to {.fun ors_extract}."
    )) # nocov end
  }

  if (length(providers) > 1) {
    ors_cli(info = list(c("i" = "Trying different extract providers...")))
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

    ors_cli(
      text = "Provider : {cli::col_green(providers[i])}",
      text = "Name \u00a0\u00a0\u00a0\u00a0: {cli::col_green(file_name)}",
      text = "Size \u00a0\u00a0\u00a0\u00a0: {cli::col_green(file_size)} MB",
      cat = "line"
    )

    if (providers[i] == "bbbike") {
      ors_cli(warn = list(c("!" = paste( # nocov start
        "bbbike extracts are known to cause issues with",
        "memory allocation. Use with caution."
      )))) # nocov end
    }

    if (length(providers) > 1) {
      ok <- yes_no("Do you want to try another provider?")
    }
  }

  # If the while loop exits and the last answer given is yes, exit
  if (ok && length(providers) > 1) {
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
    path <- file.path(data_dir, paste0(providers[i], "_", file_name))
    rel_path <- relative_path(path, self$paths$top, pretty = TRUE)

    ors_cli(progress = list(
      "step",
      msg = "Downloading OSM extract...",
      msg_done = "The extract was successfully downloaded to the following path: {rel_path}",
      msg_failed = "Extract could not be downloaded.",
      spinner = verbose > 1
    ))

    timeout <- max(timeout, getOption("timeout"))

    proc <- callr::r_bg(
      function(place_match, providers, data_dir, timeout) {
        options(timeout = timeout)
        osmextract::oe_download(
          place_match$url,
          provider = providers[i],
          download_directory = data_dir,
          quiet = TRUE,
          # maximum of 50 GB; above that, you probably need something else
          max_file_size = 5e+10
        )
      },
      args = list(place_match, providers, data_dir, timeout),
      package = TRUE
    )

    while (proc$is_alive()) {
      ors_cli(progress = "update")
    }
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


set_extract <- function(self, file) {
  filename <- basename(file)
  data_dir <- file.path(self$paths$top, "docker", "data")
  if (!identical(filename, file) || file.exists(filename)) {
    file <- normalizePath(file, "/")

    if (!file.exists(file)) {
      cli::cli_abort("Extract file does not exist.")
    }

    copied <- suppressWarnings(
      file.copy(file, file.path(data_dir, filename), overwrite = TRUE)
    )

    if (!copied) {
      cli::cli_abort(c(
        "!" = "Extract file was not set.",
        "i" = "Is the ORS directory properly initialized?"
      ))
    }
  } else {
    if (!filename %in% list.files(data_dir)) {
      cli::cli_abort(c(
        "!" = "Extract file does not exist in the data directory.",
        "i" = "Did you mean to pass an absolute file path?"
      ))
    }
  }
  file.path(data_dir, filename)
}


get_current_extract <- function(compose, dir) {
  cur_extract <- identify_extract(compose, dir)

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
