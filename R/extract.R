get_extract <- function(self, place, provider, timeout, verbose, ...) {
  data_dir <- file.path(self$paths$top, "docker/data")
  ok <- TRUE
  i <- 0L

  if (is.null(provider)) {
    providers <- osmextract::oe_providers(quiet = TRUE)$available_providers
  } else {
    providers <- provider
  }

  if ((!interactive() || verbose <= 1) && length(providers) > 1L) {
    cli::cli_abort(paste(
      "In batch or non-verbose mode, explicitly pass",
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
      ok <- yes_no("Do you want to try another provider?")
    }
  }

  # If the while loop exits and the last answer given is yes, exit
  if (ok && length(providers) > 1) {
    ors_cli(info = c(
      "!" = "All providers have been searched. Please download the extract manually."
    ))
    return(invisible())
  }

  # If a file with the same name already exists, skip the download
  file_occurences <- grepl(file_name, dir(data_dir))
  if (sum(file_occurences) == 1L) {
    ors_cli(info = c(
      "i" = paste("The extract already exists. Download will be skipped.")
    ))

    path <- paste(data_dir,
      dir(data_dir)[file_occurences],
      sep = "/"
    )

    rel_path <- relative_path(path, self$paths$top)

    ors_cli(info = c("i" = paste("Download path: {rel_path}")))
    # If no file exists, remove all download a new one
  } else {
    path <- file.path(data_dir, paste0(providers[i], "_", file_name))
    rel_path <- relative_path(path, self$paths$top)
    ors_cli(
      progress = "step",
      msg = "Downloading OSM extract...",
      msg_done = "The extract was successfully downloaded to the following path: {.href [{rel_path}](file://{path})}",
      msg_failed = "Extract could not be downloaded.",
      spinner = TRUE
    )

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
  size <- file.info(path)$size / 1024L / 1024L
  if (size >= 6000L) {
    ors_cli(info = paste(c("i" =
      "The OSM extract is very large. Make sure that you have enough",
      "working memory available."
    )))
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
  current_extract <- identify_extract(compose, dir)

  if (!is.null(current_extract) && !file.exists(current_extract)) {
    pretty_path <- relative_path(current_extract, dir)
    cli::cli_warn(c(
      "The current extract {.href [{pretty_path}]({current_extract})} could not be found.",
      "Consider mounting a different extract."
    ))
  }

  current_extract
}


validate_extract <- function(extract_path) {
  if (!is.null(extract_path) &&
      file.exists(extract_path) &&
      is_pbf(extract_path)) {
    extract_path
  }
}


is_pbf <- function(x) {
  exts <- c(".pbf", ".osm.gz", ".osm.zip", ".osm")
  vapply(x, \(x) any(endsWith(x, exts)), FUN.VALUE = logical(1))
}
