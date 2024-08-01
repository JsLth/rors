write_dockercompose <- function(compose, file = NULL) {
  # Put some but not all strings in double quotes
  attr(compose$services$`ors-app`$ports, "quoted") <- TRUE

  if (!is.null(compose$services$`ors-app`$environment$ADDITIONAL_JAVA_OPTS))
    attr(compose$services$`ors-app`$environment$ADDITIONAL_JAVA_OPTS, "quoted") <- TRUE

  if (!is.null(compose$services$`ors-app`$user))
    attr(compose$services$`ors-app`$user, "quoted") <- TRUE

  if (length(compose$services$`ors-app`$volumes))
    compose$services$`ors-app`$volumes <- as.list(
      compose$services$`ors-app`$volumes
    )

  # Build yaml with indented bullet points
  yml <- yaml::as.yaml(
    compose,
    indent.mapping.sequence = TRUE,
    handlers = yaml_handlers()
  )

  # Write new yaml to old yaml file
  if (!is.null(file)) {
    cat(yml, file = file)
  } else {
    cat(yml)
  }
}


ports_to_df <- function(ports) {
  ports <- strsplit(ports, ":")
  ports <- do.call(rbind.data.frame, ports)
  names(ports) <- c("host", "docker")
  row.names(ports) <- c("api", "log")
  ports
}


format_ports <- function(self, port) {
  assert_that(length(port) <= 2)
  compose <- self$compose$parsed
  ports_chr <- compose$services$`ors-app`$ports
  old_ports <- ports_to_df(ports_chr)

  ports_chr[1] <- sprintf("%s:%s", port[1] %||% old_ports[1, 1], old_ports[1, 2])
  ports_chr[2] <- sprintf("%s:%s", port[2] %NA% old_ports[2, 1], old_ports[2, 2])
  ports_chr
}


random_port <- function(n = 1) {
  if (loadable("httpuv")) {
    replicate(n, httpuv::randomPort())
  } else {
    cli::cli_abort(
      "To assign a random port, install the {.pkg httpuv} package.",
      class = "ors_loadable_error"
    )
  }
}


read_memory <- function(compose) {
  init <- compose$services$`ors-app`$environment$XMS
  max <- compose$services$`ors-app`$environment$XMX

  list(
    init = xmx_to_numeric(init),
    max = xmx_to_numeric(max)
  )
}


xmx_to_numeric <- function(mem) {
  suffix <- substr(mem, nchar(mem), nchar(mem))
  number <- as.numeric(regex_match(mem, "[0-9]+")[[1]])
  mult <- switch(
    tolower(suffix),
    k = 0.000001,
    m = 0.001,
    g = 1,
    NA
  )
  number * mult
}


adjust_memory <- function(self, private, init, max) {
  verbose <- private$.verbose

  if (!is.null(init) && !is.null(max)) {
    init <- init * 1000
    max <- max * 1000
  } else if (!is.null(init)) {
    init <- init * 1000
    max <- init
  } else if (!is.null(max)) {
    max <- max * 1000
    init <- max / 2
  } else {
    if (length(self$extract) && length(self$config)) {
      size <- self$extract$size
      no_prof <- length(self$config$profiles)
      max <- max(ceiling(size * 2.5 * no_prof), 100)
      init <- max(ceiling(max / 2L), 50)
    } else {
      ors_cli(
        warn = list(c(
          "!" = "Memory allocation options were not changed.",
          "i" = paste(
            "Memory estimation is based on extract size and number",
            "of profiles. Either pass memory specifications explicitly",
            "or add extract and configuration first"
          )
        ))
      )
      return(NULL)
    }
  }

  if (self$compose$memory$total * 0.8 - max / 1024 <= 0) {
    msg <- paste(
      "You are allocating more than your available memory.",
      "Consider lowering the allocated RAM."
    )

    if (max >= 1e+05) {
      msg <- c("!" = msg, "i" = "Did you accidentally pass MB instead of GB?")
    }

    ors_cli(warn = list(msg))
  }

  c(ceiling(init), ceiling(max))
}


format_memory <- function(self, memory) {
  compose <- self$compose$parsed
  memory <- paste0(memory, "m")
  compose$services$`ors-app`$environment$XMS <- memory[1]
  compose$services$`ors-app`$environment$XMX <- memory[2]
  compose$services$`ors-app`$environment
}


set_compose_image <- function(compose, image) {
  compose$services$`ors-app`$image <- sprintf(
    "openrouteservice/openrouteservice:%s", image
  )
  compose
}


read_compose_image <- function(compose) {
  regex_match(compose$services$`ors-app`$image, ":(.+)$")[[1]][2]
}


random_ors_name <- function(private, name) {
  hash <- substr(private$.get_hash(), 1, 7)
  paste0("ors-app-", hash)
}


set_gp <- function(self, mode) {
  compose <- self$compose$parsed
  compose$services$`ors-app`$environment$REBUILD_GRAPHS <- mode
  compose$services$`ors-app`$environment[1L]
}


check_version <- function(version) {
  if (startsWith(version, "v"))
    version <- substr(version, 2, nchar(version))

  if (is_numver(version))
    paste0("v", version)
  else if (is_version_desc(version, "dh"))
    version
}


modify_files_volume <- function(compose, path) {
  volumes <- compose$services$`ors-app`$volumes

  # is there already a files volume?
  files_vol <- endsWith(volumes, "files")

  if (any(files_vol)) {
    # if yes, replace it
    idx <- files_vol
  } else {
    # otherwise, add a new volume
    idx <- length(volumes) + 1
  }

  if (!is.null(path)) {
    # if a path is given, use it
    if (endsWith(path, "ors-docker/files"))
      path <- "files"
    change_node <- paste0(path, ":/home/ors/files")
    compose$services$`ors-app`$volumes[idx] <- change_node
  } else {
    # otherwise, remove the files volume
    compose$services$`ors-app`$volumes <- volumes[!idx]
  }

  compose
}


configure_volumes <- function(compose) {
  compose$volumes <- list(
    graphs = NULL,
    elevation_cache = NULL,
    config = NULL,
    logs = NULL,
    files = NULL
  )

  vpaths <- compose$services$`ors-app`$volumes
  vpaths <- vpaths[!endsWith(vpaths, "/home/ors")]
  for (vname in names(compose$volumes)) {
    if (!any(endsWith(vpaths, vname))) {
      vpaths[length(vpaths) + 1] <-
        sprintf("%s:/home/ors/%s", vname, vname)
    }
  }

  compose$services$`ors-app`$volumes <- vpaths
  compose
}


volume_path <- function(path, top) {
  if (startsWith(path, top)) {
    repl <- ifelse(endsWith(top, "/"), "./", ".")
    gsub(top, repl, path, fixed = TRUE)
  } else {
    path
  }
}

