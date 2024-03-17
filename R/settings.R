write_dockercompose <- function(compose, file = NULL) {
  java_opts <- compose$services$`ors-app`$environment[2L]
  catalina_opts <- compose$services$`ors-app`$environment[3L]
  user <- compose$services$`ors-app`$user

  # Put some but not all strings in double quotes
  compose$services$`ors-app`$environment <- as.list(
    compose$services$`ors-app`$environment
  )
  attr(compose$services$`ors-app`$ports, "quoted") <- TRUE
  attr(compose$services$`ors-app`$environment[[2]], "quoted") <- TRUE
  attr(compose$services$`ors-app`$environment[[3]], "quoted") <- TRUE
  attr(compose$services$`ors-app`$user, "quoted") <- TRUE

  # Build yaml with indented bullet points
  yml <- yaml::as.yaml(compose, indent.mapping.sequence = TRUE)

  # Remove line breaks of long strings
  yml <- gsub("\\n\\s{7,}-", " -", yml)

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


read_memory <- function(compose, num, gb = TRUE) {
  java_options <- compose$services$`ors-app`$environment[2L]
  java_mem <- java_mem_chr <- utils::tail(unlist(strsplit(java_options, " ")), 2L)

  if (num) {
    java_mem <- as.numeric(gsub(".*?([0-9]+).*", "\\1", java_mem_chr))

    java_mem <- lapply(c(1, 2), function(mi) {
      if (gb && grepl("[0-9]m", java_mem_chr[mi])) {
        java_mem[mi] / 1000L
      } else if (!gb && grepl("[0-9]g", java_mem_chr[mi])) {
        java_mem[mi] * 1000L
      } else {
        java_mem[mi]
      }
    })
  }

  names(java_mem) <- c("init", "max")

  java_mem
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
  init <- memory[1]
  max <- memory[2]

  java_options <- compose$services$`ors-app`$environment[2L]
  java_mem <- read_memory(compose, num = FALSE)

  init_mem_allocation <- java_mem[1L]
  max_mem_allocation <- java_mem[2L]

  java_options <- gsub(
    init_mem_allocation,
    sprintf("-Xms%sm", format(init, scientific = FALSE)),
    java_options
  )

  java_options <- gsub(
    max_mem_allocation,
    sprintf("-Xmx%sm", format(max, scientific = FALSE)),
    java_options
  )

  compose$services$`ors-app`$environment[2] <- java_options
  compose$services$`ors-app`$environment[2]
}


set_compose_image <- function(compose, image) {
  compose$services$`ors-app`$image <-
    gsub(":.+$", paste0(":", image), compose$services$`ors-app`$image)
  compose
}


read_compose_image <- function(compose) {
  regex_match(compose$services$`ors-app`$image, ":(.+)$")[[1]][2]
}


random_ors_name <- function(private, name) {
  hash <- substr(private$.get_hash(), 1, 7)
  paste0("ors-app-", hash)
}


graphbuilding_enabled <- function(compose) {
  gp <- compose$services$`ors-app`$environment[1L]
  as.logical(strsplit(gp, "=")[[1]][2])
}


set_gp <- function(self, mode) {
  compose <- self$compose$parsed

  if (isTRUE(mode)) {
    paths <- self$paths

    # Enable graph building
    compose <- force_gp(TRUE, compose)

    # Set extract volume to change extract in a built container
    extract_path <- relative_path(paths$extract, paths$top)
    change_node <- sprintf("./%s:/home/ors/ors-core/data/osm_file.pbf", extract_path)
    compose$services$`ors-app`$volumes[6L] <- change_node
  } else {
    # Disable graph building
    compose <- force_gp(FALSE, compose)

    # Remove extract volume
    if (!is.na(compose$services$`ors-app`$volumes[6L])) {
      compose$services$`ors-app`$volumes <- compose$services$`ors-app`$volumes[-6L]
    }
  }
  compose$services$`ors-app`$environment[1L]
}


force_gp <- function(mode, compose) {
  mode <- capitalize_char(mode)
  build_graphs_string <- sprintf("BUILD_GRAPHS=%s", mode)
  compose$services$`ors-app`$environment[1L] <- build_graphs_string
  compose
}


change_extract <- function(compose, path) {
  volumes <- compose$services$`ors-app`$volumes
  do_rm <- is.null(path)

  if (!do_rm) {
    change_node <- sprintf(
      "./docker/data/%s:/home/ors/ors-core/data/osm_file.pbf",
      basename(path)
    )
  } else {
    # remove extract node if path is null
    change_node <- NULL
  }

  # is there already a pbf volume?
  pbf_vol <- is_pbf(compose$services$`ors-app`$volumes)

  if (any(pbf_vol)) {
    # if yes, replace it
    idx <- pbf_vol
  } else if (!do_rm) {
    # otherwise, add a new volume
    idx <- length(volumes) + 1
  } else {
    idx <- NULL
  }

  if (!do_rm && !is.null(idx)) {
    compose$services$`ors-app`$volumes[idx] <- change_node
  } else {
    compose$services$`ors-app`$volumes <- compose$services$`ors-app`$volumes[!idx]
  }

  compose
}
