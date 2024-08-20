skip_if_docker_unavailable <- function() {
  skip_if_not(docker_installed() && has_docker_access(), "docker unavailable")
}

is_mock_test <- function() {
  !nzchar(Sys.getenv("REAL_REQUESTS"))
}

#' Start a local ORS instance using `ors_instance`.
#'
#' @param ... Arguments to ors_instance
#' @param complete If dry = TRUE, whether to start a complete instance
#' including extract and config
#' @noRd
local_ors_instance <- function(dir = tempdir(),
                               ...,
                               dry = TRUE,
                               complete = FALSE,
                               .local_envir = parent.frame()) {
  ors <- ors_instance(dir = dir, dry = dry, ...)
  ors$set_port()

  if (isTRUE(dry) && complete) {
    create_dry_files(ors)
    ors$update("self")
  }

  if (inherits(ors, "ORSDocker")) {
    ors$set_name(paste0("test-rors-", cli::hash_obj_md5(sample(1:1000, 1))))
  }

  withr::defer(ors$purge(), envir = .local_envir)
  invisible(ors)
}

with_ors_instance <- function(code,
                              dir = tempdir(),
                              ...,
                              verbose = TRUE,
                              dry = TRUE,
                              complete = FALSE,
                              .local_envir = parent.frame()) {
  ors <- ors_instance(
    dir = dir,
    dry = dry,
    verbose = verbose,
    prompts = FALSE,
    ...
  )

  if (isTRUE(dry) && complete) {
    create_dry_files(ors)
    ors$update("self")
  }

  if (inherits(ors, "ORSDocker")) {
    ors$set_name(paste0("test-rors-", cli::hash_obj_md5(sample(1:1000, 1))))
  }

  env <- new.env(parent = .local_envir)
  assign("ors", ors, envir = env)

  withr::defer(capture.output(ors$purge(), type = "message"), envir = env)
  res <- force(eval(substitute(code), envir = env))
  withr::deferred_run(envir = env)
  res
}

create_dry_files <- function(ors) {
  data_dir <- file.path(ors$paths$top, "docker", "data")
  if (dir.exists(data_dir)) stop("Data dir somehow already exists")
  dir.create(data_dir, recursive = TRUE)
  ors$set_extract(file = test_pbf(), do_use = FALSE)

  conf_dir <- file.path(ors$paths$top, "docker", "conf")
  if (dir.exists(conf_dir)) stop("Conf dir somehow already exists")
  dir.create(conf_dir, recursive = TRUE)
  yaml::write_yaml(
    list(
      ors = list(
        engine = list(
          source_file = "ors-api/src/test/files/heidelberg.osm.gz",
          profiles = list(car = list(enabled = TRUE))
        )
      )
    ),
    file = file.path(conf_dir, "ors-config.yml")
  )
}


on_os <- function(os) {
  os <- switch(
    os,
    windows = "windows",
    mac = "darwin",
    linux = "linux",
    solaris = "sunos"
  )
  identical(tolower(Sys.info()[["sysname"]]), os)
}


has_valid_java <- function() {
  !inherits(try(check_jdk_version(FALSE)), "try-error")
}


test_pbf <- function() {
  system.file("setup/monaco.pbf", package = "rors")
}


test_coords <- function(idx = 1:6) {
  sf::st_sfc(
    sf::st_point(c(7.41879016120929, 43.73832285129219)),
    sf::st_point(c(7.424013297449404, 43.7325200234902)),
    sf::st_point(c(7.424303740722736, 43.74071117584624)),
    sf::st_point(c(7.412191107004145, 43.73130094784932)),
    sf::st_point(c(7.417231545623077, 43.734492595419866)),
    sf::st_point(c(7.429963726750094, 43.740043918669045))
  )[idx]
}
