env_var_is_true <- function(x) {
  isTRUE(as.logical(Sys.getenv(x, "false")))
}

on_ci <- function() {
  env_var_is_true("CI")
}

on_cran <- function() {
  !interactive() && !env_var_is_true("NOT_CRAN")
}

on_real_tester <- function() {
  env_var_is_true("REAL_REQUESTS")
}

skip_if_docker_unavailable <- function() {
  skip_if_not(docker_running(), "docker unavailable")
}

is_mock_test <- function() {
  (on_ci() || on_cran() || !docker_running() || !has_valid_java()) &&
    !on_real_tester()
}

#' Start a local ORS instance using `ors_instance`.
#'
#' @param ... Arguments to ors_instance
#' @param complete If dry = TRUE, whether to start a complete instance
#' including extract and config
#' @noRd
local_ors_instance <- function(dir = tempdir(),
                               ...,
                               version = "8.0.0",
                               verbose = TRUE,
                               dry = TRUE,
                               .local_envir = parent.frame()) {
  ors <- ors_instance(
    dir = dir,
    dry = dry,
    version = version,
    verbose = verbose,
    prompts = FALSE,
    ...
  )

  if (!dry) make_test_ors(ors)
  withr::defer(ors$purge(), envir = .local_envir)
  invisible(ors)
}

with_ors_instance <- function(code,
                              dir = tempdir(),
                              ...,
                              version = "8.0.0",
                              verbose = TRUE,
                              dry = TRUE,
                              .local_envir = parent.frame()) {
  ors <- ors_instance(
    dir = dir,
    dry = dry,
    version = version,
    verbose = verbose,
    prompts = FALSE,
    ...
  )

  if (!dry) make_test_ors(ors)
  env <- new.env(parent = .local_envir)
  assign("ors", ors, envir = env)
  withr::defer(capture.output(ors$purge(), type = "message"), envir = env)
  res <- force(eval(substitute(code), envir = env))
  withr::deferred_run(envir = env)
  res
}


make_test_ors <- function(ors) {
  ors$set_port()
  ors$set_extract(file = test_pbf())
  ors$set_endpoints(isochrones = list(maximum_intervals = 2, maximum_locations = 3))
  if (inherits(ors, "ORSDocker")) {
    ors$set_name(paste0("test-rors-", cli::hash_obj_md5(sample(1:1000, 1))))
  }
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


rors_cleanup <- function() {
  ors_cache <- get0("ors_cache", envir = asNamespace("rors"))
  rm(list = ls(envir = ors_cache), envir = ors_cache)
}
