#' ORS WAR instance
#' @description
#' Creates a new ORS instance object that operates on a WAR instance of
#' OpenRouteservice, i.e. an instance that is started using a .war file.
#' This R6 class is typically constructed by \code{\link{ors_instance}}.
#' \code{ORSJar} requires Tomcat >= 10 to be installed on the system.
#' Tomcat requires a Java Runtime Environment (JRE) to run. Additionally,
#' both \code{JAVA_HOME} and \code{CATALINA_HOME} environment variables must
#' be set to start Tomcat. If these requirements cannot be met, consider using
#' \code{\link{ORSDocker}} or \code{\link{ORSJar}}.
#'
#' For technical details on the setup of local ORS instances, refer to the
#' \href{https://giscience.github.io/openrouteservice/run-instance/running-war}{Running WAR documentation}.
#' For details on how to use \code{ORSDocker} objects, refer to the installation
#' vignette:
#'
#' \preformatted{
#' vignette("ors-installation", package = "rors")
#' }
#' @export
ORSWar <- R6::R6Class(
  classname = "ORSWar",
  inherit = ORSLocal,

  public = list(
    initialize = function(dir,
                          version = "8.0.0",
                          overwrite = FALSE,
                          verbose = TRUE,
                          ...) {
      check_tomcat(verbose)
      get_ors_release(dir, version, file = "war", overwrite, verbose)

      private$.dir <- dir
      private$.version <- version
      private$.overwrite <- overwrite
      private$.verbose <- verbose

      invisible(self)
    }
  )
)


check_tomcat <- function(path, verbose) {
  catalina_home <- Sys.getenv("CATALINA_HOME")
  java_home <- Sys.getenv("JAVA_HOME")

  if (!nzchar(catalina_home) || !dir.exists(catalina_home)) {
    cli::cli_abort(c(
      "!" = "Environment variable {.var CATALINA_HOME} is not defined correctly.",
      "i" = "Point the environment variable to an existing Tomcat installation."
    ))
  }

  if (!nzchar(java_home) || !dir.exists(java_home)) {
    cli::cli_abort(c(
      "!" = "Environment variable {.var JAVA_HOME} is not defined correctly.",
      "i" = "Point the environment variable to an existing JRE installation."
    ))
  }

  bin <- ifelse(is_windows(), "bin/catalina.bat", "bin/catalina")
  bin <- file.path(catalina_home, bin)
  version <- callr::run(bin, "version", error_on_status = FALSE)

  envvar_error <- grepl("not defined correctly|is defined", version$stdout)
  if (!identical(version$status, 0L) || envvar_error) {
    err <- ifelse(envvar_error, "stdout", "stderr")
    err <- gsub("\n", "\f", version[[err]])
    cli::cli_abort(c("Tomcat failed with the following error:", err))
  }

  ors_cli(cat = version$stdout)
  invisible(NULL)
}
