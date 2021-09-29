# Title     : Functions to get infos from ORS
# Objective : Get container or local server info such as ports, status or
#             extract location
# Created by: Jonas Lieth
# Created on: 29.09.2021


get_container_info <- function() {
  container_info <- system(
    ensure_permission(
      "docker container inspect ors-app"
    ),
    intern = TRUE
  ) %>%
    paste0(collapse = "\n") %>%
    jsonlite::fromJSON()

  if (length(container_info) == 0) {
    cli::cli_abort("No such container: ors-app")
  }
  return(container_info)
}


get_ors_info <- function(only_profiles = NA) {
  url <- sprintf("http://localhost:%s/ors/status", get_ors_port())
  ors_info <- httr::GET(url) %>%
    httr::content(as = "text", type = "application/json", encoding = "UTF-8")
  ors_info <- jsonlite::fromJSON(ors_info)
  if (isTRUE(only_profiles)) {
    profiles <- sapply(ors_info$profiles, function(x) x$profiles)
    return(unname(profiles))
  } else {
    return(ors_info)
  }

}


identify_extract <- function() {
  # Read docker container info using a system call
  container_info <- get_container_info()

  # Save docker working directory
  mdir <- container_info$Config$Labels$com.docker.compose.project.working_dir

  # Read extract file location from compose file
  compose <- yaml::yaml.load_file(paste0(mdir, "/docker-compose.yml"))
  extract_path <- compose$services$`ors-app`$build$args$OSM_FILE

  # Check if build argument is set
  if (is.null(extract_path)) {
    extract_path <- compose$services$`ors-app`$volumes[6]

    # If not, check if change volume is set
    if (is.na(change_extract)) {
      osm_file_occurences <- dir("docker/data") %>%
        grepl(".pbf|.osm.gz|.osm.zip|.osm", .)

      # As a last resort, check if we can just pick it up from the data folder
      if (sum(osm_file_occurences) == 1) {
        extract_path <- dir(paste0(mdir, "/data")[osm_file_occurences]
        )
      } else {
        cli::cli_abort(
          paste(
            "Cannot identify current extract file. Pass it explicitly."
          )
        )
      }
    }
  }
  # Convert relative to absolute path
  extract_path <- basename(extract_path) %>%
    paste(mdir, "data", ., sep = "/") %>%
    normalizePath(winslash = "/")
  return(extract_path)
}


get_ors_port <- function() {
  container_info <- get_container_info()
  port <- container_info$NetworkSettings$Ports[[1]][[1]]$HostPort[1]
  return(port)
}