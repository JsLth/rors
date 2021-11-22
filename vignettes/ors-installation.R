## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, echo = FALSE------------------------------------------------------
library(ORSRouting)
library(magrittr)


## ----init-topclass------------------------------------------------------------
ors <- ORSInstance$new()
ors


## ----init-setup, eval = FALSE-------------------------------------------------
## ors$init_setup(
##   profiles = c("car", "walking"),
##   extract_path = "osm_file.osm.pbf"
## )


## ----set-extract--------------------------------------------------------------
path_to_extract <- paste(tempdir(), "osm_file.osm.pbf", sep = "/")
download.file("https://download.geofabrik.de/europe/germany/nordrhein-westfalen/arnsberg-regbez-latest.osm.pbf",
              destfile = path_to_extract)
file.path("openrouteservice-master/docker/data", basename(ors$extract$set_extract(path_to_extract)))


## ----get-extract--------------------------------------------------------------
ors$extract$get_extract("Arnsberg", provider = "geofabrik")


## ----init-config--------------------------------------------------------------
ors$config


## ----active-profiles----------------------------------------------------------
ors$config$active_profiles <- c("car", "bike-regular", "walking")
ors$config$ors_config$ors$services$routing$profiles$active


## ----parsed-config------------------------------------------------------------
# Setting the maximum snapping radius to -1 removes the limit for coordinates to be snapped to a
# nearby geometry edge. For coordinates that are far off from ways or streets, this is especially
# useful
ors$
        config$
        ors_config$
        ors$
        services$
        routing$
        profiles$
        default_params$
        maximum_snapping_radius <- -1
ors$
        config$
        ors_config$
        ors$
        services$
        routing$
        profiles$
        `profile-car`$
        parameters$
        maximum_snapping_radius <- NULL


## ----save-config--------------------------------------------------------------
ors$config$save_config()


## ----init-setup-settings------------------------------------------------------
ors$setup_settings


## ----graphbuilding------------------------------------------------------------
ors$setup_settings$graph_building <- "build"


## ----memory-------------------------------------------------------------------
ors$setup_settings$allocate_memory(init = NULL, max = NULL)
as.data.frame(ors$setup_settings$memory)


## ----init-docker--------------------------------------------------------------
ors$docker


## ----green-lights-------------------------------------------------------------
data.frame(
  status = c(
    ors$docker$docker_running,
    ors$docker$image_built,
    ors$docker$container_exists,
    ors$docker$container_running,
    ors$docker$service_ready
  ),
  row.names = c(
    'Is docker running?',
    'Does the image exist?',
    'Does the container exist?',
    'Is the container running?',
    'Is the service ready to use?'
  )
)


## ----image-up-----------------------------------------------------------------
# Verbose is set to FALSE to not litter the output, but it can provide useful
# information on the process
ors$docker$image_up(wait = TRUE, verbose = FALSE)




## ----change-extract-----------------------------------------------------------
# Remove container
ors$docker$container_down()

# Get a new extract and set graph building to "change".
# This allows Docker to overwrite the graphs directory
# using the new extract.
ors$extract$get_extract("Cologne", provider = "geofabrik")
ors$setup_settings$graph_building <- "change"

# Finally, start the container again.
ors$docker$image_up(verbose = FALSE)




## ----change-config------------------------------------------------------------
# Stop the container
ors$docker$stop_container()

# Re-initialize ORSConfig to get the active config file
ors$config <- "refresh"

# Change the default maximum distance. This only changes the maximum distance
# for profiles that don't have their own distance limit.
ors$config$ors_config$ors$services$routing$profiles$default_params$maximum_distance <- 1000000

# driving-car has its own distance limit by default, so we'll eliminate it
ors$config$ors_config$ors$services$routing$profiles$`profile-car`$parameters$maximum_distance <- NULL

# Don't forget to save changes to the parsed config file
ors$config$save_config()

# Re-start the container
ors$docker$start_container()


## ----all-fine-----------------------------------------------------------------
set.seed(111)
sample_a <- ors_sample(size = 20)

set.seed(222)
sample_b <- ors_sample(size = 20)

get_route_lengths(sample_a, sample_b, profile = "driving-car")

