#' ORSRouting: A package for routing distance computations using
#' OpenRouteservice
#'
#' The package allows the usage of OpenRouteService to calculate
#' the (shortest) routing distances of large datasets to certain points of
#' interest. It also provides functions to download or calculate nearby points
#' of interest. To process larger datasets, the package relies on a local
#' Docker setup of the OpenRouteService backend. The package facilitates the
#' setup by including a class that acts as a setup wizard and control panel.
#'
#' @section Exports:
#' ORS setup and control panel:
#' \itemize{
#'  \item \code{\link{ORSInstance}}
#'  \item \code{\link{ORSExtract}}
#'  \item \code{\link{ORSConfig}}
#'  \item \code{\link{ORSSetupSettings}}
#'  \item \code{\link{ORSDockerInterface}}
#' }
#'
#' Points of interest data extraction and formatting:
#' \itemize{
#'  \item \code{\link{get_osm_pois}}
#'  \item \code{\link{get_nearest_pois}}
#' }
#'
#' Route computation and inspection:
#' \itemize{
#'  \item \code{\link{get_route_lengths}}
#'  \item \code{\link{get_shortest_routes}}
#'  \item \code{\link{inspect_route}}
#' }
#'
#' Utility functions:
#' \itemize{
#'  \item \code{\link{get_profiles}}
#'  \item \code{\link{ors_ready}}
#'  \item \code{\link{last_ors_conditions}}
#'  \item \code{\link{grant_docker_privileges}}
#' }
#'
#' @section Imports:
#' \describe{
#'  \item{httr}{Necessary to query the ORS API on Docker}
#'  \item{jsonlite}{Necessary to parse ORS responses}
#'  \item{yaml}{Necessary to parse Docker compose files}
#'  \item{magrittr}{Necessary for pretty code to work}
#'  \item{purrr}{Necessary for pretty and efficient code to work}
#'  \item{R6}{Necessary to use ORSInstance and its children}
#'  \item{sf}{Necessary for all kinds of geometry computations}
#'  \item{memuse}{Necessary to aid Docker memory allocation}
#'  \item{dplyr}{Necessary for certain data wrangling operations}
#'  \item{osmdata}{Necessary for \code{get_osm_data}}
#'  \item{fields}{Necessary for \code{get_nearest_pois}}
#'  \item{tidyr}{Necessary for certain data wrangling operations}
#'  \item{osmextract}{Necessary for \code{$get_extract} and \code{ors_sample}}
#' }
#'
#' @references
#' This package is powered by OpenRouteService. For problems concerning their
#' service, refer to <https://github.com/GIScience/openrouteservice>
#' © openrouteservice.org by HeiGIT | Map data © OpenStreetMap contributors
#'
#' @encoding UTF-8
#' @docType package
#' @name ORSRouting
NULL