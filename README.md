<!-- README.md is generated from README.Rmd. Please edit that file -->





# ORSRouting

# <img src="man/figures/orsrouting_sticker.png" width = "150" align="right" />

<!-- badges: start -->
[![R-CMD-check](https://github.com/JsLth/ORSRouting/actions/workflows/check-standard.yaml/badge.svg)](https://github.com/JsLth/ORSRouting/actions/workflows/R-CMD-check.yaml)
[![Lifecycle: maturing](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://lifecycle.r-lib.org/articles/stages.html#maturing)
[![](https://www.r-pkg.org/badges/version/ORSRouting)](https://cran.r-project.org/package=ORSRouting)

<!-- badges: end -->

The purpose of this package is to provide a comprehensive and convenient R interface to local OpenRouteService instances in order to facilitate batch routing. The package functions (so far) enable qualitative and quantitative route computations, distance matrix generation, and accessibility analyses (i.e. isochrones). Also included is a function family to build local customized OpenRouteService instances from scratch. While it is possible to use ORSRouting with the official web API, requests will be very slow due to rate restrictions and therefore not really suitable for larger scale analyses.

## Installation

You can install the development version of ORSRouting from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("JsLth/ORSRouting")
```

## Basic usage

To connect to a running OpenRouteService server - or to build a new one, use the workhorse function `ors_instance`:


```r
library(ORSRouting)

# API keys are stored in the ORS_TOKEN environment variable
ors <- ors_instance(server = "public")
```

The ORS instance is then attached to the session and is automatically detected by all other functions. To perform a simple routing request, run `ors_inspect`:


```r
ors_inspect(pharma[1:2, ], profile = "driving-car", extra_info = "surface")
#> Simple feature collection with 139 features and 9 fields
#> Geometry type: LINESTRING
#> Dimension:     XY
#> Bounding box:  xmin: -0.727928 ymin: 52.5876 xmax: -0.704255 ymax: 52.66963
#> Geodetic CRS:  WGS 84
#> # A tibble: 139 × 10
#>    name                 distance duration avgspeed elevation  type instruction      exit_number surface                  geometry
#>  * <chr>                     [m]      [s]   [km/h]       [m] <int> <chr>                  <int> <fct>            <LINESTRING [°]>
#>  1 Market Place            19.5      7        10.0      148     11 Head west on Ma…          NA Asphalt (-0.722324 52.58762, -0.…
#>  2 Orange Street, A6003    25.0      2.06     43.6      148      1 Turn right onto…          NA Paved   (-0.72261 52.5876, -0.72…
#>  3 Orange Street, A6003    23.0      1.89     43.8      148      1 Turn right onto…          NA Paved   (-0.722639 52.58782, -0.…
#>  4 Orange Street, A6003     7.92     0.65     43.9      148      1 Turn right onto…          NA Paved   (-0.722765 52.58801, -0.…
#>  5 Orange Street, A6003    16.4      1.35     43.8      148      1 Turn right onto…          NA Paved   (-0.722833 52.58807, -0.…
#>  6 Orange Street, A6003    23.4      1.93     43.6      148      1 Turn right onto…          NA Paved   (-0.722931 52.5882, -0.7…
#>  7 Orange Street, A6003    63.8      5.25     43.7      148      1 Turn right onto…          NA Paved   (-0.722971 52.58841, -0.…
#>  8 Orange Street, A6003    81.8      6.73     43.7      148      1 Turn right onto…          NA Paved   (-0.722933 52.58899, -0.…
#>  9 Orange Street, A6003    40.2      3.31     43.7      147.     1 Turn right onto…          NA Paved   (-0.722804 52.58972, -0.…
#> 10 Orange Street, A6003    42.7      3.52     43.7      147.     1 Turn right onto…          NA Paved   (-0.722753 52.59008, -0.…
#> # ℹ 129 more rows
```


## Local instances

While `ORSRouting` can work with public API requests, it is primarily designed to be used together with local instances. The `ors_instance` family can be used to manage, control and build local ORS instances. The following code would jumpstart an initial instance, add an OSM extract of Rutland, add three routing profiles, set a random port, 100 MB of RAM and finally start the ORS instance. For more details, refer to `vignette("ors-installation")`.


```r
ors <- ors_instance(dir = "~")
ors$set_extract("Rutland", provider = "geofabrik")
ors$add_profiles("car", "bike-regular", "walking")
ors$set_port()
ors$set_ram(0.1)
ors$up()
```

