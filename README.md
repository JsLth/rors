
# ORSRouting

<img src="man/figures/orsrouting_sticker.png" width = "150" align="right" />

<!-- badges: start -->

[![R-CMD-check](https://github.com/JsLth/ORSRouting/actions/workflows/check-standard.yaml/badge.svg)](https://github.com/JsLth/ORSRouting/actions/workflows/check-standard.yaml)
[![Lifecycle:
maturing](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://lifecycle.r-lib.org/articles/stages.html#maturing)
[![](https://www.r-pkg.org/badges/version/ORSRouting)](https://cran.r-project.org/package=ORSRouting)

<!-- badges: end -->

The purpose of this package is to provide a comprehensive and convenient
R interface to local OpenRouteService instances in order to facilitate
batch routing. The package functions (so far) enable qualitative and
quantitative route computations, distance matrix generation, and
accessibility analyses (i.e. isochrones). Also included is a function
family to build local customized OpenRouteService instances from
scratch. While it is possible to use ORSRouting with the official web
API, requests will be very slow due to rate restrictions and therefore
not really suitable for larger scale analyses.

## Installation

You can install the development version of ORSRouting from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("JsLth/ORSRouting")
```

## Basic usage

To connect to a running OpenRouteService server - or to build a new one,
use the workhorse function `ors_instance`:

``` r
library(ORSRouting)
#> © openrouteservice.org by HeiGIT | Data © OpenStreetMap contributors, ODbL 1.0. https://www.openstreetmap.org/copyright
library(sf)
#> Linking to GEOS 3.11.2, GDAL 3.7.2, PROJ 9.3.0; sf_use_s2() is TRUE

# API keys are stored in the ORS_TOKEN environment variable
ors <- ors_instance(server = "public")
```

The ORS instance is then attached to the session and is automatically
detected by all other functions. To perform a simple routing request,
run `ors_inspect`:

``` r
ors_inspect(pharma[1:2, ], profile = "driving-car", extra_info = "surface")
#> Simple feature collection with 139 features and 9 fields
#> Geometry type: LINESTRING
#> Dimension:     XY
#> Bounding box:  xmin: -0.727928 ymin: 52.5876 xmax: -0.704255 ymax: 52.66963
#> Geodetic CRS:  WGS 84
#> # A tibble: 139 × 10
#>    name       distance duration avgspeed elevation  type instruction exit_number
#>  * <chr>           [m]      [s]   [km/h]       [m] <int> <chr>             <int>
#>  1 Market Pl…    19.5      7        10.0      148     11 Head west …          NA
#>  2 Orange St…    25.0      2.06     43.6      148      1 Turn right…          NA
#>  3 Orange St…    23.0      1.89     43.8      148      1 Turn right…          NA
#>  4 Orange St…     7.92     0.65     43.9      148      1 Turn right…          NA
#>  5 Orange St…    16.4      1.35     43.8      148      1 Turn right…          NA
#>  6 Orange St…    23.4      1.93     43.6      148      1 Turn right…          NA
#>  7 Orange St…    63.8      5.25     43.7      148      1 Turn right…          NA
#>  8 Orange St…    81.8      6.73     43.7      148      1 Turn right…          NA
#>  9 Orange St…    40.2      3.31     43.7      147.     1 Turn right…          NA
#> 10 Orange St…    42.7      3.52     43.7      147.     1 Turn right…          NA
#> # ℹ 129 more rows
#> # ℹ 2 more variables: surface <fct>, geometry <LINESTRING [°]>
```

## Local instances

While `ORSRouting` can work with public API requests, it is primarily
designed to be used together with local instances. The `ors_instance`
family can be used to manage, control and build local ORS instances. The
following code would jumpstart an initial instance, add an OSM extract
of Rutland, add three routing profiles, set a random port, 100 MB of RAM
and finally start the ORS instance. For more details, refer to
`vignette("ors-installation")`.

``` r
ors <- ors_instance(dir = "~")
ors$set_extract("Rutland", provider = "geofabrik")
ors$add_profiles("car", "bike-regular", "walking")
ors$set_port()
ors$set_ram(0.1)
ors$up()
```
