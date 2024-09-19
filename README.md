
# rors <img src="man/figures/rors_sticker.png" align="right" height="180/"/>

<!-- badges: start -->

[![R-CMD-check](https://github.com/JsLth/rors/actions/workflows/check-standard.yaml/badge.svg)](https://github.com/JsLth/rors/actions/workflows/check-standard.yaml)
[![Lifecycle:
maturing](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://lifecycle.r-lib.org/articles/stages.html#maturing)
[![](https://www.r-pkg.org/badges/version/rors)](https://cran.r-project.org/package=rors)
[![Codecov test
coverage](https://codecov.io/gh/JsLth/rors/branch/master/graph/badge.svg)](https://app.codecov.io/gh/JsLth/rors?branch=master)
[![CodeFactor](https://www.codefactor.io/repository/github/jslth/rors/badge/master)](https://www.codefactor.io/repository/github/jslth/rors/overview/master)

<!-- badges: end -->

The purpose of `rors` is to provide a tidy, pipeable and comprehensive R
interface to local or remote
[OpenRouteService](https://openrouteservice.org/) (ORS) instances.
`rors` currently enables analyses based on all available endpoints:

- Qualitative and quantitative routing computations
- Distance matrices
- Accessibility analyses
- Street snapping
- Graph network export

Functions are designed to be pipeable, API calls are performed
gracefully and the results are tidied up to digestible (sf) tibbles.

Another important feature of `rors` is the setup and management of local
OpenRouteService instances from scratch. Local instances facilitate
computationally intensive data analyses and allow the definition of
custom API configurations. While it is possible to use `rors` with the
official web API, requests will be very slow due to rate restrictions
and therefore not really suitable for larger scale analyses.

## Installation

You can install the development version of `rors` from
[GitHub](https://github.com/jslth/rors) with:

``` r
# install.packages("devtools")
devtools::install_github("jslth/rors")
```

## Basic usage

To connect to a running OpenRouteService server - or to build a new one,
use the workhorse function `ors_instance`:

``` r
library(rors)
library(sf)

# API keys are stored in the ORS_TOKEN environment variable
ors <- ors_instance(server = "public")
```

The ORS instance is then attached to the session and is automatically
detected by all other functions. To perform a simple routing request,
run `ors_inspect`:

``` r
ors_inspect(pharma, profile = "driving-car", level = "segment")
#> Simple feature collection with 7 features and 5 fields
#> Geometry type: MULTILINESTRING
#> Dimension:     XY
#> Bounding box:  xmin: -0.730349 ymin: 52.5876 xmax: -0.467708 ymax: 52.67472
#> Geodetic CRS:  WGS 84
#> # A tibble: 7 × 6
#>   name            distance duration avgspeed elevation                  geometry
#>   <chr>              <dbl>    <dbl>    <dbl>     <dbl>     <MULTILINESTRING [°]>
#> 1 Uppingham Road…   9983.     618.      69.8     120.  ((-0.722324 52.58762, -0…
#> 2 High Street, B…     26.3      2.4     39.4     112   ((-0.727928 52.66962, -0…
#> 3 Uppingham Road…  10059.     628.      68.8     120.  ((-0.728317 52.66962, -0…
#> 4 Uppingham Road…  10642.     676.      69.8     122.  ((-0.721097 52.58816, -0…
#> 5 Stamford Road,…  19187.    1144.      63.5      78.4 ((-0.730349 52.66991, -0…
#> 6 St Mary's Stre…    358.      61.9     25.0      34.9 ((-0.478222 52.65071, -0…
#> 7 St George's St…   1094.     140.      27.7      38.5 ((-0.477902 52.65231, -0…
```

## Local instances

While `rors` can work with public API requests, it is primarily designed
to be used together with local instances. The `ors_instance` family can
be used to manage, control and build local ORS instances. The following
code would jumpstart an initial instance, add an OSM extract of Rutland,
add three routing profiles, set a random port, 100 MB of RAM and finally
start the ORS instance. For more details, refer to
[`vignette("ors-installation")`](https://jslth.github.io/rors/articles/ors-installation.html).

``` r
ors <- ors_instance()$
  set_extract("Rutland")$
  add_profiles("car", "bike-regular", "walking")$
  set_port()$
  set_ram(0.1)$
  up()
```
