# <img src="inst/figures/orsr_sticker.png" width = "150" align="right" />

<!-- badges: start -->

[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![](https://www.r-pkg.org/badges/version/ORSRouting)](https://cran.r-project.org/package=ORSRouting)
<!-- badges: end -->

# Overview

The purpose of this package is to provide a comprehensive and convenient `R` interface to local OpenRouteService instances in order to facilitate batch routing. The package functions (so far) enable qualitative and quantitative route computations, distance matrix generation, and accessibility analyses (i.e. isochrones). Also included is a function family to build local customized OpenRouteService instances from scratch. While it *is* possible to use `ORSRouting` with the official web API, requests will be very slow due to rate restrictions and therefore not really suitable for larger scale analyses.

# Installation

The package is only available over GitHub an can be installed using

```
remotes::install_github("JsLth/ORSRouting")
```
