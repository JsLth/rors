# <img src="inst/figures/orsr_sticker.png" width = "150" align="right" />

<!-- badges: start -->

[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

# Overview

The main purpose of the package is to calculate the routing distances and durations from geo-referenced survey data or other geolocations to nearby points of interest (e.g. hospitals). To achieve this, the package provides two routing functions that are powered by OpenRouteService (ORS), and two functions to either extract OpenStreetMap elements or digest local location datasets. To enable the processing of large datasets, the package requires a local installation of the OpenRouteService backend. The setup can be done manually but is time-consuming and not very intuitive. The package provides an R6 class to set up the backend and manage it from within R.

# Installation

The package is only available over GitHub an can be installed using

```
remotes::install_github("JsLth/ORSRouting")
```
