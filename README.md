# <img src="inst/figures/orsr_sticker.png" width = "150" align="right" />

<!-- badges: start -->
![Coverage](https://img.shields.io/badge/Coverage-7.39%25-red.svg)
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

# Overview

The main purpose of the package is to calculate the routing distances and durations from geo-referenced survey data or other geolocations to nearby points of interest (e.g. hospitals). To achieve this, the package provides two routing functions that are powered by OpenRouteService (ORS), and two functions to either extract OpenStreetMap elements or digest local location datasets. To enable the processing of large datasets, the package requires a local installation of the OpenRouteService backend. The setup can be done manually but is time-consuming and not very intuitive. The package provides an R6 class to set up the backend and manage it from within R.

# Installation

If you have access to the GitLab repository, you can simply pull the package:

```
remotes::install_git('https://git.gesis.org/liethjs/orsrouting.git')
```

If you do not have access, use the following deploy token:

```
remotes::install_git('https://gitlab+deploy-token-14:vQsKtxX75h2eg5oFjKNW@git.gesis.org/liethjs/orsrouting')
```

After the installation, the package can be loaded into the R environment using

```
library(ORSRouting)
```

Since the package relies on a local installation of the OpenRouteService backend, the functions are only usable after installing the backend. For more details, refer to the vignette on setting up the backend:

```
vignette('ors-installation', package = 'ORSRouting')
```
