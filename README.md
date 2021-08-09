# Overview

The main purpose of the package is to calculate the routing distances and durations from geo-referenced survey data or other geolocations to nearby points of interest (e.g. hospitals). To achieve this, the package provides two routing functions that are powered by OpenRouteService (ORS), and two functions to either extract OpenStreetMap elements or digest local location datasets. To enable the processing of large datasets, the package requires a local installation of the OpenRouteService backend. The setup can be done manually but is time-consuming and not very intuitive. The package provides an R6 class to set up the backend and manage it from within R.

# Installation

```
remotes::install_gitlab('liethjs/orsrouting')
```

After the installation, the package can be loaded into the R environment using

```
library(ORSRouting)
```

Since the package relies on a local installation of the OpenRouteService backend, the functions are only usable after installing the backend. For more details, refer to the vignette on setting up the backend:

```
vignette('ors-installation', package = 'ORSRouting')
```