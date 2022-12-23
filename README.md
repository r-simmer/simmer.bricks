# <img src="https://raw.githubusercontent.com/r-simmer/r-simmer.github.io/master/images/simmer-logo.png" alt="simmer" width="200" />.bricks

<!-- badges: start -->
[![build](https://github.com/r-simmer/simmer.bricks/actions/workflows/build.yml/badge.svg)](https://github.com/r-simmer/simmer.bricks/actions/workflows/build.yml)
[![Coverage Status](https://codecov.io/gh/r-simmer/simmer.bricks/branch/master/graph/badge.svg)](https://codecov.io/gh/r-simmer/simmer.bricks)
[![CRAN\_Status\_Badge](https://www.r-pkg.org/badges/version/simmer.bricks)](https://cran.r-project.org/package=simmer.bricks)
[![Downloads](https://cranlogs.r-pkg.org/badges/simmer.bricks)](https://cran.rstudio.com/package=simmer.bricks)
<!-- badges: end -->

**simmer.bricks** provides helper methods for [**simmer**](http://r-simmer.org), the Discrete-Event Simulation (DES) package for R. Each **simmer** *brick* wraps a common activity pattern that can be used to build trajectories more conveniently.

## Documentation

Documentation is available at [r-simmer.org/extensions/bricks/reference](http://r-simmer.org/extensions/bricks/reference). To get started, please explore our [vignettes online](http://r-simmer.org/extensions/bricks/articles/), or in R:

``` r
vignette(package = "simmer.bricks")
```

## Installation

Install the release version from CRAN:

``` r
install.packages("simmer.bricks")
```

The installation from GitHub requires the [remotes](https://cran.r-project.org/package=remotes) package.

``` r
# install.packages("remotes")
remotes::install_github("r-simmer/simmer.bricks")
```
