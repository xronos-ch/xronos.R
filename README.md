
# xronos <a href="https://r.xronos.ch"><img src="man/figures/logo.svg" align="right" height="139" /></a>

<!-- badges: start -->
[![Project Status: WIP – Initial development is in progress, but there has not yet been a stable, usable release suitable for the public.](https://www.repostatus.org/badges/latest/wip.svg)](https://www.repostatus.org/#wip)
[![CRAN status](https://www.r-pkg.org/badges/version/xronos)](https://CRAN.R-project.org/package=xronos)
[![R-CMD-check](https://github.com/xronos-ch/xronos.R/workflows/R-CMD-check/badge.svg)](https://github.com/xronos-ch/xronos.R/actions)
[![Codecov test coverage](https://codecov.io/gh/xronos-ch/xronos.R/branch/master/graph/badge.svg)](https://codecov.io/gh/xronos-ch/xronos.R?branch=master)
<!-- badges: end -->

An R client to [XRONOS](https://xronos.ch), a worldwide database of chronological information from archaeological contexts, including radiocarbon and dendrochronological data.

## Installation

You can install the development version of xronos using the [remotes](https://remotes.r-lib.org/) package:

``` r
remotes::install_github("xronos-ch/xronos.R")
```

<!--
You can install the released version of xronos from [CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("xronos")
```
-->

## Usage

Use `chron_data()` to get chronological data, using any of the search parameters supported by [the XRONOS API](https://xronos.ch/api):

``` r
chron_data(country = "Switzerland", material = c("charcoal", "bone"))
```

For further usage, see the [introductory vignette](https://xronos-ch.github.io/xronos.R/articles/xronos.html) (`vignette("xronos")`).

## Citation

If you use xronos in academic work, please consider citing:

To cite package 'xronos' in publications use:

> Roe, Joe and Martin Hinz (2021). xronos: Client for the 'XRONOS' Chronological Database. R package version 0.0.0.9000. https://r.xronos.ch

A BibTeX entry for LaTeX users is

```bib
  @Manual{,
    title = {xronos: Client for the 'XRONOS' Chronological Database},
    author = {Joe Roe and Martin Hinz},
    year = {2021},
    url = {https://r.xronos.ch},
    note = {R package version 0.0.0.9000},
  }
```
  
Please cite both 'xronos' and R when using 'xronos'. For R citation, see ‘citation()’

You can also get this citation information using 

``` r
citation("xronos")
```

