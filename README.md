
<!-- README.md is generated from README.Rmd. Please edit that file -->
<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
<!-- badges: end -->

**NOTE** ℹ️: This is the repository for the companion package to our
paper Altmeyer, Agusti, and Vidal-Quadras Costa (2021). We recently
presented our paper at the [NeurIPS 2021 MLECON
workshop](https://nips.cc/Conferences/2021/ScheduleMultitrack?event=21847).
For the specific code used for the paper and additional resources please
see [this repository](https://github.com/pat-alt/deepvarsMacro).

## `deepvars`

The `deepvars` package provides a framework for Deep Vector
Autoregression in R. The methodology is based on (Altmeyer, Agusti, and
Vidal-Quadras Costa 2021), a working paper initially prepared as part of
the [Masters Degree in Data
Science](https://bse.eu/study/masters-programs/data-science-methodology)
at [Barcelona School of Economics](https://bse.eu). For a summary of the
first version of the working paper see
[here](https://thevoice.bse.eu/2021/09/16/deep-vector-autoregression-for-macroeconomic-data/).

<p align="center">
<img src="www/hex.png" style="width: 250px;" />
</p>

## Installation

You can either clone this repository and install from source or simply
run the below in R:

``` r
devtools::install_github("pat-alt/deepvars", build_vignettes=TRUE)
library(deepvars)
```

## Getting started

Full documentation of the package is still a work-in-progress. In the
meantime, detailed guidance on different topics and estimation methods
covered by `deepvars`, can be found in the vignettes. Simply type the
following command once you have completed the steps above:

``` r
utils::browseVignettes('deepvars')
```

## Disclaimer

*Date*: 1 Februar, 2022.

This package was developed in tandem with the initial research for my
masters thesis. Documentation is incomplete and it should at this point
**not** be regarded as a fully-fledged, tested and production-ready
piece of software, so please bear this in mind. That being said, I’m
quite confident about the basic functionality of training and predicting
from a Deep VAR as well as various plotting methods that can be used for
visualizing the results. I encourage you to try it out yourself and
should you encounter any problem, please just open an issue.
