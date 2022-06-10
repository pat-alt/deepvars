
<!-- README.md is generated from README.Rmd. Please edit that file -->
<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
<!-- badges: end -->

**NOTE** ℹ️: This is the repository for the companion package to our
paper Altmeyer, Agusti, and Vidal-Quadras Costa (2021). We presented our paper at the [NeurIPS 2021 MLECON
workshop](https://nips.cc/Conferences/2021/ScheduleMultitrack?event=21847).
For the specific code used for the paper and additional resources please
see [this repository](https://github.com/pat-alt/deepvarsMacro).

## Disclaimer ⚠

Since we worked on this project in 2021, Patrick has continued to work on the [`deepvar`](https://github.com/pat-alt/deepvars) package in his spare time. Among other things, he has found a bug in the original code, which has produced erroneous results for the test set in Table 1 of the paper that introduced the the approach ([see here](https://github.com/pat-alt/deepvarsMacro)). We are also not convinced that the empirical results presented in the paper are robust, after looking at replicating the findings for another dataset. Nonethess, we believe that the proposed methodological framework is interesting and this package should help facilitate future researchers interested in exploring the applicability of deep learning to macroeconomic data. Since Patrick is caught up in his PhD for now, this project is officially idle. Do feel free to salvage what you can though.

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

### Prerequisites

As one of its dependencies the `deepvars` uses `tensorflow`, which is an
R interface to the popular [TensorFlow](https://www.tensorflow.org)
library. We have tried to automate the TensorFlow configuration as
explained
[here](https://rstudio.github.io/reticulate/articles/python_dependencies.html).

``` r
install.packages("tensorflow")
tensorflow::install_tensorflow()
```

For uncertainty quantification we use `tensorflow_probability` for
Bayesian inference.

``` r
install.packages("tfprobability")
tfprobability::install_tfprobability()
```

Should you run into issues you may have to manually install the
TensorFlow dependencies. Detailed instructions to this end can be found
[here](https://tensorflow.rstudio.com/installation/).

### Install

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

*Date*: 2 December, 2021.

This package was developed in tandem with the initial research for my
masters thesis. Documentation is incomplete and it should at this point
**not** be regarded as a fully-fledged, tested and production-ready
piece of software, so please bear this in mind. That being said, I’m
quite confident about the basic functionality of training and predicting
from a Deep VAR as well as various plotting methods that can be used for
visualizing the results. I encourage you to try it out yourself and
should you encounter any problem, please just open an issue.
