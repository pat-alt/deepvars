
<!-- README.md is generated from README.Rmd. Please edit that file -->
<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
<!-- badges: end -->

# SVAA

The goal of SVAA is to provide an accessible and scalable framework for
structural vector autoregressive analysis in R. Similar and more
comprehensive packages have long existed and are well-established within
the R community (namely
[vars](https://cran.r-project.org/web/packages/vars/vars.pdf)). SVAA is
designed to be more flexible and educational in nature. The package
draws heavily on the text book [Structural Vector Autoregressive
Analysis](https://sites.google.com/site/lkilian2019/textbook) by Lutz
Kilian and Helmut Luethkepohl. Notation in the code is closely aligned
with notation in the text back which should make it accessible to
students.

## Installation

You can either clone this repository and install from source or simply
run the below in R:

``` r
devtools::install_github("pat-alt/SVAA", build_vignettes=TRUE)
```

Once installed you need to attach the package:

``` r
library(SVAA)
```

## Guidance

For detailed guidance on different topics and estimation methods covered
by SVAA, you can consult the package vignettes. Simply type
`utils::browseVignettes()` once you have completed the steps above.
