<!-- README.md is generated from README.Rmd. Please edit that file -->
SVAA
====

The goal of SVAA is is to provide an accessible and scalable framework
for structural vector autoregressive analysis in R. While similar - and
at this point more comprehensive - packages have long existed and been
established within the R community (namely
[vars](https://cran.r-project.org/web/packages/vars/vars.pdf)). This guide will not only take you through the core
functionality of SVAA, but also explain theory underlying the code. The
package draws heavily on the text book [Structural Vector Autoregressive
Analysis](https://sites.google.com/site/lkilian2019/textbook) by Lutz
Kilian and Helmut Luethkepohl.

Installation
------------

You can install SVAA from GitHub through:

    idevtools::install_git("pat-alt/SVAA")

Once installed you need to attach the package:

    library(SVAA)

Guidance
--------

For detailed guidance on different topics and estimation methods covered
by SVAA, you should consult the package vignettes. Simply type
`utils::browseVignettes()` once you have completed the steps above.
