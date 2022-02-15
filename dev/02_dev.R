# Building a Prod-Ready, Robust Shiny Application.
#
# README: each step of the dev files is optional, and you don't have to
# fill every dev scripts before getting started.
# 01_start.R should be filled at start.
# 02_dev.R should be used to keep track of your development during the project.
# 03_deploy.R should be used once you need to deploy your app.
#
#
###################################
#### CURRENT FILE: DEV SCRIPT #####
###################################

# Engineering

## Dependencies ----
## Add one line by package you want to add as dependency
usethis::use_package("parallel", min_version = T)
usethis::use_package("foreach", min_version = T)
usethis::use_package("tensorflow", min_version = T)
usethis::use_package("tfprobability", min_version = T)
usethis::use_package("keras", min_version = T)
usethis::use_package("data.table", min_version = T)
usethis::use_package("readr", min_version = T)
usethis::use_package("gridExtra", min_version = T)
usethis::use_package("scales", min_version = T)
usethis::use_package("ggplot2", min_version = T)
usethis::use_package("zoo", min_version = T)
usethis::use_package("stringr", min_version = T)
usethis::use_package("lubridate", min_version = T)
usethis::use_package("caret", min_version = T)
usethis::use_package("stats4", min_version = T)
usethis::use_package("expm", min_version = T)
usethis::use_package("tidyr", min_version = T)
usethis::use_package("lattice", min_version = T)
usethis::use_package("testthat", "Suggests", min_version = T)
usethis::use_package("rmarkdown")
usethis::use_package("knitr")

## Tests ----
## Add one line by test you want to create
usethis::use_test( "compute_J" )

## Data ----
usethis::use_data(canada, internal = F, overwrite = T)

# Documentation

## Vignette ----
usethis::use_vignette("Intro")s
usethis::use_vignette("VARs")
usethis::use_vignette("SVARs")
usethis::use_vignette("deep_vars")
devtools::build_vignettes()

## PDF manual ----
# Put "R CMD Rd2pdf ~/git/SVAA" into the terminal

## Code coverage ----
## (You'll need GitHub there)
# usethis::use_github()
# usethis::use_travis()
# usethis::use_appveyor()

## Creeate hex ----
library(hexSticker)
library(vars)
library(ggplotify)
data(Canada)
varres <- VAR(Canada[1:40,], p = 2, type = "none")
pred <- predict(varres, n.ahead = 50)
p <- as.ggplot(~fanchart(pred, names = "prod", axes=FALSE, main=NA))
s <- sticker(
  p, package="deepvars", p_size=7.5, s_x=1.05, s_y=.8, s_width=1.5, s_height=1,
  h_fill="#0697fd", p_color="white", h_color="black", filename="www/hex.png"
)
