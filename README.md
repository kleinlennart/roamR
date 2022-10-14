
<!-- README.md is generated from README.Rmd. Please edit that file -->

# roamR

The R wrapper package for the [Roam Backend API
(beta)](https://roamresearch.com/#/app/developer-documentation/page/W4Po8pcHQ)!!

<!-- badges: start -->
<!-- badges: end -->

## Installation

You can install the development version of `roamR` with:

``` r
# install.packages("devtools")
devtools::install_github("kleinlennart/roamR")
```

## API Key

See
<https://roamresearch.com/#/app/developer-documentation/page/W4Po8pcHQ>
on how to get your **API Key**.

As of now, you have to sign in here to export it:
<https://relemma-git-feat-frontdesk.roamresearch.com/#/app>

## Example

``` r
library(roamR)

# get all pages as a tibble 
roam_q(
  query = "[:find ?p ?title :where [?p :node/title ?title]]",
  graph = "<GRAPH-NAME",
  key = "<GRAPH-API-KEY>", 
  format = "tibble"
)
```
