
<!-- README.md is generated from README.Rmd. Please edit that file -->
dartboard
=========

The goal of dartboard is to creat an object with all the correct dimensions of a dartboard and use it as an input into dart strategy simulations.

Installation
------------

You can install the released version of dartboard from [GitHub](https://github.com) with:

``` r
devtools::install_github("chringer-git/dartboard")
```

Example
-------

For instance, you can draw the dartboard with a few simple functions.

``` r
library(dartboard)

# Create dartboard
db <- create_dartboard()

# Draw dartboard and return a ggplot2 object
draw_dartboard(db)
```

<img src="man/figures/README-example-1.png" width="100%" />
