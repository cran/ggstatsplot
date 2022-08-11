## -----------------------------------------------------------------------------
# needed libraries
library(ggstatsplot)

knitr::opts_chunk$set(
  collapse = TRUE,
  out.width = "100%",
  dpi = 300,
  comment = "#>",
  message = FALSE,
  warning = FALSE
)

set.seed(123) # for reproducibility

## -----------------------------------------------------------------------------
citation("ggstatsplot")

## -----------------------------------------------------------------------------
library(ggstatsplot)

ggbetweenstats(iris, Species, Sepal.Length)

## -----------------------------------------------------------------------------
knitr::include_graphics("../man/figures/stats_reporting_format.png")

