## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  dpi = 300,
  out.width = "100%",
  collapse = TRUE,
  comment = "#>"
)

## ----theme_mprl, message = FALSE, warning = FALSE, fig.height = 4, fig.width = 4----
library(ggplot2)

# Basic scatter plot
ggplot2::ggplot(
  data = datasets::mtcars, 
  mapping = ggplot2::aes(x = wt, y = mpg)
) + 
  ggplot2::geom_point() +
  ggplot2::theme_bw()

# Basic scatter plot with theme_mprl() added
ggplot2::ggplot(
  data = datasets::mtcars, 
  mapping = ggplot2::aes(x = wt, y = mpg)
) + 
  ggplot2::geom_point() + 
  ggstatsplot::theme_mprl(ggtheme = ggplot2::theme_bw())

## ----theme1, message = FALSE, warning = FALSE, fig.height = 6, fig.width = 8----
ggstatsplot::ggbetweenstats(
  data = datasets::iris,
  x = Species,
  y = Sepal.Length,
  ggtheme = ggplot2::theme_grey()          # selected ggplot2 theme
)

## ----theme2, message = FALSE, warning = FALSE, fig.height = 6, fig.width = 8----
ggstatsplot::ggscatterstats(
  data = datasets::iris,
  x = Sepal.Width,
  y = Sepal.Length,
  ggtheme = ggplot2::theme_dark()          # selected ggplot2 theme
)

## ----theme3, message = FALSE, warning = FALSE, fig.height = 6, fig.width = 6----
ggstatsplot::ggcorrmat(
  data = datasets::iris,
  cor.vars = c(Sepal.Length:Petal.Width),
  type = "upper",
  ggtheme = ggplot2::theme_dark            # selected ggplot2 theme
)

