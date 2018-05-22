## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
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
  ggplot2::geom_point()

# Basic scatter plot with theme_mprl() added
ggplot2::ggplot(
  data = datasets::mtcars, 
  mapping = ggplot2::aes(x = wt, y = mpg)
) + 
  ggplot2::geom_point() + 
  ggstatsplot::theme_mprl()

## ----ggcorrmat_theme1, message = FALSE, warning = FALSE, fig.height = 6, fig.width = 6----
ggstatsplot::ggcorrmat(
  data = datasets::iris,
  cor.vars = c(Sepal.Length:Petal.Width),
  type = "upper",
  ggtheme = ggplot2::theme_dark,          # selected ggplot2 theme
  ggstatsplot.theme = FALSE               # turn off overlaying theme_mprl on selected ggplot2 theme
)

## ----ggcorrmat_theme2, message = FALSE, warning = FALSE, fig.height = 6, fig.width = 6----
ggstatsplot::ggcorrmat(
  data = datasets::iris,
  cor.vars = c(Sepal.Length:Petal.Width),
  type = "upper",
  ggtheme = ggplot2::theme_dark,         # selected ggplot2 theme
  ggstatsplot.theme = TRUE                # overlaying theme_mprl on selected ggplot2 theme
)

