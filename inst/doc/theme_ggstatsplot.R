## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  dpi = 300,
  out.width = "100%",
  collapse = TRUE,
  comment = "#>"
)

## ----theme_ggstatsplot, message = FALSE, warning = FALSE, fig.height = 4, fig.width = 8----
library(ggplot2)

ggstatsplot::combine_plots(
  # basic scatter plot
  ggplot2::ggplot(
    data = datasets::mtcars,
    mapping = ggplot2::aes(x = wt, y = mpg)
  ) +
    ggplot2::geom_point(),
  # basic scatter plot with theme_ggstatsplot() added
  ggplot2::ggplot(
    data = datasets::mtcars,
    mapping = ggplot2::aes(x = wt, y = mpg)
  ) +
    ggplot2::geom_point() +
    ggstatsplot::theme_ggstatsplot(),
  labels = c("(i)", "(ii)"),
  nrow = 1,
  title.text = "Plot with and without ggstatsplot theme"
)

## ----theme1, message = FALSE, warning = FALSE, fig.height = 6, fig.width = 6----
ggstatsplot::ggcorrmat(
  data = datasets::iris,
  cor.vars = c(Sepal.Length:Petal.Width),
  matrix.type = "upper",
  ggtheme = ggthemes::theme_fivethirtyeight(),            # selected ggplot2 theme
  ggstatsplot.layer = FALSE
)

## ----session_info-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
options(width = 200)
devtools::session_info()

