## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----gapminder, warning = FALSE, message = FALSE-------------------------
library(gapminder)
library(dplyr)

dplyr::glimpse(x = gapminder)

## ----ggcorrmat1, warning = FALSE, message = FALSE, fig.height = 6, fig.width = 6----
library(ggstatsplot)

# select data only from the year 2007
gapminder_2007 <- dplyr::filter(.data = gapminder::gapminder, year == 2007)

# producing the correlation matrix
ggstatsplot::ggcorrmat(
  data = gapminder_2007,             # data from which variable is to be taken
  cor.vars = lifeExp:gdpPercap       # specifying correlation matrix variables
)

## ----ggcorrmat2, message = FALSE, warning = FALSE, fig.height = 6, fig.width = 6----
ggstatsplot::ggcorrmat(
  data = gapminder_2007,                      # data from which variable is to be taken
  cor.vars = lifeExp:gdpPercap,               # specifying correlation matrix variables
  cor.vars.names = c("Life Expectancy", 
                     "population", 
                     "GDP (per capita)"),
  corr.method = "kendall",                    # which correlation coefficient is to be computed
  lab.col = "red",                            # label color
  ggtheme = ggplot2::theme_light(),           # selected ggplot2 theme
  ggstatsplot.layer = FALSE,                  # turn off default ggestatsplot theme overlay
  matrix.type = "lower",                      # correlation matrix structure
  colors = NULL,                              # turning off manual specification of colors
  palette = "category10_d3",                  # choosing a color palette
  package = "ggsci",                          # package to which color palette belongs
  title = "Gapminder correlation matrix",     # custom title
  subtitle = "Source: Gapminder Foundation"   # custom subtitle
)

## ----diamonds, warning = FALSE, message = FALSE--------------------------
library(ggplot2)

dplyr::glimpse(x = ggplot2::diamonds)

## ----ggcorrmat3, warning = FALSE, message = FALSE, fig.height = 7, fig.width = 7----
# for reproducibility
set.seed(123)

# let's use just 5% of the data to speed it up
ggstatsplot::ggcorrmat(
  data = dplyr::sample_frac(tbl = ggplot2::diamonds, size = 0.05),             
  cor.vars = c(carat, depth:z),        # note how the variables are getting selected
  cor.vars.names = c(
    "carat",
    "total depth",
    "table",
    "price",
    "length (in mm)",
    "width (in mm)",
    "depth (in mm)"
    ), 
  hc.order = TRUE                      # use hierarchical clustering
)

## ----ggcorrmat4, warning = FALSE, message = FALSE, fig.height = 7, fig.width = 7----
# for reproducibility
set.seed(123)

# let's use just 5% of the data to speed it up
ggstatsplot::ggcorrmat(
  data = dplyr::sample_frac(tbl = ggplot2::diamonds, size = 0.05),             
  cor.vars = c(price, carat, depth:table, x:z),    # note how the variables are getting selected
  cor.vars.names = c(
    "price",
    "carat",
    "total depth",
    "table",
    "length (in mm)",
    "width (in mm)",
    "depth (in mm)"
  ), 
  corr.method = "spearman",
  sig.level = 0.008,
  matrix.type = "lower",
  title = "Relationship between diamond attributes and price",
  subtitle = "Dataset: Diamonds from ggplot2 package",
  colors = c("#0072B2", "#D55E00", "#CC79A7"),
  lab.col = "yellow",
  lab.size = 6,
  pch = 7,
  pch.col = "white",
  pch.cex = 14,
  caption = expression(                            # changing the default caption text for the plot 
    paste(italic("Note"), ": Point shape denotes correlation non-significant at p < 0.008; adjusted for 6 comparisons")
  )
)

## ----ggcorrmat5, warning = FALSE, message = FALSE------------------------
# for reproducibility
set.seed(123)

# to get correlations
ggstatsplot::ggcorrmat(
  data = dplyr::sample_frac(tbl = ggplot2::diamonds, size = 0.15),             
  cor.vars = c(price, carat, depth:table, x:z),
  output = "correlations",
  corr.method = "robust",
  digits = 3
)

# to get p-values
ggstatsplot::ggcorrmat(
  data = dplyr::sample_frac(tbl = ggplot2::diamonds, size = 0.15),             
  cor.vars = c(price, carat, depth:table, x:z),
  output = "p-values",
  corr.method = "robust",
  digits = 3
)

## ----ggcorrmat6, warning = FALSE, message = FALSE, fig.height = 16, fig.width = 10----
# for reproducibility
set.seed(123)

# let's use just 5% of the data to speed it up
ggstatsplot::grouped_ggcorrmat(
  # arguments relevant for ggstatsplot::ggcorrmat
  data = dplyr::sample_frac(tbl = ggplot2::diamonds, size = 0.05),
  corr.method = "r",                  # percentage bend correlation coefficient
  beta = 0.2,                         # bending constant
  p.adjust.method = "holm",           # method to adjust p-values for multiple comparisons
  grouping.var = cut,
  title.prefix = "Quality of cut",
  cor.vars = c(carat, depth:z),
  cor.vars.names = c(
    "carat",
    "total depth",
    "table",
    "price",
    "length (in mm)",
    "width (in mm)",
    "depth (in mm)"
  ),
  lab.size = 3.5,
  # arguments relevant for ggstatsplot::combine_plots
  title.text = "Relationship between diamond attributes and price across cut",
  title.size = 16,
  title.color = "red",
  caption.text = "Dataset: Diamonds from ggplot2 package",
  caption.size = 14,
  caption.color = "blue",
  labels = c("(a)","(b)","(c)","(d)","(e)"),
  nrow = 3,
  ncol = 2
)

## ----ggcorrmat7, warning = FALSE, message = FALSE------------------------
# for reproducibility
set.seed(123)

# let's use just 5% of the data to speed it up
ggstatsplot::grouped_ggcorrmat(
  data = dplyr::sample_frac(tbl = ggplot2::diamonds, size = 0.05),
  grouping.var = cut,
  cor.vars = c(price, carat, depth:table, x:z),
  output = "correlations",
  corr.method = "robust",
  digits = 3
)

## ----session_info-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
options(width = 200)
devtools::session_info()

