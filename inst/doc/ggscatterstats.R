## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----ggplot2movies1, warning = FALSE, message = FALSE--------------------
library(ggstatsplot)

# see the selected data (we have data from 1813 movies)
dplyr::glimpse(x = ggstatsplot::movies_wide)

## ----ggscatterstats1, warning = FALSE, message = FALSE, fig.height = 6, fig.width = 8----
# for reproducibility
set.seed(123)

# to speed up the calculation, let's use only 10% of the data
movies_10 <- dplyr::sample_frac(tbl = ggstatsplot::movies_long, size = 0.1)

# plot
ggstatsplot::ggscatterstats(
  data = movies_10,                                            # dataframe from which variables are taken
  x = budget,                                                  # predictor/independent variable 
  y = rating,                                                  # dependent variable
  xlab = "Budget (in millions of US dollars)",                 # label for the x-axis
  ylab = "Rating on IMDB",                                     # label for the y-axis
  label.var = "title",                                         # variable to use for labeling data points
  label.expression = "rating < 5 & budget > 100",              # expression for deciding which points to label
  point.alpha = 0.7,
  point.size = 4,
  point.color = "grey50",
  marginal = TRUE,                                             # show marginal distribution 
  marginal.type = "density",                                   # type of plot for marginal distribution
  centrality.para = "mean",                                    # centrality parameter to be plotted
  margins = "both",                                            # marginal distribution on both axes
  xfill = "#CC79A7",                                           # fill for marginals on the x-axis
  yfill = "#009E73",                                           # fill for marginals on the y-axis
  xalpha = 0.5,                                                # transparency for the x-axis marginals
  yalpha = 0.75,                                               # transparency for the y-axis marginals
  xsize = 1,                                                   # size for the x-axis marginals
  ysize = 1,                                                   # size for the y-axis marginals
  type = "pearson",                                            # type of linear association
  title = "Relationship between movie budget and IMDB rating",
  caption = "Source: www.imdb.com",
  messages = FALSE
)

## ----ggscatterstats2, eval = FALSE---------------------------------------
#  # include the following code in your code chunk inside R Notebook or Markdown
#  grid::grid.newpage()
#  grid::grid.draw(
#    ggstatsplot::ggscatterstats(
#      data = ggstatsplot::movies_wide,
#      x = budget,
#      y = rating,
#      marginal = TRUE,
#      messages = FALSE
#    )
#  )

## ----grouped1, warning = FALSE, message = FALSE, fig.height = 12, fig.width = 7----
# for reproducibility
set.seed(123)

# to speed up the calculation, let's use only 20% of the data
# also since there are only 7 movies with NC-17 ratings, leave them out
movies_20 <-
  dplyr::filter(.data = ggstatsplot::movies_wide, mpaa != "NC-17") %>%
  dplyr::sample_frac(tbl = ., size = 0.2)

# plot
ggstatsplot::grouped_ggscatterstats(
  # arguments relevant for ggstatsplot::ggscatterstats
  data = movies_10,
  title.prefix = "MPAA Rating",
  x = budget,
  y = rating,
  xfill = NULL,
  package = "ggsci",
  palette = "nrc_npg",
  grouping.var = mpaa,
  label.var = "title",
  label.expression = "rating < 5 & budget > 75",
  marginal.type = "boxplot",
  ggtheme = ggthemes::theme_tufte(),
  ggstatsplot.layer = FALSE,
  messages = FALSE,
  k = 2,
  # arguments relevant for ggstatsplot::combine_plots
  title.text = "Relationship between movie budget and IMDB rating",
  caption.text = "Source: www.imdb.com",
  nrow = 3,
  ncol = 1,
  labels = c("(a)","(b)","(c)","(d)")
)

## ----grouped3, warning = FALSE, message = FALSE, fig.height = 12, fig.width = 12----
library(mgcv)

# for reproducibility
set.seed(123)

# creating a list of plots with different smoothing functions
plot_list2 <- purrr::pmap(
  .l = list(
    # let's use only 5% of the data to speed up the calculations
    data = list(dplyr::sample_frac(tbl = ggstatsplot::movies_wide, size = 0.05)),
    x = "budget",
    y = "rating",
    title = list(
      "Robust linear model using an M estimator (rlm)",
      "Generalized additive model (GAM) with a penalized smoother",
      "Local Polynomial Regression Fitting",
      "Quadratic fit"
    ),
    method = list(MASS::rlm, 
                  "gam", 
                  "loess", 
                  "lm"),
    formula = list(y ~ x, 
                   y ~ s(x, k = 3), 
                   y ~ x, 
                   y ~ x + I(x ^ 2)),
    line.color = list("#009E73", "#F0E442", "#0072B2", "#D55E00"),
    marginal = FALSE,
    results.subtitle = FALSE,
    messages = FALSE
  ),
  .f = ggstatsplot::ggscatterstats
)

# combining all individual plots from the list into a single plot using combine_plots function
ggstatsplot::combine_plots(
  plotlist = plot_list2,
  title.text = "Trying out different smoothing functions with ggscatterstats",
  caption.text = "Source: www.imdb.com",
  nrow = 2,
  ncol = 2,
  labels = c("(a)", "(b)", "(c)", "(d)")
)

## ----session_info-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
options(width = 200)
devtools::session_info()

