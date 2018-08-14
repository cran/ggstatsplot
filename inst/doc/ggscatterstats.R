## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----ggplot2movies1, warning = FALSE, message = FALSE--------------------
library(ggplot2movies)
library(dplyr)

dplyr::glimpse(x = ggplot2movies::movies)

## ----ggplot2movies2, warning = FALSE, message = FALSE--------------------
library(ggstatsplot)

# see the selected data (we have data from 1813 movies)
dplyr::glimpse(x = ggstatsplot::movies_wide)

## ----ggscatterstats1, warning = FALSE, message = FALSE, fig.height = 6, fig.width = 8----

ggstatsplot::ggscatterstats(
  data = ggstatsplot::movies_wide,                             # dataframe from which variables are to be taken
  x = budget,                                                  # predictor/independent variable 
  y = rating,                                                  # dependent variable
  xlab = "Budget (in millions of US dollars)",                 # label for the x-axis
  ylab = "Rating on IMDB",                                     # label for the y-axis
  marginal = TRUE,                                             # show marginal distribution 
  marginal.type = "density",                                   # type of plot for marginal distribution
  centrality.para = "mean",                                    # if and which type of centrality parameter to be plotted
  margins = "both",                                            # marginal distribution on both axes
  xfill = "#CC79A7",                                           # fill for marginal distributions on the x-axis
  yfill = "#009E73",                                           # fill for marginal distributions on the y-axis
  xalpha = 0.5,                                                # changing transparency for the x-axis marginals
  yalpha = 0.75,                                               # changing transparency for the y-axis marginals
  xsize = 1,                                                   # changing size for the x-axis marginals
  ysize = 1,                                                   # changing size for the y-axis marginals
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

## ----grouped1, warning = FALSE, message = FALSE, fig.height = 14, fig.width = 8----
ggstatsplot::grouped_ggscatterstats(
  # arguments relevant for ggstatsplot::ggscatterstats
  data = ggstatsplot::movies_wide,
  title.prefix = "MPAA Rating",
  x = budget,
  y = rating,
  grouping.var = mpaa,
  marginal.type = "boxplot",
  # arguments relevant for ggstatsplot::combine_plots
  title.text = "Relationship between movie budget and IMDB rating",
  caption.text = "Source: www.imdb.com",
  nrow = 4,
  ncol = 1,
  labels = c("(a)","(b)","(c)","(d)")
)

## ----grouped2, warning = FALSE, message = FALSE, fig.height = 16, fig.width = 8----
# let's split the dataframe and create a list by mpaa rating
mpaa_list <- ggstatsplot::movies_wide %>%
  base::split(x = ., f = .$mpaa, drop = TRUE)

# this created a list with 4 elements, one for each mpaa rating
# you can check the structure of the file for yourself
# str(mpaa_list)

# checking the length and names of each element
length(mpaa_list)
names(mpaa_list)

# running function on every element of this list note that if you want the same
# value for a given argument across all elements of the list, you need to
# specify it just once
plot_list <- purrr::pmap(
  .l = list(
    data = mpaa_list,
    x = "budget",
    y = "rating",
    xlab = "Budget (in millions of US dollars)",
    ylab = "Rating on IMDB",
    title = list(
      "MPAA Rating: NC-17",
      "MPAA Rating: PG",
      "MPAA Rating: PG-13",
      "MPAA Rating: R"
    ),
    type = list("r", "np", "np", "np"),
    marginal.type = list("histogram", "boxplot", "density", "violin"),
    centrality.para = "mean",
    xfill = list("#56B4E9", "#009E73", "#999999", "#0072B2"),
    yfill = list("#D55E00", "#CC79A7", "#F0E442", "#D55E00"),
    ggtheme = list(
      ggplot2::theme_grey(),
      ggplot2::theme_classic(),
      ggplot2::theme_light(),
      ggplot2::theme_minimal()
    ),
    messages = FALSE
  ),
  .f = ggstatsplot::ggscatterstats
)
  
# combining all individual plots from the list into a single plot using combine_plots function
ggstatsplot::combine_plots(
  plotlist = plot_list,
  title.text = "Relationship between movie budget and IMDB rating",
  caption.text = "Source: www.imdb.com",
  caption.size = 16,
  title.color = "red",
  caption.color = "blue",
  nrow = 4,
  ncol = 1,
  labels = c("(a)","(b)","(c)","(d)")
)

