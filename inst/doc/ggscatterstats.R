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

# selecting data of interest
movies_wide <- ggplot2movies::movies %>%
  dplyr::select(.data = ., c(title:votes, mpaa)) %>%         # `.` is just a placeholder for the data
  dplyr::filter(.data = ., mpaa != "") %>%                   # removing movies without mpaa ratings
  stats::na.omit(.) %>%                                      # removing NAs 
  dplyr::mutate(.data = ., budget = budget/1000000) %>%      # convert the budge to millions of dollars
  dplyr::mutate_if(.tbl = .,                                 # convert mpaa ratings to a factor
                   .predicate = purrr::is_bare_character,
                   .funs = ~as.factor(.))
  
# see the selected data (we have data from 1813 movies)
dplyr::glimpse(x = movies_wide)

## ----ggscatterstats1, cache.extra = movies_wide, warning = FALSE, message = FALSE, fig.height = 6, fig.width = 8----

ggstatsplot::ggscatterstats(
  data = movies_wide,                                          # dataframe from which variables are to be taken
  x = budget,                                                  # predictor/independent variable 
  y = rating,                                                  # dependent variable
  xlab = "Budget (in millions of US dollars)",                 # label for the x-axis
  ylab = "Rating on IMDB",                                     # label for the y-axis
  marginal = TRUE,                                             # show marginal distribution 
  marginal.type = "histogram",                                 # type of plot for marginal distribution
  centrality.para = "mean",                                    # if and which type of centrality parameter to be plotted
  margins = "both",                                            # marginal distribution on both axes
  xfill = "#CC79A7",                                           # fill for marginal distributions on the x-axis
  yfill = "#009E73",                                           # fill for marginal distributions on the y-axis
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
#      data = movies_wide,
#      x = budget,
#      y = rating,
#      marginal = TRUE,
#      messages = FALSE
#    )
#  )

## ----grouped1, cache.extra = movies_wide, warning = FALSE, message = FALSE, fig.height = 14, fig.width = 8----
ggstatsplot::grouped_ggscatterstats(
  # arguments relevant for ggstatsplot::ggscatterstats
  data = movies_wide,
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

## ----grouped2, cache.extra = movies_wide, warning = FALSE, message = FALSE, fig.height = 16, fig.width = 8----
# let's split the dataframe and create a list by mpaa rating
mpaa_list <- movies_wide %>%
  base::split(x = ., f = .$mpaa, drop = TRUE)

# this created a list with 4 elements, one for each mpaa rating
str(mpaa_list)

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

