## ----setup, include = FALSE, warning = FALSE, message = FALSE------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----gapminder, warning = FALSE, message = FALSE-------------------------
library(gapminder)
library(dplyr)

dplyr::glimpse(x = gapminder::gapminder)

## ----age_distribution1, warning = FALSE, message = FALSE, fig.height = 6----
library(ggstatsplot)

gapminder::gapminder %>%
  dplyr::filter(.data = ., year == 2007) %>%            # select data only from the year 2007
  ggstatsplot::gghistostats(
    data = .,                                           # data from which variable is to be taken
    x = pop,                                            # numeric variable
    results.subtitle = FALSE,                           # don't run statistical tests
    messages = FALSE,                                   # turn off messages
    xlab = "Population",                                # x-axis label
    title = "Distribution of population worldwide",     # title for the plot
    subtitle = "Year: 2007",                            # subtitle for the plot
    caption = "Data courtesy of: Gapminder Foundation"  # caption for the plot
  )

## ----age_distribution2, warning = FALSE, message = FALSE, fig.height = 6----
gapminder::gapminder %>%
  dplyr::filter(.data = ., year == 2007) %>%            # select data only from the year 2007
  dplyr::mutate(.data = ., pop_log = log10(pop)) %>%    # creating new population variable
  ggstatsplot::gghistostats(
    data = .,                                           # data from which variable is to be taken
    x = pop_log,                                        # numeric variable
    results.subtitle = FALSE,                           # don't run statistical tests
    messages = FALSE,                                   # turn off messages
    xlab = "Population (logarithmic)",                  # x-axis label
    title = "Distribution of population worldwide",     # title for the plot
    subtitle = "Year: 2007",                            # subtitle for the plot
    caption = "Data courtesy of: Gapminder Foundation", # caption for the plot
    binwidth = 1                                        # new binwidth
  )

## ----age_distribution3, warning = FALSE, message = FALSE, fig.height = 6----
gapminder::gapminder %>%
  dplyr::filter(.data = ., year == 2007) %>%            # select data only from the year 2007
  dplyr::mutate(.data = ., pop_log = log10(pop)) %>%    # creating new population variable
  ggstatsplot::gghistostats(
    data = .,                                           # data from which variable is to be taken
    x = pop_log,                                        # numeric variable
    bar.measure = "density",                            # bar height measure
    results.subtitle = FALSE,                           # don't run statistical tests
    messages = FALSE,                                   # turn off messages
    xlab = "Population (logarithmic)",                  # x-axis label
    title = "Distribution of population worldwide",     # title for the plot
    subtitle = "Year: 2007",                            # subtitle for the plot
    caption = "Data courtesy of: Gapminder Foundation", # caption for the plot
    binwidth = 1,                                       # new binwidth
    low.color = "#009E73",                              # color for the lower end of the colorbar
    high.color = "#F0E442"                              # color for the upper end of the colorbar
  )

## ----age_distribution4, warning = FALSE, message = FALSE, fig.height = 6----
gapminder::gapminder %>%
  dplyr::filter(.data = ., year == 2007) %>%            
  ggstatsplot::gghistostats(
    data = .,                                           # data from which variable is to be taken
    x = lifeExp,                                        # numeric variable
    bar.measure = "proportion",                         # bar height measure
    results.subtitle = FALSE,                           # don't run statistical tests
    messages = FALSE,                                   # turn off messages
    xlab = "Life expectancy",                           # x-axis label
    title = "Distribution of life expectancy",          # title for the plot
    subtitle = "Year: 2007",                            # subtitle for the plot
    caption = "Data courtesy of: Gapminder Foundation", # caption for the plot
    binwidth = 5,                                       # new binwidth
    low.color = "#009E73",                              # color for the lower end of the colorbar
    high.color = "#F0E442"                              # color for the upper end of the colorbar
  )

## ----stats1, warning = FALSE, message = FALSE, fig.height = 6------------
gapminder::gapminder %>%
  dplyr::filter(.data = ., year == 2007) %>%            # select data only from the year 2007
  ggstatsplot::gghistostats(
    data = .,                                           # data from which variable is to be taken
    x = lifeExp,                                        # numeric variable
    messages = FALSE,                                   # turn off messages
    test.value = 48,                                    # test value against which sample mean is to be compared
    xlab = "Life expectancy",                           # x-axis label
    title = "Life expectancy worldwide",                # title for the plot
    subtitle = "Year: 2007",                            # subtitle for the plot
    caption = "Data courtesy of: Gapminder Foundation", # caption for the plot
    centrality.para = "mean"                            # plotting centrality parameter (mean)
  )

## ----stats_bf, warning = FALSE, message = FALSE, fig.height = 6----------
gapminder::gapminder %>%
  dplyr::filter(.data = ., year == 2007) %>%
  ggstatsplot::gghistostats(
    data = .,                                              # data from which variable is to be taken
    x = lifeExp,                                           # numeric variable
    messages = FALSE,                                      # turn off messages
    type = "bf",                                           # bayesian one sample t-test
    test.value = 48,                                       # test value 
    xlab = "Life expectancy",                              # x-axis label
    title = "Life expectancy worldwide",                   # title for the plot
    subtitle = "Year: 2007",                               # subtitle for the plot
    caption = "Note: black line - 1950; blue line - 2007", # caption for the plot
    test.value.line = TRUE,                                # show a vertical line at `test.value`
    centrality.para = "mean"                               # plotting centrality parameter (mean)
  )

## ----grouped1, warning = FALSE, message = FALSE, fig.height = 12, fig.width = 12----
gapminder::gapminder %>%                        # select data only for the year 2007
  dplyr::filter(.data = ., year == 2007) %>%
ggstatsplot::grouped_gghistostats(
  # arguments relevant for ggstatsplot::gghistostats
  data = .,                                     # `.` is placeholder for data plugged by %>%
  x = lifeExp,
  xlab = "Life expectancy",
  title.prefix = "Continent",                   # prefix for the fixed title
  type = "robust",                              # robust test: one-sample percentile bootstrap
  robust.estimator = "mom",                     # the modified one-step M-estimator of location
  test.value = 48,                              # test value against which sample mean is to be compared
  test.value.line = TRUE,                       # show a vertical line at `test.value`
  messages = FALSE,                             # turn off messages
  centrality.para = "mean",                     # plotting centrality parameter (mean)
  grouping.var = continent,                     # grouping variable with multiple levels
  # arguments relevant for ggstatsplot::combine_plots
  title.text = "Life expectancy change in different continents since 1950",
  caption.text = "Note: black line - 1950; blue line - 2007",
  nrow = 3,
  ncol = 2,
  labels = c("(a)","(b)","(c)","(d)","(e)")
)

## ----grouped2, warning = FALSE, message = FALSE, fig.height = 14, fig.width = 12----
# let's split the dataframe and create a list by continent
continent_list <- gapminder::gapminder %>%
  dplyr::filter(.data = ., year == 2007) %>%
  base::split(x = ., f = .$continent, drop = TRUE)

# this created a list with 5 elements, one for each continent
# you can check the structure of the file for yourself
# str(continent_list)

# checking the length and names of each element
length(continent_list)
names(continent_list)

# running function on every element of this list note that if you want the same
# value for a given argument across all elements of the list, you need to
# specify it just once
plot_list <- purrr::pmap(
  .l = list(
    data = continent_list,
    x = "lifeExp",
    xlab = "Life expectancy",
    test.value = list(35.6, 58.4, 41.6, 64.7, 63.4),
    type = list("p", "np", "r", "np", "p"),
    title = list(
      "Continent: Africa",
      "Continent: Americas",
      "Continent: Asia",
      "Continent: Europe",
      "Continent: Oceania"
    ),
    messages = FALSE,
    test.value.line = TRUE,
    test.value.color = "black",
    centrality.para = "mean",
    centrality.color = "blue",
    low.color = list("#56B4E9", "#009E73", "#999999", "#0072B2", "#D55E00"),
    high.color = list("#D55E00", "#CC79A7", "#F0E442", "#D55E00", "#56B4E9"),
    ggtheme = list(
      ggplot2::theme_grey(),
      ggplot2::theme_classic(),
      ggplot2::theme_light(),
      ggplot2::theme_minimal(),
      ggplot2::theme_bw()
    )
  ),
  .f = ggstatsplot::gghistostats
)
  
# combining all individual plots from the list into a single plot using combine_plots function
ggstatsplot::combine_plots(
  plotlist = plot_list,
  title.text = "Improvement in life expectancy worldwide since 1950",
  caption.text = "Note: black line - 1950; blue line - 2007",
  nrow = 3,
  ncol = 2,
  labels = c("(a)", "(b)", "(c)", "(d)", "(e)")
)

