## ----setup, include = FALSE, warning = FALSE, message = FALSE------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----gapminder, warning = FALSE, message = FALSE-------------------------
library(gapminder)
library(dplyr)
library(magrittr)

gapminder::gapminder %>%
  dplyr::glimpse(x = .)

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
    binwidth.adjust = TRUE,                             # adjust binwidth
    binwidth = 1                                        # new binwidth
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

## ----grouped1, warning = FALSE, message = FALSE, fig.height = 12, fig.width = 10----
gapminder::gapminder %>%
  dplyr::filter(.data = ., year == 2007) %>%
ggstatsplot::grouped_gghistostats(
  # arguments relevant for ggstatsplot::gghistostats
  data = .,
  x = lifeExp,
  xlab = "Life expectancy",
  type = "p",                                   # parametric test
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

