## ----setup, include = FALSE, warning = FALSE, message = FALSE------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----psychact, warning = FALSE, message = FALSE--------------------------
# loading needed libraries
library(ggstatsplot)
library(psych)
library(dplyr)

# looking at the structure of the data using glimpse
dplyr::glimpse(x = psych::sat.act)

## ----psychact2, warning = FALSE, message = FALSE, fig.height = 6, fig.width = 7----
ggstatsplot::gghistostats(
    data = psych::sat.act,                              # data from which variable is to be taken
    x = ACT,                                            # numeric variable
    results.subtitle = FALSE,                           # don't run statistical tests
    messages = FALSE,                                   # turn off messages
    xlab = "ACT Score",                                 # x-axis label
    title = "Distribution of ACT Scores",               # title for the plot
    subtitle = "N = 700",                               # subtitle for the plot
    caption = "Data courtesy of: SAPA project (https://sapa-project.org)", # caption for the plot
    centrality.k = 1                                    # show 1 decimal places for centrality label
  )

## ----psychact3, warning = FALSE, message = FALSE, fig.height = 6, fig.width = 7----
ggstatsplot::gghistostats(
    data = psych::sat.act,                              # data from which variable is to be taken
    x = ACT,                                            # numeric variable
    results.subtitle = TRUE,                            # run statistical tests
    messages = TRUE,                                    # turn on messages
    bar.measure = "proportion",                         # proportions
    xlab = "ACT Score",                                 # x-axis label
    title = "Distribution of ACT Scores",               # title for the plot
    type = "p",                                         # one sample t-test
    bf.message = TRUE,                                  # display Bayes method results
    ggtheme = ggthemes::theme_tufte(),                  # changing default theme
    bar.fill = "#D55E00",                               # change fill color
    test.value = 20,                                    # test value 
    test.value.line = TRUE,                             # show a vertical line at `test.value`
    caption = "Data courtesy of: SAPA project (https://sapa-project.org)", # caption for the plot
    centrality.k = 1                                    # show 1 decimal places for centrality label
  )

## ----grouped1, warning = FALSE, message = FALSE, fig.height = 10, fig.width = 7----
ggstatsplot::grouped_gghistostats(
  # arguments relevant for ggstatsplot::gghistostats
  data = psych::sat.act,                        # same dataset
  x = ACT,                                      # same outcome variable
  xlab = "ACT Score",
  grouping.var = gender,                        # grouping variable males = 1, females = 2
  title.prefix = "Gender",                      # prefix for the fixed title
  k = 1,                                        # number of decimal places in results
  type = "r",                                   # robust test: one-sample percentile bootstrap
  robust.estimator = "mom",                     # changing the robust estimator used
  test.value = 20,                              # test value against which sample mean is to be compared
  test.value.line = TRUE,                       # show a vertical line at `test.value`
  bar.measure = "density",                      # density
  centrality.para = "median",                   # plotting centrality parameter
  centrality.color = "#D55E00",                 # color for centrality line and label
  test.value.color = "#009E73",                 # color for test line and label
  messages = FALSE,                             # turn off messages
  ggtheme = ggthemes::theme_stata(),            # changing default theme
  ggstatsplot.layer = FALSE,                    # turn off ggstatsplot theme layer
  # arguments relevant for ggstatsplot::combine_plots
  title.text = "Distribution of ACT scores across genders",
  caption.text = "Data courtesy of: SAPA project (https://sapa-project.org)",
  nrow = 2,
  ncol = 1,
  labels = c("Male","Female")
)

## ----session_info-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
options(width = 200)
devtools::session_info()

