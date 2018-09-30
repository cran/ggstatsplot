## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----gapminder, warning = FALSE, message = FALSE-------------------------
library(gapminder)
library(dplyr)

dplyr::glimpse(x = gapminder::gapminder)

## ----ggbetweenstats1, message = FALSE, warning = FALSE, fig.height = 6, fig.width = 8----
# loading the necessary libraries
library(ggstatsplot)
library(gapminder)

# since the confidence intervals for the effect sizes are computed using
# bootstrapping, important to set a seed for reproducibility
set.seed(123)

# function call
ggstatsplot::ggbetweenstats(
  data = dplyr::filter(.data = gapminder, year == 2007),
  x = continent,
  y = lifeExp,
  messages = FALSE
)

## ----ggbetweenstats2, message = FALSE, warning = FALSE, fig.height = 6, fig.width = 8----
library(ggstatsplot)
library(gapminder)

# for reproducibility
set.seed(123)

ggstatsplot::ggbetweenstats(
  data = dplyr::filter(.data = gapminder, year == 2007),      # dataframe
  x = continent,                                              # grouping/independent variable
  y = lifeExp,                                                # dependent variables
  xlab = "Continent",                                         # label for the x-axis
  ylab = "Life expectancy",                                   # label for the y-axis
  plot.type = "boxviolin",                                    # type of plot
  type = "parametric",                                        # type of statistical test  
  effsize.type = "biased",                                    # type of effect size
  outlier.tagging = TRUE,                                     # whether outliers should be flagged
  outlier.coef = 1.5,                                         # coefficient for Tukey's rule
  outlier.label = country,                                    # label to attacht to outlier values
  outlier.label.color = "red",                                # outlier point label color
  mean.plotting = TRUE,                                       # whether the mean is to be displayed
  mean.color = "darkblue",                                    # color for mean
  messages = FALSE,                                           # turn off messages
  ggtheme = ggthemes::theme_economist(),                      # a different theme
  package = "yarrr",                                          # package for a color palette
  palette = "info2",                                          # choosing a color palette
  title = "Comparison of life expectancy across continents (Year: 2007)",
  caption = "Source: Gapminder Foundation"
) +                                                           # modifying the plot further
  ggplot2::scale_y_continuous(limits = c(35,85), breaks = seq(from = 35, to = 85, by = 5))

## ----ggbetweenstats3, message = FALSE, warning = FALSE, fig.height = 10, fig.width = 10----
library(ggstatsplot)
library(gapminder)

# selecting subset of the data
df_year <- dplyr::filter(.data = gapminder::gapminder, 
                         year == 2007 | year == 1957)

# for reproducibility
set.seed(123)

# parametric ANOVA and box plot
p1 <- ggstatsplot::ggbetweenstats(
  data = df_year,
  x = year,
  y = lifeExp,
  plot.type = "box",
  type = "p",
  effsize.type = "d",
  title = "parametric test",
  package = "ggsci",
  palette = "nrc_npg",
  k = 2,
  messages = FALSE
)

# Kruskal-Wallis test (nonparametric ANOVA) and violin plot
p2 <- ggstatsplot::ggbetweenstats(
  data = df_year,
  x = year,
  y = lifeExp,
  plot.type = "violin",
  type = "np",
  title = "non-parametric test",
  package = "ggsci",
  palette = "uniform_startrek",
  k = 2,
  messages = FALSE
)

# robust ANOVA and boxviolin plot
p3 <- ggstatsplot::ggbetweenstats(
  data = df_year,
  x = year,
  y = lifeExp,
  plot.type = "boxviolin",
  type = "r",
  title = "robust test",
  tr = 0.005,
  package = "wesanderson",
  palette = "Royal2",
  nboot = 15,
  k = 2,
  messages = FALSE
)

# robust ANOVA and boxviolin plot
p4 <- ggstatsplot::ggbetweenstats(
  data = df_year,
  x = year,
  y = lifeExp,
  type = "bf",
  title = "bayesian test",
  package = "palettetown",
  palette = "natu",
  k = 2,
  messages = FALSE
)

# combining the individual plots into a single plot
ggstatsplot::combine_plots(
  p1, p2, p3, p4, 
  nrow = 2, 
  ncol = 2, 
  labels = c("(a)", "(b)", "(c)"),
  title.text = "Comparison of life expectancy between 1957 and 2007",
  caption.text = "Note: Comparing results from parametric, non-parametric, robust, and bayesian tests",
  title.size = 14,
  caption.size = 12
)

## ----grouped1, warning = FALSE, message = FALSE, fig.height = 15, fig.width = 7----
# for reproducibility
set.seed(123)

ggstatsplot::grouped_ggbetweenstats(
  # arguments relevant for ggstatsplot::ggbetweenstats
  data = dplyr::filter(
    .data = gapminder::gapminder,
    year == 1967 |
    year == 1987 |
    year == 2007, 
    continent != "Oceania"
  ),
  x = continent,
  y = lifeExp,
  # number of decimal places in results
  k = 2,
  nboot = 10,
  ggtheme = ggthemes::theme_tufte(),
  package = "ggsci",
  palette = "default_jco",
  outlier.tagging = TRUE,
  ggstatsplot.layer = FALSE,
  outlier.label = country,
  grouping.var = year,
  title.prefix = "Year",
  messages = FALSE,
  # arguments relevant for ggstatsplot::combine_plots
  title.text = "Changes in life expectancy across continents (1967-2007)",
  nrow = 3,
  ncol = 1,
  labels = c("(a)","(b)","(c)")
)

## ----session_info, eval = FALSE------------------------------------------
#  options(width = 200)
#  devtools::session_info()

