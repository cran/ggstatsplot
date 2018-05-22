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
library(ggstatsplot)
library(gapminder)

ggstatsplot::ggbetweenstats(
  data = dplyr::filter(.data = gapminder, year == 2007),
  x = continent,
  y = lifeExp,
  messages = FALSE
)

## ----ggbetweenstats2, message = FALSE, warning = FALSE, fig.height = 6, fig.width = 8----
library(ggstatsplot)
library(gapminder)

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
  title = "Comparison of life expectancy across continents (Year: 2007)",
  caption = "Source: Gapminder Foundation"
) +                                                           # modifying the plot further
  ggplot2::scale_y_continuous(limits = c(35,85), breaks = seq(from = 35, to = 85, by = 5))

## ----ggbetweenstats3, message = FALSE, warning = FALSE, fig.height = 12, fig.width = 8----
library(ggstatsplot)
library(gapminder)

# parametric ANOVA and box plot
p1 <- ggstatsplot::ggbetweenstats(
  data = dplyr::filter(.data = gapminder, year == 2007),
  x = continent,
  y = lifeExp,
  plot.type = "box",
  type = "p",
  messages = FALSE
)

# Kruskal-Wallis test (nonparametric ANOVA) and violin plot
p2 <- ggstatsplot::ggbetweenstats(
  data = dplyr::filter(.data = gapminder, year == 2007),
  x = continent,
  y = lifeExp,
  plot.type = "violin",
  type = "np",
  messages = FALSE
)

# robust ANOVA and boxviolin plot
p3 <- ggstatsplot::ggbetweenstats(
  data = dplyr::filter(.data = gapminder, year == 2007),
  x = continent,
  y = lifeExp,
  plot.type = "boxviolin",
  type = "r",
  messages = FALSE
)

# combining the individual plots into a single plot
ggstatsplot::combine_plots(
  p1, p2, p3, 
  nrow = 3, 
  ncol = 1, 
  labels = c("(a)", "(b)", "(c)"),
  title.text = "Comparison of life expectancy across continents (Year: 2007)",
  caption.text = "Note: Comparing results from parametric, non-parametric, and robust tests",
  title.size = 14,
  caption.size = 12
)

## ----grouped1, warning = FALSE, message = FALSE, fig.height = 20, fig.width = 7----
ggstatsplot::grouped_ggbetweenstats(
  # arguments relevant for ggstatsplot::ggbetweenstats
  data = dplyr::filter(
    .data = gapminder::gapminder,
    year == 1957 |
    year == 1967 |
    year == 1977 |
    year == 1987 |
    year == 1997 |
    year == 2007
  ),
  x = continent,
  y = lifeExp,
  outlier.tagging = TRUE,
  outlier.label = country,
  grouping.var = year,
  title.prefix = "Year",
  messages = FALSE,
  # arguments relevant for ggstatsplot::combine_plots
  title.text = "Changes in life expectancy across continents (1957-2007)",
  nrow = 6,
  ncol = 1,
  labels = c("(a)","(b)","(c)", "(d)", "(e)", "(f)")
)

## ----grouped2, warning = FALSE, message = FALSE, fig.height = 22, fig.width = 7----
# let's split the dataframe and create a list by years of interest
year_list <- gapminder::gapminder %>%
  dplyr::filter(
    .data = .,
    year == 1957 |
    year == 1967 |
    year == 1977 |
    year == 1987 |
    year == 1997 |
    year == 2007
  ) %>%
  base::split(x = ., f = .$year, drop = TRUE)

# this created a list with 4 elements, one for each mpaa rating
str(year_list)

# running function on every element of this list note that if you want the same
# value for a given argument across all elements of the list, you need to
# specify it just once
plot_list <- purrr::pmap(
  .l = list(
    data = year_list,
    x = "continent",
    y = "lifeExp",
    outlier.label = "country",
    outlier.label.color = list(
      "#56B4E9",
      "#009E73",
      "#F0E442",
      "#0072B2",
      "#D55E00",
      "#CC79A7"
    ),
    xlab = "Continent",
    ylab = "Life expectancy",
    title = list(
      "Year: 1957",
      "Year: 1967",
      "Year: 1977",
      "Year: 1987",
      "Year: 1997",
      "Year: 2007"
    ),
    type = list("r", "p", "np", "p", "np", "r"),
    effsize.type = list(
      "biased",
      "unbiased",
      "biased",
      "unbiased",
      "biased",
      "unbiased"
    ),
    plot.type = list("box", "boxviolin", "box", "boxviolin", "box", "violin"),
    messages = FALSE
  ),
  .f = ggstatsplot::ggbetweenstats
)
  
# combining all individual plots from the list into a single plot using combine_plots function
ggstatsplot::combine_plots(
  plotlist = plot_list,
  title.text = "Changes in life expectancy across continents (1957-2007)",
  title.color = "red",
  nrow = 6,
  ncol = 1,
  labels = c("(a)","(b)","(c)","(d)", "(e)", "(f)")
)

