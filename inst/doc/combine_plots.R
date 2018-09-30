## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----ggscatterstats_purrr, message = FALSE, warning = FALSE, fig.height = 12, fig.width = 8----
library(glue)
library(dplyr)
library(ggplot2)

# for reproducibility
set.seed(123)

# creating a list column with `ggstatsplot` plots
plots <- datasets::iris %>%
  dplyr::mutate(.data = ., Species2 = Species) %>% # just creates a copy of this variable
  dplyr::group_by(.data = ., Species) %>%                
  tidyr::nest(data = .) %>%                        # a nested dataframe with list column called `data`
  dplyr::mutate(                                   # creating a new list column of ggstatsplot outputs
    .data = .,
    plot = data %>%
      purrr::map(
        .x = .,
        .f = ~ ggstatsplot::ggscatterstats(
          data = .,
          x = Sepal.Length,
          y = Sepal.Width,
          xfill = "#0072B2",
          yfill = "#009E73",
          ggtheme = ggthemes::theme_fivethirtyeight(),
          ggstatsplot.layer = FALSE,
          messages = FALSE,                        # turns off warnings and notes messages   
          marginal.type = "boxplot",
          title =
            glue::glue("Species: {.$Species2} (n = {length(.$Sepal.Length)})")
        )
      )
  )

# display the new object (notice that the class of the `plot` list column is
# S3: ggExtraPlot)
plots

# creating a grid with cowplot
ggstatsplot::combine_plots(
  plotlist = plots$plot,                           # list column containing all ggstatsplot objects
  nrow = 3,
  ncol = 1,
  title.text = "Relationship between sepal length and width for each Iris species",
  title.size = 14,
  caption.text = expression(
    paste(
      italic("Note"),
      ": Iris flower dataset was collected by Edgar Anderson.",
      sep = ""
    ),
    caption.size = 12
  )
)

## ----ggscatterstats_plyr, message = FALSE, warning = FALSE, fig.height = 12, fig.width = 12----
library(plyr)
library(gapminder)

# for reproducibility
set.seed(123)

# let's have a look at the structure of the data
dplyr::glimpse(x = gapminder::gapminder)

# creating a list of plots
plots <- plyr::dlply(
  .data = dplyr::filter(gapminder::gapminder, 
                        year == 2007, continent != "Oceania"),
  .variables = .(continent),
  .fun = function(data)
    ggstatsplot::ggscatterstats(
      data = data,
      x = gdpPercap,
      y = lifeExp,
      xfill = "#0072B2",
      yfill = "#009E73",
      label.var = "country",
      label.expression = "lifeExp < 45",
      title = glue::glue("Continent: {data$continent}"),
      marginal = FALSE
    ) +
    ggplot2::scale_x_continuous(labels = scales::comma)
)

# combining individual plots
ggstatsplot::combine_plots(
  plotlist = plots,
  title.text = "Relationship between GDP (per capita) and life expectancy",
  nrow = 2,
  ncol = 2
)

## ----ggbetweenstats_subtext, message = FALSE, warning = FALSE, fig.height = 8, fig.width = 8----
library(ggstatsplot)

ggstatsplot::combine_plots(
  # preparing the plot with ggstatsplot function
  # let's use only 20% of data to speed up the process
  ggstatsplot::ggcoefstats(
    x = stats::lm(
      formula = rating ~ stats::poly(budget, degree = 3),
      data = dplyr::sample_frac(tbl = ggstatsplot::movies_long, size = 0.2),
      na.action = na.omit
    ),
    exclude.intercept = FALSE,
    title = "Relationship between movie budget and IMDB rating",
    subtitle = "Source: Internet Movie Database",
    ggtheme = ggplot2::theme_gray(),
    stats.label.color = c("#CC79A7", "darkgreen", "#0072B2", "red")
  ) +
    # modifying the plot outside of ggstatsplot using ggplot2 functions
    ggplot2::scale_y_discrete(
      labels = c(
        "Intercept (c)",
        "1st degree (b1)",
        "2nd degree (b2)",
        "3rd degree (b3)"
      )
    ) + 
    ggplot2::labs(y = "term (polynomial regression)"),
  # adding additional text element to the plot since title, subtitle, caption are all already occupied
  sub.text = expression(
    paste(
      "linear model: ", bolditalic(y),
      " ~ ",
      italic(c) + italic(b)[1] * bolditalic(x) + italic(b)[2] * bolditalic(x) ^
        2 + italic(b)[3] * bolditalic(x) ^ 3,
      sep = ""
    )
  ),
  sub.size = 12
)

## ----session_info, eval = TRUE------------------------------------------------------------------------------------------------------------------------------------------------------------------------
options(width = 200)
devtools::session_info()

