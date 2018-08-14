## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----ggscatterstats_purrr, message = FALSE, warning = FALSE, fig.height = 12, fig.width = 8----
library(glue)
library(dplyr)
library(ggplot2)

### creating a list column with `ggstatsplot` plots
plots <- datasets::iris %>%
  dplyr::mutate(.data = ., Species2 = Species) %>% # just creates a copy of this variable
  dplyr::group_by(.data = ., Species) %>%                
  tidyr::nest(data = .) %>%                        # creates a nested dataframe with list column called `data`
  dplyr::mutate(                                   # creating a new list column of ggstatsplot outputs
    .data = .,
    plot = data %>%
      purrr::map(
        .x = .,
        .f = ~ ggstatsplot::ggscatterstats(
          data = .,
          x = Sepal.Length,
          y = Sepal.Width,
          messages = FALSE,                        # turns off all the warnings, notes, and reference messages   
          marginal.type = "boxplot",
          title =
            glue::glue("Species: {.$Species2} (n = {length(.$Sepal.Length)})")
        )
      )
  )

### display the new object (notice that the class of the `plot` list column is S3: gg)
plots

### creating a grid with cowplot
ggstatsplot::combine_plots(
  plotlist = plots$plot,                           # list column containing all ggstatsplot objects
  labels = c("(a)", "(b)", "(c)"),
  nrow = 3,
  ncol = 1,
  title.text = "Relationship between sepal length and width for all Iris species",
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

## ----ggbetweenstats_purrr, message = FALSE, warning = FALSE, fig.height = 12, fig.width = 8----
library(dplyr)
library(glue)

### creating a list column with `ggstatsplot` plots
plots <- datasets::mtcars %>%
  dplyr::mutate(.data = ., cyl2 = cyl) %>%        # just creates a copy of this variable
  dplyr::group_by(.data = ., cyl) %>%             # 
  tidyr::nest(data = .) %>%                       # creates a nested dataframe with list column called `data`
  dplyr::mutate(                                  # creating a new list column of ggstatsplot outputs
    .data = .,
    plot = data %>%
      purrr::map(
        .x = .,
        .f = ~ ggstatsplot::ggbetweenstats(
          data = .,
          x = am,
          y = mpg,
          plot.type = "box",                      # type of plot needed
          messages = FALSE,                       # turns off all the warnings, notes, and reference messages
          xlab = "Transmission",
          ylab = "Miles/(US) gallon",
          title = glue::glue(
            "Number of cylinders: {.$cyl2}"       # this is where the duplicated cyl2 column is useful
            ) 
        )
      )
  )

### display the new object (notice that the class of the `plot` list column is S3: gg)
plots

### creating a grid with cowplot
ggstatsplot::combine_plots(
  plotlist = plots$plot,       # list column containing all ggstatsplot objects
  nrow = 3,
  ncol = 1,
  labels = c("(a)","(b)","(c)"),
  title.text = "MPG and car transmission relationship (for each cylinder count)",
  title.size = 13,
  title.color = "blue",
  caption.text = expression(
    paste(
      italic("Transmission"),
      ": 0 = automatic, 1 = manual",
      sep = ""
    ),
    caption.size = 10,
    caption.color = "red"
  )
)

## ----ggbetweenstats_plyr1, message = FALSE, warning = FALSE, fig.height = 12, fig.width = 8----
library(plyr)
library(ggstatsplot)

# for reproducibility
set.seed(123)

# let's have a look at the structure of the data
dplyr::glimpse(x = ggstatsplot::movies_long)

# creating a list of plots
plots <- plyr::dlply(
  .data = ggstatsplot::movies_long,
  .variables = .(genre),
  .fun = function(data)
    ggstatsplot::ggpiestats(
      data = data,
      main = mpaa,
      title = glue::glue("Genre: {data$genre}")
    )
)

# combining individual plots
ggstatsplot::combine_plots(plotlist = plots, 
                           title.text = "Differences in MPAA ratings by movie genre",
                           title.color = "darkgreen",
                           caption.text = "MPAA: Motion Picture Association of America",
                           caption.color = "orange",
                           nrow = 3,
                           ncol = 2)

## ----ggbetweenstats_subtext, message = FALSE, warning = FALSE, fig.height = 8, fig.width = 8----
library(ggstatsplot)

ggstatsplot::combine_plots(
  # preparing the plot with ggstatsplot function
  ggstatsplot::ggcoefstats(
    x = stats::lm(
      formula = rating ~ stats::poly(budget, degree = 3),
      data = ggstatsplot::movies_long,
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

