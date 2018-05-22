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
  title.color = "black",
  caption.text = expression(
    paste(
      italic("Note"),
      ": Iris flower dataset was collected by Edgar Anderson."
    ),
    caption.size = 10
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
  title.color = "black",
  caption.text = expression(
    paste(
      italic("Transmission"),
      ": 0 = automatic, 1 = manual"
    ),
    caption.size = 10
  )
)

