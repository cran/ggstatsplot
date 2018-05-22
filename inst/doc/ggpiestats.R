## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----titanic1, warning = FALSE, message = FALSE--------------------------
library(datasets)
library(dplyr)

# looking at the table
dplyr::glimpse(x = Titanic)

# converting to tibble
tibble::as_data_frame(x = Titanic)

## ----titanic2, warning = FALSE, message = FALSE--------------------------

# a custom function to repeat dataframe `rep` number of times, which is going to
# be count data for us
rep_df <- function(df, rep) {
  df[rep(1:nrow(df), rep), ]
}

# converting dataframe to full length based on count information
Titanic_full <- tibble::as_data_frame(datasets::Titanic) %>%
  tibble::rowid_to_column(df = ., var = "id") %>%
  dplyr::mutate_at(
    .tbl = .,
    .vars = dplyr::vars("id"),
    .funs = ~ as.factor(.)
  ) %>%
  base::split(x = ., f = .$id) %>%
  purrr::map_dfr(.x = ., .f = ~ rep_df(df = ., rep = .$n)) %>%
  dplyr::mutate_at(
    .tbl = .,
    .vars = dplyr::vars("id"),
    .funs = ~ as.numeric(as.character(.))
  ) %>%
  dplyr::mutate_if(
    .tbl = .,
    .predicate = is.character,
    .funs = ~ base::as.factor(.)
  ) %>%
  dplyr::mutate_if(
    .tbl = .,
    .predicate = is.factor,
    .funs = ~ base::droplevels(.)
  ) %>%
  dplyr::arrange(.data = ., id)

# reordering the Class variables
Titanic_full$Class <-
  base::factor(x = Titanic_full$Class,
  levels = c("1st", "2nd", "3rd", "Crew", ordered = TRUE))

# looking at the final dataset
dplyr::glimpse(Titanic_full)

## ----ggpiestats1, cache.extra = Titanic_full, warning = FALSE, message = FALSE, fig.height = 5, fig.width = 8----
ggstatsplot::ggpiestats(data = Titanic_full,
                        condition = Sex,
                        main = Survived) 

## ----ggpiestats2, cache.extra = Titanic_full, warning = FALSE, message = FALSE, fig.height = 5, fig.width = 8----
library(ggstatsplot)

ggstatsplot::ggpiestats(
  data = Titanic_full,                          # dataframe
  main = Survived,                              # rows in the contingency table
  condition = Sex,                              # columns in the contingecy table
  title = "Passengar survival by gender",       # title for the entire plot
  caption = "Source: Titanic survival dataset", # caption for the entire plot
  legend.title = "Survived?",                   # legend title
  facet.wrap.name = "Gender",                   # changing the facet wrap title
  facet.proptest = TRUE,                        # proportion test for each facet
  stat.title = "survival x gender"              # title for statistical test
) +                                             # further modification outside of ggstatsplot
  ggplot2::scale_fill_brewer(palette = "Dark2")

## ----ggpiestats3, cache.extra = Titanic_full, warning = FALSE, message = FALSE, fig.height = 5, fig.width = 6----
library(ggstatsplot)

ggstatsplot::ggpiestats(
  data = Titanic_full,                          
  main = Age
  ) + 
  ggplot2::scale_fill_brewer(palette = "Set2")

## ----ggpiestats4, cache.extra = Titanic_full, warning = FALSE, message = FALSE, fig.height = 20, fig.width = 9----
library(ggstatsplot)

ggstatsplot::grouped_ggpiestats(
  # arguments relevant for ggstatsplot::gghistostats
  data = Titanic_full,
  grouping.var = Class,
  title.prefix = "Passenger class",
  stat.title = "survival x gender",
  main = Survived,
  condition = Sex,
  # arguments relevant for ggstatsplot::combine_plots
  title.text = "Survival in Titanic disaster by gender for all passenger classes",
  caption.text = "Asterisks denote results from proportion tests; ***: p < 0.001, ns: non-significant",
  nrow = 4,
  ncol = 1,
  labels = c("(a)","(b)","(c)", "(d)")
)

## ----ggpiestats5, cache.extra = Titanic_full, warning = FALSE, message = FALSE, fig.height = 10, fig.width = 10----
library(ggstatsplot)

ggstatsplot::grouped_ggpiestats(
  data = Titanic_full,                          
  main = Age,
  grouping.var = Class
  ) 

## ----ggpiestats6, cache.extra = Titanic_full, warning = FALSE, message = FALSE, fig.height = 20, fig.width = 9----
# let's split the dataframe and create a list by passenger class
class_list <- Titanic_full %>%
  base::split(x = ., f = .$Class, drop = TRUE)

# this created a list with 4 elements, one for each class
str(class_list)

# running function on every element of this list note that if you want the same
# value for a given argument across all elements of the list, you need to
# specify it just once
plot_list <- purrr::pmap(
  .l = list(
    data = class_list,
    main = "Survived",
    condition = "Sex",
    facet.wrap.name = "Gender",
    title = list(
      "Passenger class: 1st",
      "Passenger class: 2nd",
      "Passenger class: 3rd",
      "Passenger class: Crew"
    ),
    caption = list(
      "Total: 319, Died: 120, Survived: 199, % Survived: 62%",
      "Total: 272, Died: 155, Survived: 117, % Survived: 43%",
      "Total: 709, Died: 537, Survived: 172, % Survived: 25%",
      "Not available"
    ),
    messages = FALSE
  ),
  .f = ggstatsplot::ggpiestats
)
  
# combining all individual plots from the list into a single plot using combine_plots function
ggstatsplot::combine_plots(
  plot_list$`1st` + ggplot2::scale_fill_brewer(palette = "Dark2"),
  plot_list$`2nd` + ggplot2::scale_fill_brewer(palette = "Dark2"),
  plot_list$`3rd` + ggplot2::scale_fill_manual(values = c("#D95F02", "#1B9E77")), # to be consistent with other legends
  plot_list$Crew + ggplot2::scale_fill_brewer(palette = "Dark2"),
  title.text = "Survival in Titanic disaster by gender for all passenger classes",
  caption.text = "Asterisks denote results from proportion tests; ***: p < 0.001, ns: non-significant",
  nrow = 4,
  ncol = 1,
  labels = c("(a)","(b)","(c)", "(d)")
)

