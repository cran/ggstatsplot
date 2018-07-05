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

## ----titanic2, warning = FALSE, message = FALSE--------------------------
library(ggstatsplot)

# looking at the final dataset
dplyr::glimpse(ggstatsplot::Titanic_full)

## ----ggpiestats1, warning = FALSE, message = FALSE, fig.height = 5, fig.width = 8----
# since effect size confidence intervals are computed using bootstrapping, let's
# set seed for reproducibility
set.seed(123)

ggstatsplot::ggpiestats(data = ggstatsplot::Titanic_full,
                        condition = Sex,
                        main = Survived) 

## ----ggpiestats2, warning = FALSE, message = FALSE, fig.height = 5, fig.width = 8----
library(ggplot2)

# for reproducibility
set.seed(123)

ggstatsplot::ggpiestats(
  data = ggstatsplot::Titanic_full,             # dataframe (matrix or table will not be accepted)
  main = Survived,                              # rows in the contingency table
  condition = Sex,                              # columns in the contingecy table
  title = "Passengar survival by gender",       # title for the entire plot
  caption = "Source: Titanic survival dataset", # caption for the entire plot
  legend.title = "Survived?",                   # legend title
  facet.wrap.name = "Gender",                   # changing the facet wrap title
  facet.proptest = TRUE,                        # proportion test for each facet
  stat.title = "survival x gender: ",           # title for statistical test
  palette = "Set1",                             # changing the color palette
  ggtheme = ggplot2::theme_classic()            # changing plot theme 
) +                                             # further modification with ggplot2 commands
  ggplot2::theme(plot.subtitle = ggplot2::element_text(
    color = "black",
    size = 10,
    face = "bold",
    hjust = 0.5
  ))

## ----ggpiestats3, warning = FALSE, message = FALSE, fig.height = 5, fig.width = 6----
ggstatsplot::ggpiestats(
  data = ggstatsplot::Titanic_full,                          
  main = Age
)

## ----ggpiestats4, warning = FALSE, message = FALSE, fig.height = 20, fig.width = 9----
ggstatsplot::grouped_ggpiestats(
  # arguments relevant for ggstatsplot::gghistostats
  data = ggstatsplot::Titanic_full,
  grouping.var = Class,
  title.prefix = "Passenger class",
  stat.title = "survival x gender: ",
  main = Survived,
  condition = Sex,
  # arguments relevant for ggstatsplot::combine_plots
  title.text = "Survival in Titanic disaster by gender for all passenger classes",
  caption.text = "Asterisks denote results from proportion tests; ***: p < 0.001, ns: non-significant",
  nrow = 4,
  ncol = 1,
  labels = c("(a)","(b)","(c)", "(d)")
)

## ----ggpiestats5, warning = FALSE, message = FALSE, fig.height = 10, fig.width = 10----
ggstatsplot::grouped_ggpiestats(
  data = ggstatsplot::Titanic_full,
  main = Age,
  grouping.var = Class,
  title.prefix = "Passenger Class"
) 

## ----ggpiestats6, warning = FALSE, message = FALSE, fig.height = 20, fig.width = 9----
# let's split the dataframe and create a list by passenger class
class_list <- ggstatsplot::Titanic_full %>%
  base::split(x = ., f = .$Class, drop = TRUE)

# this created a list with 4 elements, one for each class
# you can check the structure of the file for yourself
# str(class_list)

# checking the length and names of each element
length(class_list)
names(class_list)

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
    palette = list("Accent", "Paired", "Pastel1", "Set2"),
    ggtheme = list(
      ggplot2::theme_grey(),
      ggplot2::theme_classic(),
      ggplot2::theme_light(),
      ggplot2::theme_minimal()
    ),
    sample.size.label = list(TRUE, FALSE, TRUE, FALSE),
    messages = FALSE
  ),
  .f = ggstatsplot::ggpiestats
)
  
# combining all individual plots from the list into a single plot using combine_plots function
ggstatsplot::combine_plots(
  plotlist = plot_list,
  title.text = "Survival in Titanic disaster by gender for all passenger classes",
  caption.text = "Asterisks denote results from proportion tests; ***: p < 0.001, ns: non-significant",
  nrow = 4,
  ncol = 1,
  labels = c("(a)","(b)","(c)", "(d)")
)

## ----ggpiestats7, message = FALSE, warning = FALSE, fig.height = 8, fig.width = 9----
# for reproducibility
set.seed(123)

# creating a dataframe
# (this is completely fictional; I don't know first thing about fishing!)
(
  fishing <- data.frame(
    Boat = c(rep("B", 4), rep("A", 4), rep("A", 4), rep("B", 4)),
    Month = c(rep("February", 2), rep("March", 2), rep("February", 2), rep("March", 2)),
    Fish = c(
      "Bass",
      "Catfish",
      "Cod",
      "Haddock",
      "Cod",
      "Haddock",
      "Bass",
      "Catfish",
      "Bass",
      "Catfish",
      "Cod",
      "Haddock",
      "Cod",
      "Haddock",
      "Bass",
      "Catfish"
    ),
    SumOfCaught = c(25, 20, 35, 40, 40, 25, 30, 42, 40, 30, 33, 26, 100, 30, 20, 20)
  ) %>% # converting to a tibble dataframe
    tibble::as_data_frame(x = .)
)

# running `ggpiestats` with counts information
ggstatsplot::ggpiestats(
  data = fishing,
  main = Fish,
  condition = Month,
  counts = SumOfCaught
)


## ----ggpiestats8, message = FALSE, warning = FALSE, fig.height = 12, fig.width = 8----
# running the grouped variant of the function
ggstatsplot::grouped_ggpiestats(
  data = fishing,
  main = Fish,
  condition = Month,
  counts = SumOfCaught,
  grouping.var = Boat,
  title.prefix = "Boat",
  nrow = 2
)

