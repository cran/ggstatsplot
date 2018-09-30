## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----titanic1, warning = FALSE, message = FALSE--------------------------
library(datasets)
library(dplyr)
library(ggstatsplot)

# looking at the original data in tabular format
dplyr::glimpse(x = Titanic)

# looking at the dataset as a tibble or dataframe
dplyr::glimpse(x = ggstatsplot::Titanic_full)

## ----ggpiestats3, warning = FALSE, message = FALSE, fig.height = 5, fig.width = 6----
# since effect size confidence intervals are computed using bootstrapping, let's
# set seed for reproducibility
set.seed(123)

# to speed up the process, let's use only half of the dataset
Titanic_full_50 <- dplyr::sample_frac(tbl = ggstatsplot::Titanic_full, size = 0.5)

# plot
ggstatsplot::ggpiestats(
  data = Titanic_full_50,                          
  main = Survived,
  title = "Passenger survival on the Titanic",       # title for the entire plot
  caption = "Source: Titanic survival dataset",      # caption for the entire plot
  legend.title = "Survived?",                        # legend title
  messages = FALSE                                   # turn off messages
)

## ----ggpiestats1, warning = FALSE, message = FALSE, fig.height = 5, fig.width = 8----
# since effect size confidence intervals are computed using bootstrapping, let's
# set seed for reproducibility
set.seed(123)

# to speed up the process, let's use only half of the dataset
Titanic_full_50 <- dplyr::sample_frac(tbl = ggstatsplot::Titanic_full, size = 0.5)

# plot
ggstatsplot::ggpiestats(
  data = Titanic_full_50,
  main = Survived,
  condition = Sex,
  title = "Passenger survival on the Titanic by gender",   # title for the entire plot
  caption = "Source: Titanic survival dataset",            # caption for the entire plot
  legend.title = "Survived?",                              # legend title
  ggtheme = ggplot2::theme_grey(),                         # changing plot theme 
  palette = "category10_d3",                               # choosing a different color palette
  package = "ggsci",                                       # package to which color palette belongs
  stat.title = "Pearson's chi-squared test: ",             # title for statistical test
  k = 2,                                                   # decimal places in result
  perc.k = 1,                                              # decimal places in percentage labels
  nboot = 10,                                              # no. of bootstrap sample for effect size CI
  messages = FALSE
)  +                                                       # further modification with `ggplot2` commands
  ggplot2::theme(plot.title = ggplot2::element_text(
    color = "black",
    size = 14,
    hjust = 0
  ))

## ----ggpiestats4, warning = FALSE, message = FALSE, fig.height = 10, fig.width = 8----
# since effect size confidence intervals are computed using bootstrapping, let's
# set seed for reproducibility
set.seed(123)

# plot
ggstatsplot::grouped_ggpiestats(
  # arguments relevant for ggstatsplot::gghistostats
  data = ggstatsplot::Titanic_full,
  grouping.var = Age,
  title.prefix = "Child or Adult?: ",
  stat.title = "Pearson's chi-squared test: ",
  main = Survived,
  condition = Sex,
  nboot = 10,
  k = 2,
  perc.k = 1,
  package = "ggsci",
  palette = "category10_d3",
  messages = FALSE,
  # arguments relevant for ggstatsplot::combine_plots
  title.text = "Passenger survival on the Titanic by gender and age",
  caption.text = "Asterisks denote results from proportion tests; \n***: p < 0.001, ns: non-significant",
  nrow = 2,
  ncol = 1
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

## ----ggpiestats8, message = FALSE, warning = FALSE, fig.height = 5, fig.width = 8----
# running `ggpiestats` with counts information
ggstatsplot::ggpiestats(
  data = fishing,
  main = Fish,
  condition = Month,
  counts = SumOfCaught,
  package = "ggsci",
  palette = "default_jama",
  title = "Type fish caught by month",
  caption = "Source: completely made up",
  legend.title = "Type fish caught: ",
  messages = FALSE
)

## ----ggpiestats9, warning = FALSE, message = FALSE, fig.height = 5, fig.width = 8----
# seed for reproducibility
set.seed(123)

# create our imaginary data
clinical_trial <- 
  tibble::tribble(
    ~SickBefore,	~SickAfter,	~Counts,
    "No",	"Yes",	4,
    "Yes",	"No",	25,
    "Yes",	"Yes",	13,
    "No",	"No",	92
  )

# display it as a simple table
stats::xtabs(
  formula = Counts ~ SickBefore + SickAfter,
  subset = NULL,
  data = clinical_trial
)

# plot
ggstatsplot::ggpiestats(
  data = clinical_trial,
  condition = SickBefore,
  main = SickAfter,
  counts = Counts,
  paired = TRUE,
  stat.title = "McNemar test: ",
  title = "Results from imaginary clinical trial",
  package = "ggsci",
  palette = "default_ucscgb",
  direction = -1,
  messages = FALSE
) 

## ----session_info-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
options(width = 200)
devtools::session_info()

