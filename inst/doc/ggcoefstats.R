## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----aov, warning = FALSE, message = FALSE, fig.height = 6, fig.width = 8----
# for reproducibility
set.seed(123)

# looking at the data
dplyr::glimpse(x = ggstatsplot::movies_long)

# plot
ggstatsplot::ggcoefstats(
  x = stats::aov(
    formula = rating ~ mpaa * genre,
    data = ggstatsplot::movies_long
  ),
  effsize = "omega",                        # changing the effect size estimate being displayed
  point.color = "red",                      # changing the point color
  point.size = 4,                           # changing the point size
  point.shape = 15,                         # changing the point shape
  title = "omnibus ANOVA"
) +                                    
  # further modification with the ggplot2 commands
  # note the order in which the labels are entered
  ggplot2::scale_y_discrete(labels = c("MPAA", "Genre", "Interaction term")) +
  ggplot2::labs(x = "effect size estimate (partial omega-squared)",
                y = NULL)

## ----aov2, warning = FALSE, message = FALSE, fig.height = 10, fig.width = 10----
library(ggstatsplot)

# for reproducibility
set.seed(123)

# plot
ggstatsplot::combine_plots(
  # model 1
  ggstatsplot::ggcoefstats(
    x = stats::aov(formula = rating ~ mpaa,
                   data = ggstatsplot::movies_long),
    title = "1. Only MPAA ratings"
  ),
  ggstatsplot::ggcoefstats(
    x = stats::aov(formula = rating ~ genre,
                   data = ggstatsplot::movies_long),
    title = "2. Only genre"
  ),
  ggstatsplot::ggcoefstats(
    x = stats::aov(formula = rating ~ mpaa + genre,
                   data = ggstatsplot::movies_long),
    title = "3. Additive effect of MPAA and genre"
  ),
  ggstatsplot::ggcoefstats(
    x = stats::aov(formula = rating ~ mpaa * genre,
                   data = ggstatsplot::movies_long),
    title = "4. Multiplicative effect of MPAA and genre"
  ),
  title.text = "Model selection using ggcoefstats",
  labels = c("(a)", "(b)", "(c)", "(d)")
)

## ----lm, warning = FALSE, message = FALSE, fig.height = 8, fig.width = 8----
# let's check all the levels for the genre variable
levels(ggstatsplot::movies_long$genre)

# plot
ggstatsplot::ggcoefstats(
  x = stats::lm(
    formula = rating ~ genre,
    data = ggstatsplot::movies_long
  ),
  conf.level = 0.99,                      # changing the confidence levels for confidence intervals
  sort = "ascending",                     # sorting the terms of the model based on estimate values
  label.direction = "both",               # direction in which to adjust position of labels (both x and y)
  ggtheme = ggplot2::theme_gray(),        # changing the default theme
  stats.label.color = c("#CC79A7", "darkgreen", "#0072B2", "black", "red"),
  title = "Movie ratings by their genre",
  subtitle = "Source: www.imdb.com"
) +                                    
  # further modification with the ggplot2 commands
  # note the order in which the labels are entered
  ggplot2::scale_y_discrete(labels = c("Comedy", "Romance", "Documentary", "Animation", "Drama")) +
  ggplot2::labs(y = "genre (comparison level: Action)") + 
  ggplot2::theme(axis.title.y = ggplot2::element_text(size = 14, face = "bold"))

## ----lmer, warning = FALSE, message = FALSE, fig.height = 14, fig.width = 7----
library(lme4)
library(ggstatsplot)

# combining the two different plots
ggstatsplot::combine_plots(
  # model 1: simple linear model
  ggstatsplot::ggcoefstats(
    x = stats::lm(
      formula = scale(rating) ~ scale(budget),
      data = ggstatsplot::movies_long
    ),
    title = "linear model",
    exclude.intercept = FALSE         # show the intercept
  ) +
    ggplot2::labs(x = parse(text = "'standardized regression coefficient' ~italic(beta)")),
  # model 2: linear mixed-effects model
  ggstatsplot::ggcoefstats(
    x = lme4::lmer(
      formula = rating ~ budget + (budget | genre),
      data = ggstatsplot::movies_long,
      control = lme4::lmerControl(calc.derivs = FALSE)
    ),
    title = "linear mixed-effects model",
    exclude.intercept = FALSE,       # show the intercept
    effects = "fixed"   # show both fixed and random effects
  ) +
    ggplot2::labs(x = parse(text = "'standardized regression coefficient' ~italic(beta)"), y = "fixed effects"),
  labels = c("(a)", "(b)"),
  nrow = 2,
  ncol = 1,
  title.text = "Relationship between movie budget and its IMDB rating"
)

## ----nls, warning = FALSE, message = FALSE, fig.height = 6, fig.width = 6----
library(ggstatsplot)

# plot
ggstatsplot::ggcoefstats(
    x = stats::nls(
      formula = rating ~ k / budget + c,                # try toying around with the form of non-linear function
      data = ggstatsplot::movies_long,
      start = list(k = 1, c = 0)
    ),
    title = "Non-linear relationship between budget and rating",
    subtitle = "Source: IMDB"
  )

## ----glm, warning = FALSE, message = FALSE, fig.height = 6, fig.width = 6----
library(ggstatsplot)

# having a look at the Titanic dataset
df <- as.data.frame(x = Titanic)
str(df)

# plot
ggstatsplot::ggcoefstats(
  x = stats::glm(
    formula = Survived ~ Sex + Age,
    data = df,
    weights = df$Freq,                          # vector containing weights (no. of observations per row)
    family = "binomial"
  ),
  exponentiate = TRUE,
  ggtheme = ggplot2::theme_dark(),
  vline.color = "red",
  vline.linetype = "solid",
  label.segment.color = "red",
  stats.label.size = 3.5,
  stats.label.color = c("orangered",
                        "dodgerblue")
) 

## ----glmer, warning = FALSE, message = FALSE, fig.height = 6, fig.width = 6----
# plot
ggstatsplot::ggcoefstats(
  x = lme4::glmer(
    formula = Survived ~ Sex + Age + (Sex + Age | Class),
    data = df,
    weights = df$Freq,
    # vector containing weights (no. of observations per row)
    family = "binomial",
    control = lme4::glmerControl(
      optimizer = "Nelder_Mead",
      calc.derivs = FALSE,
      boundary.tol = 1e-7
    )
  ),
  exponentiate = TRUE
)

