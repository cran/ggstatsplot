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

## ----rlm, warning = FALSE, message = FALSE, fig.height = 8, fig.width = 8----
ggstatsplot::ggcoefstats(
  x = MASS::rlm(
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
    exclude.intercept = FALSE                       # show the intercept
  ) +
    ggplot2::labs(x = parse(text = "'standardized regression coefficient' ~italic(beta)")),
  # model 2: linear mixed-effects model
  ggstatsplot::ggcoefstats(
    x = lme4::lmer(
      formula = rating ~ budget + (budget | genre),
      data = ggstatsplot::movies_long,
      control = lme4::lmerControl(calc.derivs = FALSE)
    ),
    p.kr = TRUE,                                    # use Kenward-Roger approximation to compute p-values (fast)
    title = "linear mixed-effects model",
    exclude.intercept = FALSE                       # show the intercept
  ) +
    ggplot2::labs(x = parse(text = "'standardized regression coefficient' ~italic(beta)"), 
                  y = "fixed effects"),
  labels = c("(a)", "(b)"),
  nrow = 2,
  ncol = 1,
  title.text = "Relationship between movie budget and its IMDB rating"
)

## ----lmer2, warning = FALSE, message = FALSE-----------------------------
broom::tidy(
  x = lme4::lmer(
    formula = rating ~ budget + (budget | genre),
    data = ggstatsplot::movies_long,
    control = lme4::lmerControl(calc.derivs = FALSE)
  ),
  conf.int = TRUE,
  conf.level = 0.95
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
    family = stats::binomial(link = "logit")    # choosing the family
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

## ----glm2, warning = FALSE, message = FALSE, fig.height = 12, fig.width = 10----
# creating dataframes to use for regression analyses
library(dplyr)

# dataframe #1
(
  df.counts <-
    base::data.frame(
      treatment = gl(n = 3, k = 3, length = 9),
      outcome = gl(n = 3, k = 1, length = 9),
      counts = c(18, 17, 15, 20, 10, 20, 25, 13, 12)
    ) %>%
    tibble::as_data_frame(x = .)
)

# dataframe #2
(df.clotting <- data.frame(
  u = c(5, 10, 15, 20, 30, 40, 60, 80, 100),
  lot1 = c(118, 58, 42, 35, 27, 25, 21, 19, 18),
  lot2 = c(69, 35, 26, 21, 18, 16, 13, 12, 12)
) %>%
  tibble::as_data_frame(x = .))

# dataframe #3
x1 <- stats::rnorm(100)
y1 <- stats::rpois(n = 100, lambda = exp(1 + x1))
(df.3 <- data.frame(x = x1, y = y1) %>%
    tibble::as_data_frame(x = .))

# dataframe #4
x2 <- stats::rnorm(100)
y2 <- rbinom(n = 100,
             size = 1,
             prob = stats::plogis(x2))

(df.4 <- data.frame(x = x2, y = y2) %>%
    tibble::as_data_frame(x = .))

# combining all plots in a single plot
ggstatsplot::combine_plots(
  ggstatsplot::ggcoefstats(
    x = stats::glm(
      formula = counts ~ outcome + treatment,
      data = df.counts,
      family = stats::poisson(link = "log")
    ),
    title = "Family: Poisson"
  ),
  ggstatsplot::ggcoefstats(
    x = stats::glm(
      formula = lot1 ~ log(u),
      data = df.clotting,
      family = stats::Gamma(link = "inverse")
    ),
    title = "Family: Gamma"
  ),
  ggstatsplot::ggcoefstats(
    x = stats::glm(
      formula = y ~ x,
      family = quasi(variance = "mu", link = "log"),
      data = df.3
    ),
    title = "Family: Quasi"
  ),
  ggstatsplot::ggcoefstats(
    x = stats::glm(
      formula = y ~ x,
      family = stats::quasibinomial(link = "logit"),
      data = df.4
    ),
    title = "Family: Quasibinomial"
  ),
    ggstatsplot::ggcoefstats(
    x = stats::glm(
      formula = y ~ x,
      family = stats::quasipoisson(link = "log"),
      data = df.4
    ),
    title = "Family: Quasipoisson"
  ),
    ggstatsplot::ggcoefstats(
    x = stats::glm(
      formula = Sepal.Length ~ Species,
      family = stats::gaussian(link = "identity"),
      data = iris
    ),
    title = "Family: Gaussian"
  ),
  labels = c("(a)", "(b)", "(c)", "(d)", "(e)", "(f)"),
  ncol = 2,
  title.text = "Exploring models with different glm families"
)

## ----glmer, warning = FALSE, message = FALSE, fig.height = 6, fig.width = 6----
# plot
ggstatsplot::ggcoefstats(
  x = lme4::glmer(
    formula = Survived ~ Sex + Age + (Sex + Age | Class),
    data = ggstatsplot::Titanic_full,
    family = stats::binomial(link = "logit"),
    control = lme4::glmerControl(
      optimizer = "Nelder_Mead",
      calc.derivs = FALSE,
      boundary.tol = 1e-7
    )
  ),
  exponentiate = TRUE
)

## ----clm, warning = FALSE, message = FALSE, fig.height = 12, fig.width = 12----
library(ggstatsplot)
library(purrr)
library(glue)

# running the function for each type of harm and creating a list of plots
# to speed up calculations, we will use just half of the dataset
plotlist <- dplyr::filter(.data = ggstatsplot::intent_morality, id <= 250) %>%
  dplyr::mutate(.data = ., plot.title = harm) %>%
  base::split(x = ., f = .$harm) %>%
  purrr::map(
    .x = .,
    .f = ~ ggstatsplot::ggcoefstats(
      x = ordinal::clm(
        formula = as.factor(rating) ~ belief * outcome,
        link = "logit",
        data = .,
        control = ordinal::clm.control(
          maxIter = 50,
          convergence = "silent"
        ),
      ),
      title = glue::glue("Type of harm: {.$plot.title}"),
      caption.summary = FALSE                            # suppress model diagnostics
    ) +
      #ggplot2::scale_y_discrete(labels = c("belief (neutral)", "interaction", "outcome (neutral)")) +
      ggplot2::labs(x = "logit regression coefficient",
                    y = NULL)
  )

# combining the plots
ggstatsplot::combine_plots(plotlist = plotlist)


## ----clmm, warning = FALSE, message = FALSE, fig.height = 6, fig.width = 6----
library(ggstatsplot)

ggstatsplot::ggcoefstats(
  x = ordinal::clmm(
    formula = as.factor(rating) ~ belief * outcome + (belief + outcome |
                                                        harm),
    # to speed up calculations, we will use just half of the dataset
    data = dplyr::filter(.data = ggstatsplot::intent_morality, id <= 250),
    control = ordinal::clmm.control(
      method = "nlminb",
      maxIter = 50,
      gradTol = 1e-4,
      innerCtrl = "noWarn"
    )
  ),
  # suppress model diagnostics
  caption.summary = FALSE
) +
  #ggplot2::scale_y_discrete(labels = c("belief (neutral)", "interaction", "outcome (neutral)")) +
  ggplot2::labs(x = "coefficient from ordinal mixed-effects regression",
                y = "fixed effects")

## ----clmm1, warning = FALSE, message = FALSE, fig.height = 6, fig.width = 6----
library(ggstatsplot)

# to speed up calculations, we will use just half of the dataset
ggstatsplot::ggcoefstats(
  x = ordinal::clmm(
    formula = as.factor(rating) ~ belief * outcome + (belief + outcome |
                                                        harm),
    link = "logit",
    data = dplyr::filter(.data = ggstatsplot::intent_morality, id <= 250),
    control = ordinal::clmm.control(
      maxIter = 50,
      gradTol = 1e-4,
      innerCtrl = "noWarn"
    )
  ),
  coefficient.type = "alpha"
) +
  ggplot2::labs(x = "logit regression coefficients",
                y = "threshold parameters")

## ----aovlist, warning = FALSE, message = FALSE, fig.height = 6, fig.width = 8----
library(ggstatsplot)
library(ggplot2)

# for reproducibility
set.seed(123)

# having a look at iris before converting to long format
dplyr::glimpse(iris)

# converting the iris dataset to long format
iris_long <- datasets::iris %>%
  dplyr::mutate(.data = ., id = dplyr::row_number(x = Species)) %>%
  tidyr::gather(
    data = .,
    key = "condition",
    value = "value",
    Sepal.Length:Petal.Width,
    convert = TRUE,
    factor_key = TRUE
  ) %>%
  tidyr::separate(
    col = "condition",
    into = c("attribute", "measure"),
    sep = "\\.",
    convert = TRUE
  ) %>%
  tibble::as_data_frame(x = .)

# looking at the long format data
dplyr::glimpse(x = iris_long)

# specifying the model (note the error structure)
ggstatsplot::ggcoefstats(
  x = stats::aov(formula = value ~ attribute * measure + Error(id / (attribute * measure)),
             data = iris_long),
  effsize = "omega",
  ggtheme = ggplot2::theme_grey(),
  stats.label.color = c("#0072B2", "#D55E00", "darkgreen"),
  title = "Variation in measurements for Iris species",
  subtitle = "Source: Iris data set (by Fisher or Anderson)"
) + 
  ggplot2::labs(caption = "Results from 2 by 2 RM ANOVA") + 
  ggplot2::theme(plot.subtitle = element_text(size = 11, face = "plain"))

## ----robust, warning = FALSE, message = FALSE, fig.height = 12, fig.width = 8----
ggstatsplot::combine_plots(
  ggstatsplot::ggcoefstats(
    x = robust::glmRob(
      formula = Survived ~ Sex,
      data = ggstatsplot::Titanic_full,
      family = stats::binomial(link = "logit")
    ),
    title = "generalized robust linear model"
  ),
  ggstatsplot::ggcoefstats(
    x = robust::lmRob(formula = Sepal.Length ~ Sepal.Width * Species, data = iris),
    title = "robust linear model"
  ),
  labels = c("(a)", "(b)"),
  title.text = "Robust variants of lm and glm",
  nrow = 2,
  ncol = 1
)

