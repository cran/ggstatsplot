
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ggstatsplot: `ggplot2` Based Plots with Statistical Details

[![CRAN\_Release\_Badge](http://www.r-pkg.org/badges/version-ago/ggstatsplot)](https://CRAN.R-project.org/package=ggstatsplot)
[![CRAN
Checks](https://cranchecks.info/badges/summary/ggstatsplot)](https://cran.r-project.org/web/checks/check_results_ggstatsplot.html)
[![packageversion](https://img.shields.io/badge/Package%20version-0.0.3.9000-orange.svg?style=flat-square)](commits/master)
[![Coverage
Status](https://img.shields.io/codecov/c/github/IndrajeetPatil/ggstatsplot/master.svg)](https://codecov.io/github/IndrajeetPatil/ggstatsplot?branch=master)
[![Daily downloads
badge](https://cranlogs.r-pkg.org/badges/last-day/ggstatsplot?color=blue)](https://CRAN.R-project.org/package=ggstatsplot)
[![Weekly downloads
badge](https://cranlogs.r-pkg.org/badges/last-week/ggstatsplot?color=blue)](https://CRAN.R-project.org/package=ggstatsplot)
[![Monthly downloads
badge](https://cranlogs.r-pkg.org/badges/last-month/ggstatsplot?color=blue)](https://CRAN.R-project.org/package=ggstatsplot)
[![Total downloads
badge](https://cranlogs.r-pkg.org/badges/grand-total/ggstatsplot?color=blue)](https://CRAN.R-project.org/package=ggstatsplot)
[![Travis Build
Status](https://travis-ci.org/IndrajeetPatil/ggstatsplot.svg?branch=master)](https://travis-ci.org/IndrajeetPatil/ggstatsplot)
[![AppVeyor Build
Status](https://ci.appveyor.com/api/projects/status/github/IndrajeetPatil/ggstatsplot?branch=master&svg=true)](https://ci.appveyor.com/project/IndrajeetPatil/ggstatsplot)
[![Licence](https://img.shields.io/badge/licence-GPL--3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0.en.html)
[![Last-changedate](https://img.shields.io/badge/last%20change-2018--07--05-yellowgreen.svg)](/commits/master)
[![lifecycle](https://img.shields.io/badge/lifecycle-stable-green.svg)](https://www.tidyverse.org/lifecycle/#stable)
[![minimal R
version](https://img.shields.io/badge/R%3E%3D-3.3.0-6666ff.svg)](https://cran.r-project.org/)

<!-- 
[![Project Status: Active - The project has reached a stable, usable state and is being actively developed.](http://www.repostatus.org/badges/latest/active.svg)](http://www.repostatus.org/#active)
[![Open issues](http://img.shields.io/github/issues/badges/ggstatsplot.svg)](https://github.com/IndrajeetPatil/ggstatsplot/issues/)
[![Dependency Status](http://img.shields.io/gemnasium/IndrajeetPatil/ggstatsplot.svg)](https://gemnasium.com/IndrajeetPatil/ggstatsplot) 
-->

## Overview

[`ggstatsplot`](https://indrajeetpatil.github.io/ggstatsplot/) is an
extension of [`ggplot2`](https://github.com/tidyverse/ggplot2) package
for creating graphics with details from statistical tests included in
the plots themselves and targeted primarily at behavioral sciences
community to provide a one-line code to produce information-rich plots.
Currently, it supports only the most common types of statistical tests
(**parametric**, **nonparametric**, and **robust** versions of
**t-tets/anova**, **correlation**, and **contingency tables** analyses).

It, therefore, produces a limited kinds of plots for the supported
analyses:

  - **violin plots** (for comparisons between groups or conditions),
  - **pie charts** (for categorical data),
  - **scatterplots** (for correlations between two variables),
  - **correlation matrices** (for correlations between multiple
    variables),
  - **histograms** (for hypothesis about distributions), and
  - **dot-and-whisker plots** (for regression models).

In addition to these basic plots, `ggstatsplot` also provides `grouped_`
versions of all functions that makes it easy to repeat the same anlysis
for any grouping variable.

Future versions will include other types of analyses and plots as well.

## Installation

To get the latest, stable CRAN release:

``` r
utils::install.packages(pkgs = "ggstatsplot")
```

You can get the **development** version from GitHub. If you are in hurry
and want to reduce the time of installation, prefer-

``` r
# needed package to download from GitHub repo
utils::install.packages(pkgs = "devtools")   

# downloading the package from GitHub
devtools::install_github(
  repo = "IndrajeetPatil/ggstatsplot", # package path on GitHub
  dependencies = FALSE,                # assumes that you already have all packages installed needed for this package to work
  quick = TRUE                         # skips docs, demos, and vignettes
)                        
```

If time is not a constraint-

``` r
devtools::install_github(
  repo = "IndrajeetPatil/ggstatsplot", # package path on GitHub
  dependencies = TRUE,                 # installs packages which ggstatsplot depends on
  upgrade_dependencies = TRUE          # updates any out of date dependencies
)
```

If you are not using the [RStudio IDE](https://www.rstudio.com/) and you
get an error related to “pandoc” you will either need to remove the
argument `build_vignettes = TRUE` (to avoid building the vignettes) or
install [pandoc](http://pandoc.org/). If you have the `rmarkdown` R
package installed then you can check if you have pandoc by running the
following in R:

``` r
rmarkdown::pandoc_available()
#> [1] TRUE
```

## Citation

If you want to cite this package in a scientific journal or in any other
context, run the following code in your R console:

``` r
utils::citation(package = "ggstatsplot")
```

## Help

There is a dedicated website to `ggstatplot`, which is updated after
every new commit: <https://indrajeetpatil.github.io/ggstatsplot/>.

In `R`, documentation for any function can be accessed with the standard
`help` command-

``` r
?ggbetweenstats
?ggscatterstats
?gghistostats
?ggpiestats
?ggcorrmat
?ggcoefstats
?combine_plots
?grouped_ggbetweenstats
?grouped_ggscatterstats
?grouped_gghistostats
?grouped_ggpiestats
?grouped_ggcorrmat
```

## Usage

`ggstatsplot` relies on [non-standard
evaluation](http://adv-r.had.co.nz/Computing-on-the-language.html),
which means you **shouldn’t** enter arguments in the following manner:
`data = NULL, x = data$x, y = data$y`. You **must** always specify the
`data` argument for all functions.

Additionally, `ggstatsplot` is a very chatty package and will by default
output information about references for tests, notes on assumptions
about linear models, and warnings. If you don’t want your console to be
cluttered with such messages, they can be turned off by setting argument
`messages = FALSE` in the function call.

Here are examples of the main functions currently supported in
`ggstatsplot`. **Note**: The documentation below is for the
**development** version of the package. So you may see some features
available here that are not currently present in the stable version of
this package on **CRAN**:
<https://cran.r-project.org/web/packages/ggstatsplot/index.html>

  - `ggbetweenstats`

This function creates either a violin plot, a box plot, or a mix of two
for **between**-group or **between**-condition comparisons with results
from statistical tests in the subtitle. The simplest function call looks
like this-

``` r
# for reproducibility
set.seed(123)

# plot
ggstatsplot::ggbetweenstats(
  data = datasets::iris, 
  x = Species, 
  y = Sepal.Length,
  messages = FALSE
)
```

<img src="man/figures/README-ggbetweenstats1-1.png" width="100%" />

Number of other arguments can be specified to make this plot even more
informative and, additionally, this function returns a `ggplot2` object
and thus any of the graphics layers can be further modified:

``` r
library(ggplot2)

# for reproducibility
set.seed(123)

# plot
ggstatsplot::ggbetweenstats(
  data = datasets::iris,
  x = Species,
  y = Sepal.Length,
  notch = TRUE,                                   # show notched box plot
  mean.plotting = TRUE,                           # whether mean for each group id to be displayed 
  type = "parametric",                            # which type of test is to be run
  outlier.tagging = TRUE,                         # whether outliers need to be tagged
  outlier.label = Sepal.Width,                    # variable to be used for the outlier tag
  xlab = "Type of Species",                       # label for the x-axis variable
  ylab = "Attribute: Sepal Length",               # label for the y-axis variable
  title = "Dataset: Iris flower data set",        # title text for the plot
  caption = expression(                           # caption text for the plot 
    paste(italic("Note"), ": this is a demo")
  ),
  ggtheme = ggplot2::theme_grey(),                # choosing a different theme
  palette = "Set1",                               # choosing a different color palette
  messages = FALSE
) +                                               # further modification outside of ggstatsplot
  ggplot2::coord_cartesian(ylim = c(3, 8)) + 
  ggplot2::scale_y_continuous(breaks = seq(3, 8, by = 1)) 
```

<img src="man/figures/README-ggbetweenstats2-1.png" width="100%" />

The `type` (of test) argument also accepts the following abbreviations:
`"p"` (for *parametric*), `"np"` (for *nonparametric*), `"r"` (for
*robust*). Additionally, the type of plot to be displayed can also be
modified (`"box"`, `"violin"`, or `"boxviolin"`).

Variant of this function `ggwithinstats` is currently under work. You
*can* still use this function just to prepare the **plot** for
exploratory data analysis, but the statistical details displayed in the
subtitle will be incorrect. You can remove them by adding `+
ggplot2::labs(subtitle = NULL)`.

For more, see the `ggbetweenstats` vignette:
<https://indrajeetpatil.github.io/ggstatsplot/articles/ggbetweenstats.html>

  - `ggscatterstats`

This function creates a scatterplot with marginal
histograms/boxplots/density/violin plots from  and results from
statistical tests in the subtitle:

``` r
ggstatsplot::ggscatterstats(
  data = datasets::iris, 
  x = Sepal.Length, 
  y = Petal.Length,
  title = "Dataset: Iris flower data set",
  messages = FALSE
)
```

<img src="man/figures/README-ggscatterstats1-1.png" width="100%" />

Number of other arguments can be specified to modify this basic plot-

``` r
library(datasets)

# for reproducibility
set.seed(123)

# plot
ggstatsplot::ggscatterstats(
  data = subset(datasets::iris, iris$Species == "setosa"),
  x = Sepal.Length,
  y = Petal.Length,
  type = "robust",                               # type of test that needs to be run
  xlab = "Attribute: Sepal Length",              # label for x axis
  ylab = "Attribute: Petal Length",              # label for y axis 
  line.color = "black",                         # changing regression line color line
  title = "Dataset: Iris flower data set",       # title text for the plot
  caption = expression(                          # caption text for the plot
    paste(italic("Note"), ": this is a demo")
  ),
  marginal.type = "density",                     # type of marginal distribution to be displayed
  xfill = "blue",                                # color fill for x-axis marginal distribution 
  yfill = "red",                                 # color fill for y-axis marginal distribution
  centrality.para = "median",                    # which type of central tendency lines are to be displayed  
  width.jitter = 0.2,                            # amount of horizontal jitter for data points
  height.jitter = 0.4,                           # amount of vertical jitter for data points
  messages = FALSE                               # turn off messages and notes
) 
```

<img src="man/figures/README-ggscatterstats2-1.png" width="100%" />

For more, see the `ggscatterstats` vignette:
<https://indrajeetpatil.github.io/ggstatsplot/articles/ggscatterstats.html>

  - `ggpiestats`

This function creates a pie chart for categorical variables with results
from contingency table analysis included in the subtitle of the plot. If
only one categorical variable is entered, results from one-sample
**proportion test** will be displayed as a subtitle.

``` r
# for reproducibility
set.seed(123)

# plot
ggstatsplot::ggpiestats(
  data = datasets::iris,
  main = Species,
  messages = FALSE
)
```

<img src="man/figures/README-ggpiestats1-1.png" width="100%" />

This function can also be used to study an interaction between two
categorical variables. Additionally, as with the other functions in
`ggstatsplot`, this function returns a `ggplot2` object and can further
be modified with `ggplot2` syntax (e.g., we can change the color palette
*after* `ggstatsplot` has produced the plot)-

``` r
library(ggplot2)

# for reproducibility
set.seed(123)

# plot
ggstatsplot::ggpiestats(
  data = datasets::mtcars,
  main = cyl,
  condition = am,
  title = "Dataset: Motor Trend Car Road Tests",      
  messages = FALSE
) + # further modification outside of ggstatsplot to change the default palette as an example 
  ggplot2::scale_fill_brewer(palette = "Set1")     
```

<img src="man/figures/README-ggpiestats2-1.png" width="100%" />

As with the other functions, this basic plot can further be modified
with additional arguments:

``` r
# for reproducibility
set.seed(123)

# plot
ggstatsplot::ggpiestats(
  data = datasets::mtcars,
  main = am,
  condition = cyl,
  title = "Dataset: Motor Trend Car Road Tests",      # title for the plot
  stat.title = "interaction: ",                       # title for the results from Pearson's chi-squared test
  legend.title = "Transmission",                      # title for the legend
  factor.levels = c("1 = manual", "0 = automatic"),   # renaming the factor level names for 'main' variable 
  facet.wrap.name = "No. of cylinders",               # name for the facetting variable
  facet.proptest = FALSE,                             # turning of facetted proportion test results
  caption = expression(                               # text for the caption
    paste(italic("Note"), ": this is a demo")
  ),
  messages = FALSE                                    # turn off messages and notes
) 
```

<img src="man/figures/README-ggpiestats3-1.png" width="100%" />

For more, including information about the variant of this function
`grouped_ggpiestats`, see the `ggpiestats` vignette:
<https://indrajeetpatil.github.io/ggstatsplot/articles/ggpiestats.html>

  - `gghistostats`

In case you would like to see the distribution of one variable and check
if it is significantly different from a specified value with a one
sample test, this function will let you do that.

``` r
library(datasets)

ggstatsplot::gghistostats(
  data = datasets::iris,
  x = Sepal.Length,
  title = "Distribution of Iris sepal length",
  type = "parametric",               # one sample t-test
  test.value = 3,                    # default value is 0
  centrality.para = "mean",          # which measure of central tendency is to be plotted
  centrality.color = "darkred",      # decides color of vertical line representing central tendency
  binwidth = 0.10,                   # binwidth value (needs to be toyed around with until you find the best one)
  messages = FALSE                   # turn off the messages
) 
```

<img src="man/figures/README-gghistostats1-1.png" width="100%" />

The `type` (of test) argument also accepts the following abbreviations:
`"p"` (for *parametric*) or `"np"` (for *nonparametric*) or `"bf"` (for
*Bayes Factor*).

``` r
ggstatsplot::gghistostats(
  data = NULL,
  title = "Distribution of variable x",
  x = stats::rnorm(n = 1000, mean = 0, sd = 1),
  test.value = 1,
  test.value.line = TRUE,
  test.value.color = "black",
  centrality.para = "mean",
  type = "bf",
  bf.prior = 0.8,
  messages = FALSE,
  caption = expression(                              
    paste(italic("Note"), ": black line - test value; blue line - observed mean")
  )
)
```

<img src="man/figures/README-gghistostats2-1.png" width="100%" />

As seen here, by default, Bayes Factor quantifies the support for the
alternative hypothesis (H1) over the null hypothesis (H0) (i.e., BF10 is
displayed).

For more, including information about the variant of this function
`grouped_gghistostats`, see the `gghistostats` vignette:
<https://indrajeetpatil.github.io/ggstatsplot/articles/gghistostats.html>

  - `ggcorrmat`

`ggcorrmat` makes correlalograms with minimal amount of code. Just
sticking to the defaults itself produces publication-ready correlation
matrices. (Wrapper around
[`ggcorrplot`](https://github.com/kassambara/ggcorrplot))

``` r
# as a default this function outputs a correlalogram plot
ggstatsplot::ggcorrmat(
  data = datasets::iris,
  corr.method = "spearman",                # correlation method
  sig.level = 0.005,                       # threshold of significance
  cor.vars = Sepal.Length:Petal.Width,     # a range of variables can be selected  
  cor.vars.names = c("Sepal Length", "Sepal Width", "Petal Length", "Petal Width"),
  title = "Correlalogram for length measures for Iris species",
  subtitle = "Iris dataset by Anderson",
  caption = expression(
    paste(
      italic("Note"),
      ": X denotes correlation non-significant at ",
      italic("p "),
      "< 0.005; adjusted alpha"
    )
  )
)
```

<img src="man/figures/README-ggcorrmat1-1.png" width="100%" />

Multiple arguments can be modified to change the appearance of the
correlation matrix.

Alternatively, you can use it just to get the correlation matrices and
their corresponding *p*-values (in a
[tibble](http://tibble.tidyverse.org/) format). This is especially
useful for robust correlation coefficient, which is not currently
supported in `ggcorrmat` plot.

``` r
# getting the correlation coefficient matrix
ggstatsplot::ggcorrmat(
  data = datasets::iris,
  cor.vars = Sepal.Length:Petal.Width,
  corr.method = "robust",
  output = "correlations",             # specifying the needed output
  digits = 3                           # number of digits to be dispayed for correlation coefficient
)
#> # A tibble: 4 x 5
#>   variable     Sepal.Length Sepal.Width Petal.Length Petal.Width
#>   <chr>               <dbl>       <dbl>        <dbl>       <dbl>
#> 1 Sepal.Length        1          -0.143        0.878       0.837
#> 2 Sepal.Width        -0.143       1           -0.426      -0.373
#> 3 Petal.Length        0.878      -0.426        1           0.966
#> 4 Petal.Width         0.837      -0.373        0.966       1

# getting the p-value matrix
ggstatsplot::ggcorrmat(
  data = datasets::iris,
  cor.vars = Sepal.Length:Petal.Width,
  corr.method = "robust",
  output = "p-values"
)
#> # A tibble: 4 x 5
#>   variable     Sepal.Length  Sepal.Width Petal.Length Petal.Width
#>   <chr>               <dbl>        <dbl>        <dbl>       <dbl>
#> 1 Sepal.Length       0      0.0818       0             0         
#> 2 Sepal.Width        0.0818 0            0.0000000529  0.00000252
#> 3 Petal.Length       0      0.0000000529 0             0         
#> 4 Petal.Width        0      0.00000252   0             0
```

For examples and more information, see the `ggcorrmat` vignette:
<https://indrajeetpatil.github.io/ggstatsplot/articles/ggcorrmat.html>

  - `ggcoefstats`

`ggcoefstats` creates a lot with the regression coefficients’ point
estimates as dots with confidence interval whiskers. This is a wrapper
function around `GGally::ggcoef`.

``` r
ggstatsplot::ggcoefstats(x = stats::lm(formula = mpg ~ am * cyl,
                                       data = mtcars)) 
```

<img src="man/figures/README-ggcoefstats1-1.png" width="80%" />

The basic can be further modified to one’s liking with additional
arguments:

``` r
ggstatsplot::ggcoefstats(
  x = stats::lm(formula = mpg ~ am * cyl,
                data = mtcars),
  point.color = "red",
  vline.color = "#CC79A7",
  vline.linetype = "dotdash",
  stats.label.size = 3.5,
  stats.label.color = c("#0072B2", "#D55E00", "darkgreen"),
  title = "Car performance predicted by transmission and cylinder count",
  subtitle = "Source: 1974 Motor Trend US magazine"
) +                                    
  # further modification with the ggplot2 commands
  # note the order in which the labels are entered
  ggplot2::scale_y_discrete(labels = c("transmission", "cylinders", "interaction")) +
  ggplot2::labs(x = "regression coefficient",
                y = NULL)
```

<img src="man/figures/README-ggcoefstats2-1.png" width="80%" />

All the regression model classes that are supported in the `broom`
package with `tidy` and `glance` methods
(<https://broom.tidyverse.org/articles/available-methods.html>) are also
supported by `ggcoefstats`. Let’s see few examples:

``` r
library(dplyr)
library(lme4)

# for reproducibility
set.seed(200)

# creating dataframes needed for the analysis below
d <- as.data.frame(Titanic)

# combining plots together
ggstatsplot::combine_plots(
  # generalized linear model
  ggstatsplot::ggcoefstats(
    x = stats::glm(
      formula = Survived ~ Sex + Age,
      data = d,
      weights = d$Freq,
      family = "binomial"
    ),
    exponentiate = TRUE,
    exclude.intercept = FALSE,
    title = "generalized linear model"
  ),
  # nonlinear least squares
  ggstatsplot::ggcoefstats(
    x = stats::nls(
      formula = mpg ~ k / wt + b,
      data = mtcars,
      start = list(k = 1, b = 0)
    ),
    point.color = "darkgreen",
    title = "non-linear least squares"
  ),
  # linear mmodel
  ggstatsplot::ggcoefstats(
    x = lme4::lmer(
      formula = Reaction ~ Days + (Days | Subject),
      data = lme4::sleepstudy
    ),
    point.color = "red",
    exclude.intercept = TRUE,
    title = "linear mixed-effects model"
  ),
  # generalized linear mixed-effects model
  ggstatsplot::ggcoefstats(
    x = lme4::glmer(
      formula = cbind(incidence, size - incidence) ~ period + (1 | herd),
      data = lme4::cbpp,
      family = binomial
    ),
    exclude.intercept = FALSE,
    title = "generalized linear mixed-effects model"
  ),
  labels = c("(a)", "(b)", "(c)", "(d)"),
  nrow = 2,
  ncol = 2
)
```

<img src="man/figures/README-ggcoefstats3-1.png" width="100%" />

This is by no means an exhaustive list of models supported by
`ggcoefstats`. For more, see the associated vignette-
<https://indrajeetpatil.github.io/ggstatsplot/articles/ggcoefstats.html>

  - `combine_plots`

`ggstatsplot` also contains a helper function `combine_plots` to combine
multiple plots. This is a wrapper around  and lets you combine multiple
plots and add combination of title, caption, and annotation texts with
suitable default parameters.

The full power of `ggstatsplot` can be leveraged with a functional
programming package like [`purrr`](http://purrr.tidyverse.org/) that
replaces many for loops with code that is both more succinct and easier
to read and, therefore, `purrr` should be preferrred.

For more, see the associated vignette-
<https://indrajeetpatil.github.io/ggstatsplot/articles/combine_plots.html>

  - `theme_mprl`

All plots from `ggstatsplot` have a default theme: `theme_mprl`. For
more on how to modify it, see the associated vignette-
<https://indrajeetpatil.github.io/ggstatsplot/articles/theme_mprl.html>
