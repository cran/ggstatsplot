% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gghistostats.R
\name{gghistostats}
\alias{gghistostats}
\title{Histogram for distribution of a numeric variable}
\usage{
gghistostats(
  data,
  x,
  binwidth = NULL,
  xlab = NULL,
  title = NULL,
  subtitle = NULL,
  caption = NULL,
  type = "parametric",
  test.value = 0,
  bf.prior = 0.707,
  bf.message = TRUE,
  effsize.type = "g",
  conf.level = 0.95,
  tr = 0.2,
  k = 2L,
  ggtheme = ggstatsplot::theme_ggstatsplot(),
  results.subtitle = TRUE,
  bin.args = list(color = "black", fill = "grey50", alpha = 0.7),
  centrality.plotting = TRUE,
  centrality.type = type,
  centrality.line.args = list(color = "blue", size = 1, linetype = "dashed"),
  normal.curve = FALSE,
  normal.curve.args = list(size = 2),
  ggplot.component = NULL,
  output = "plot",
  ...
)
}
\arguments{
\item{data}{A dataframe (or a tibble) from which variables specified are to
be taken. Other data types (e.g., matrix,table, array, etc.) will \strong{not}
be accepted.}

\item{x}{A numeric variable from the dataframe \code{data}.}

\item{binwidth}{The width of the histogram bins. Can be specified as a
numeric value, or a function that calculates width from \code{x}. The default is
to use the \code{max(x) - min(x) / sqrt(N)}. You should always check this value
and explore multiple widths to find the best to illustrate the stories in
your data.}

\item{xlab}{Labels for \code{x} and \code{y} axis variables. If \code{NULL} (default),
variable names for \code{x} and \code{y} will be used.}

\item{title}{The text for the plot title.}

\item{subtitle}{The text for the plot subtitle. Will work only if
\code{results.subtitle = FALSE}.}

\item{caption}{The text for the plot caption.}

\item{type}{A character specifying the type of statistical approach:
\itemize{
\item \code{"parametric"}
\item \code{"nonparametric"}
\item \code{"robust"}
\item \code{"bayes"}
}

You can specify just the initial letter.}

\item{test.value}{A number indicating the true value of the mean (Default:
\code{0}).}

\item{bf.prior}{A number between \code{0.5} and \code{2} (default \code{0.707}), the prior
width to use in calculating Bayes factors and posterior estimates. In
addition to numeric arguments, several named values are also recognized:
\code{"medium"}, \code{"wide"}, and \code{"ultrawide"}, corresponding to \emph{r} scale values
of 1/2, sqrt(2)/2, and 1, respectively. In case of an ANOVA, this value
corresponds to scale for fixed effects.}

\item{bf.message}{Logical that decides whether to display Bayes Factor in
favor of the \emph{null} hypothesis. This argument is relevant only \strong{for
parametric test} (Default: \code{TRUE}).}

\item{effsize.type}{Type of effect size needed for \emph{parametric} tests. The
argument can be \code{"d"} (for Cohen's \emph{d}) or \code{"g"} (for Hedge's \emph{g}).}

\item{conf.level}{Confidence/Credible Interval (CI) level. Default to \code{0.95}
(\verb{95\%}).}

\item{tr}{Trim level for the mean when carrying out \code{robust} tests. In case
of an error, try reducing the value of \code{tr}, which is by default set to
\code{0.2}. Lowering the value might help.}

\item{k}{Number of digits after decimal point (should be an integer)
(Default: \code{k = 2L}).}

\item{ggtheme}{A \code{ggplot2} theme. Default value is
\code{ggstatsplot::theme_ggstatsplot()}. Any of the \code{ggplot2} themes (e.g.,
\code{ggplot2::theme_bw()}), or themes from extension packages are allowed
(e.g., \code{ggthemes::theme_fivethirtyeight()}, \code{hrbrthemes::theme_ipsum_ps()},
etc.).}

\item{results.subtitle}{Decides whether the results of statistical tests are
to be displayed as a subtitle (Default: \code{TRUE}). If set to \code{FALSE}, only
the plot will be returned.}

\item{bin.args}{A list of additional aesthetic arguments to be passed to the
\code{stat_bin} used to display the bins. Do not specify \code{binwidth} argument in
this list since it has already been specified using the dedicated argument.}

\item{centrality.plotting}{Logical that decides whether centrality tendency
measure is to be displayed as a point with a label (Default: \code{TRUE}).
Function decides which central tendency measure to show depending on the
\code{type} argument.
\itemize{
\item \strong{mean} for parametric statistics
\item \strong{median} for non-parametric statistics
\item \strong{trimmed mean} for robust statistics
\item \strong{MAP estimator} for Bayesian statistics
}

If you want default centrality parameter, you can specify this using
\code{centrality.type} argument.}

\item{centrality.type}{Decides which centrality parameter is to be displayed.
The default is to choose the same as \code{type} argument. You can specify this
to be:
\itemize{
\item \code{"parameteric"} (for \strong{mean})
\item \code{"nonparametric"} (for \strong{median})
\item \code{robust} (for \strong{trimmed mean})
\item \code{bayes} (for \strong{MAP estimator})
}

Just as \code{type} argument, abbreviations are also accepted.}

\item{centrality.line.args}{A list of additional aesthetic arguments to be
passed to the \code{geom_line} used to display the lines corresponding to the
centrality parameter.}

\item{normal.curve}{A logical value that decides whether to super-impose a
normal curve using \code{stats::dnorm(mean(x), sd(x))}. Default is \code{FALSE}.}

\item{normal.curve.args}{A list of additional aesthetic arguments to be
passed to the normal curve.}

\item{ggplot.component}{A \code{ggplot} component to be added to the plot prepared
by \code{ggstatsplot}. This argument is primarily helpful for \code{grouped_}
variants of all primary functions. Default is \code{NULL}. The argument should
be entered as a \code{ggplot2} function or a list of \code{ggplot2} functions.}

\item{output}{Character that describes what is to be returned: can be
\code{"plot"} (default) or \code{"subtitle"} or \code{"caption"}. Setting this to
\code{"subtitle"} will return the expression containing statistical results. If
you have set \code{results.subtitle = FALSE}, then this will return a \code{NULL}.
Setting this to \code{"caption"} will return the expression containing details
about Bayes Factor analysis, but valid only when \code{type = "parametric"} and
\code{bf.message = TRUE}, otherwise this will return a \code{NULL}.}

\item{...}{Currently ignored.}
}
\description{
Histogram with statistical details from one-sample test included in the plot
as a subtitle.
}
\details{
For more details, see:
\url{https://indrajeetpatil.github.io/ggstatsplot/articles/web_only/gghistostats.html}
}
\examples{
# for reproducibility
set.seed(123)
library(ggstatsplot)
\donttest{
# using defaults, but modifying which centrality parameter is to be shown
gghistostats(
  data = ToothGrowth,
  x = len,
  xlab = "Tooth length",
  centrality.type = "np"
)
}
}
\seealso{
\code{\link{grouped_gghistostats}}, \code{\link{ggdotplotstats}},
\code{\link{grouped_ggdotplotstats}}
}
