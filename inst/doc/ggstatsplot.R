## ---- warning=FALSE, message=FALSE, echo=FALSE--------------------------------
# needed libraries
library(ggstatsplot)

knitr::opts_chunk$set(
  collapse = TRUE,
  out.width = "100%",
  dpi = 300,
  comment = "#>",
  message = FALSE,
  warning = FALSE
)

set.seed(123) # for reproducibility

## ----citation, echo=FALSE, comment = ""---------------------------------------
citation("ggstatsplot")

## ----iris, fig.width=8, fig.height=5, error=TRUE, fig.cap="Example plot from the `{ggstatsplot}` package illustrating its philosophy of juxtaposing informative visualizations with details from statistical analysis. To see all supported plots and statistical analyses, see the package website: \\url{https://indrajeetpatil.github.io/ggstatsplot/}"----
library(ggstatsplot)

ggbetweenstats(iris, Species, Sepal.Length)

## ----reporting, echo=FALSE, fig.cap="Comparing the 'Standard' approach of reporting statistical analysis in a publication/report with the 'ggstatsplot' approach of reporting the same analysis next to an informative graphic. Note that the results described in the 'Standard' approach are about the 'Dinosaur' dataset plotted on the right. Without the accompanying visualization, it is hard to evaluate the validity of the results. The ideal reporting practice will be a hybrid of these two approaches where the plot contains both the visual and numerical summaries about a statistical model, while the narrative provides interpretative context for the reported statistics."----
knitr::include_graphics("../man/figures/stats_reporting_format.png")

