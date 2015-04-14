\name{sectionview3d.list}
\alias{sectionview3d.list}
\title{Plot a 3-D (using RGL) view of a model, including design points}
\usage{
  sectionview3d.list(model, center = NULL, axis = NULL,
    npoints = 20, col_points = "red", col_surf = "blue",
    col_needles = NA, bg_blend = 5, Xname = NULL,
    yname = NULL, Xscale = 1, yscale = 1, xlim = NULL,
    ylim = NULL, title = NULL, add = FALSE, ...)
}
\arguments{
  \item{model}{a list that can be used in the
  \code{modelPredict} function of the \pkg{DiceEval}
  package.}

  \item{center}{optional coordinates (as a list or data
  frame) of the center of the section view if the model's
  dimension is > 2.}

  \item{axis}{optional matrix of 2-axis combinations to
  plot, one by row. The value \code{NULL} leads to all
  possible combinations i.e. \code{choose(D, 2)}.}

  \item{npoints}{an optional number of points to discretize
  plot of response surface and uncertainties.}

  \item{col_points}{color of points.}

  \item{col_surf}{color for the surface.}

  \item{col_needles}{color of "needles" for the points. The
  default \code{NA} corresponds to no needle plotted. When
  a valid color is given, needles are plotted using the
  same fading mechanism as for points.}

  \item{bg_blend}{an optional factor of alpha (color
  channel) blending used to plot design points outside from
  this section.}

  \item{xlim}{an optional list to force x range for all
  plots. The default value \code{NULL} is automatically set
  to include all design points.}

  \item{ylim}{an optional list to force y range for all
  plots. The default value \code{NULL} is automatically set
  to include all design points.}

  \item{Xname}{an optional list of string to overload names
  for X.}

  \item{yname}{an optional string to overload name for y.}

  \item{Xscale}{an optional factor to scale X.}

  \item{yscale}{an optional factor to scale y.}

  \item{title}{an optional overload of main title.}

  \item{add}{to print graphics on an existing window.}

  \item{\dots}{optional arguments passed to the first call
  of \code{plot3d}.}
}
\description{
  Plot a 3-D view of a model, thus providing a better
  understanding of its behaviour.
}
\details{
  Experimental points are plotted with fading colors.
  Points that fall in the specified section (if any) have
  the color specified \code{col_points} while points far
  away from the center have shaded versions of the same
  color. The amount of fading is determined using the
  Euclidean distance between the plotted point and
  \code{center}. The variables chosen with their number are
  to be found in the \code{data$X} element of the model.
  Thus they are original data variables but not trend
  variables that may have been created using the model's
  formula
}
\examples{
## A 2D example - Branin-Hoo function
## a 16-points factorial design, and the corresponding response
d <- 2; n <- 16
design.fact <- expand.grid(seq(0, 1, length = 4), seq(0, 1, length = 4))
design.fact <- data.frame(design.fact); names(design.fact) <-c("x1", "x2")
y <- branin(design.fact)

## linear model
m1 <- modelFit(design.fact, y$x1, type = "Linear", formula = "Y~.")

## the same as sectionview3d.list
sectionview3d(m1)
}
\author{
  Yann Richet, IRSN
}
\seealso{
  \code{\link{sectionview.list}} for a 2D plot, and the
  \code{\link[DiceEval]{modelPredict}} function in the
  \pkg{DiceEval} package. The
  \code{\link{sectionview3d.km}} produces a similar plot
  for \code{km} objects.
}
\keyword{models}

