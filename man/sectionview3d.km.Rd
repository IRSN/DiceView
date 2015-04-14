\name{sectionview3d.km}
\alias{sectionview3d.km}
\title{Plot a 3-D (using RGL) view of a kriging model, including design points}
\usage{
  sectionview3d.km(model, type = "UK", center = NULL,
    axis = NULL, npoints = 20, col_points = "red",
    col_surf = "blue", col_needles = NA,
    conf_lev = c(0.95), conf_blend = NULL, bg_blend = 5,
    Xname = NULL, yname = NULL, Xscale = 1, yscale = 1,
    xlim = NULL, ylim = NULL, title = NULL, add = FALSE,
    ...)
}
\arguments{
  \item{model}{an object of class \code{"km"}.}

  \item{type}{the kriging type to use for model
  prediction.}

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

  \item{conf_lev}{an optional list of confidence interval
  values to display.}

  \item{conf_blend}{an optional factor of alpha (color
  channel) blending used to plot confidence intervals.}

  \item{bg_blend}{an optional factor of alpha (color
  channel) blending used to plot design points outside from
  this section.}

  \item{xlim}{an optional list to force x range for all
  plots. The default value \code{NULL} is automatically set
  to include all design points.}

  \item{ylim}{an optional list to force y range for all
  plots. The default value \code{NULL} is automatically set
  to include all design points (and their 1-99
  percentiles).}

  \item{Xname}{an optional list of string to overload names
  for X.}

  \item{yname}{an optional string to overload name for y.}

  \item{Xscale}{an optional factor to scale X.}

  \item{yscale}{an optional factor to scale y.}

  \item{title}{an optional overload of main title.}

  \item{add}{to print graphics on an existing window.}

  \item{\dots}{further arguments passed to the first call
  of \code{plot3d}.}
}
\description{
  Plot a 3-D view of a kriging model: mean response
  surface, fitted points and confidence surfaces. Provide a
  better understanding of the kriging model behaviour.
}
\details{
  Experimental points are plotted with fading colors.
  Points that fall in the specified section (if any) have
  the color specified \code{col_points} while points far
  away from the center have shaded versions of the same
  color. The amount of fading is determined using the
  Euclidean distance between the plotted point and
  \code{center}. The variables chosen with their number are
  to be found in the \code{X} slot of the model. Thus they
  are 'spatial dimensions' but not 'trend variables'.
}
\note{
  The confidence bands are computed using normal quantiles
  and the standard error given by \code{predict.km}.
}
\examples{
## A 2D example - Branin-Hoo function. See DiceKriging package manual
## a 16-points factorial design, and the corresponding response
d <- 2; n <- 16
design.fact <- expand.grid(seq(0, 1, length = 4), seq(0, 1, length = 4))
design.fact <- data.frame(design.fact); names(design.fact)<-c("x1", "x2")
y <- branin(design.fact)

## kriging model 1 : matern5_2 covariance structure, no trend, no nugget effect

m1 <- km(design = design.fact, response = y)

## the same as sectionview3d.km
sectionview3d(m1)

## change colors
sectionview3d(m1, col_points = "firebrick", col_surf = "SpringGreen2")

## change colors,  use finer grid and add needles
sectionview3d(m1, npoints = c(50, 30), col_points = "orange",
  col_surf = "SpringGreen2", col_needles = "firebrick")
}
\author{
  Yann Richet, IRSN
}
\seealso{
  See \code{\link{sectionview.km}} and the
  \code{\link[DiceKriging]{km}} function in the
  \pkg{DiceKriging} package.
}
\keyword{models}

