\name{sectionview.km}
\alias{sectionview.km}
\title{Plot section views of a kriging model, including design points}
\usage{
  sectionview.km(model, type = "UK", center = NULL,
    axis = NULL, npoints = 100, col_points = "red",
    col_surf = "blue",
    conf_lev = c(0.5, 0.8, 0.9, 0.95, 0.99),
    conf_blend = NULL, bg_blend = 5, mfrow = NULL,
    Xname = NULL, yname = NULL, Xscale = 1, yscale = 1,
    xlim = NULL, ylim = NULL, title = NULL, add = FALSE,
    ...)
}
\arguments{
  \item{model}{an object of class "km".}

  \item{type}{the kriging type to use for model
  prediction.}

  \item{center}{optional coordinates (as a list or data
  frame) of the center of the section view if the model's
  dimension is > 1.}

  \item{axis}{optional matrix of 1-axis combinations to
  plot, one by row. The value \code{NULL} leads to all
  possible combinations i.e. \code{1:D}.}

  \item{npoints}{an optional number of points to discretize
  plot of response surface and uncertainties.}

  \item{col_points}{color of points.}

  \item{col_surf}{color for the section.}

  \item{conf_lev}{an optional list of confidence interval
  values to display.}

  \item{conf_blend}{an optional factor of alpha (color
  channel) blending used to plot confidence intervals.}

  \item{bg_blend}{an optional factor of alpha (color
  channel) blending used to plot design points outside from
  this section.}

  \item{mfrow}{an optional list to force \code{par(mfrow =
  ...)} call. The default value \code{NULL} is
  automatically set for compact view.}

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
  of \code{plot}.}
}
\description{
  Plot one section view per dimension of a kriging model
  thus providing a better understanding of the model
  behaviour including uncertainty.
}
\details{
  A multiple rows/columns plot is produced. Experimental
  points are plotted with fading colors. Points that fall
  in the specified section (if any) have the color
  specified \code{col_points} while points far away from
  the center have shaded versions of the same color. The
  amount of fading is determined using the Euclidean
  distance between the plotted point and \code{center}.
}
\examples{
## A 2D example - Branin-Hoo function
## a 16-points factorial design, and the corresponding response
d <- 2; n <- 16
design.fact <- expand.grid(seq(0, 1, length = 4), seq(0, 1, length = 4))
design.fact <- data.frame(design.fact); names(design.fact)<-c("x1", "x2")
y <- branin(design.fact)

## kriging model 1 : matern5_2 covariance structure, no trend, no nugget effect
m1 <- km(design = design.fact, response = y)

sectionview(m1, center = c(.333, .333))

## Display reference function
sectionview(branin,dim=2,center=c(.333, .333),add=TRUE,col='red')
}
\author{
  Yann Richet, IRSN
}
\seealso{
  The function \code{\link{sectionview3d.km}} produces a 3D
  version. For more information on the \code{km} class, see
  the \code{\link[DiceKriging]{km}} function in the
  \pkg{DiceKriging} package.
}
\keyword{models}

