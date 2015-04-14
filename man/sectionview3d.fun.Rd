\name{sectionview3d.fun}
\alias{sectionview3d.fun}
\title{Plot a 3-D (using RGL) view of a function}
\usage{
  sectionview3d.fun(fun,
    dim = ifelse(is.null(center), 2, length(center)),
    center = NULL, axis = NULL, npoints = 20, col = "blue",
    Xname = NULL, yname = NULL, Xscale = 1, yscale = 1,
    xlim = c(0, 1), ylim = NULL, title = NULL, add = FALSE,
    ...)
}
\arguments{
  \item{fun}{an object of class \code{"function"}.}

  \item{dim}{the dimension of fun arguments.}

  \item{center}{optional coordinates (as a list or data
  frame) of the center of the section view if the model's
  dimension is > 2.}

  \item{axis}{optional matrix of 2-axis combinations to
  plot, one by row. The value \code{NULL} leads to all
  possible combinations i.e. \code{choose(D, 2)}.}

  \item{npoints}{an optional number of points to discretize
  plot of response surface and uncertainties.}

  \item{col}{color for the surface.}

  \item{xlim}{a list to give x range for all plots.}

  \item{ylim}{an optional list to force y range for all
  plots.}

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
  Plot a 3-D view of a function. Provide a better
  understanding of the model behaviour.
}
\examples{
## A 2D example - Branin-Hoo function.
sectionview3d.fun(branin,dim = 2)
}
\author{
  Yann Richet, IRSN
}
\seealso{
  \code{\link{sectionview}}
}

