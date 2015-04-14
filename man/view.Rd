\name{view}
\alias{view}
\alias{view,character,km-method}
\alias{view,character,list-method}
\alias{view,character,function-method}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Plot a view of a kriging, modelPredict model or function.}
\description{
  Standard entry point function toplot a  view of a kriging, modelPredict model or function. It is useful for a better understanding of a model behaviour.
  This function is just a wrapping of all other ploting functions (section, contour, section3d), for all supported types (km, list, function).
}

\usage{
view(type = "auto", model, ...)
}


%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{type}{a string to describe the type of view to display: "auto", "section", "xy", "section3d", "3d", "contour".}
  \item{model}{an object of class \code{"km"}, a list that can be used
    in a \code{"modelPredict"} call, or a function.}
  \item{\dots}{other arguments of the \code{sectionview}, \code{sectionview3d} or \code{contourview} function }
}

%%\value{ No value.}

\author{Yann Richet, IRSN }

\seealso{\code{\link{sectionview}, \link{sectionview3d}, \link{contourview}}}

\keyword{models}
\examples{
## A 2D example - Branin-Hoo function. See DiceKriging package manual 
## a 16-points factorial design, and the corresponding response
d <- 2; n <- 16
design.fact <- expand.grid(seq(0, 1, length = 4), seq(0, 1, length = 4))
design.fact <- data.frame(design.fact); names(design.fact)<-c("x1", "x2")
y <- branin(design.fact) 

## kriging model 1 : matern5_2 covariance structure, no trend, no nugget effect

m1 <- km(design = design.fact, response = y)

## the same as sectionview3d
view("3d",m1)
view("3d",branin, dim = 2, col='red', add = TRUE)

}
