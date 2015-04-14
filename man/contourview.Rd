\name{contourview}
\alias{contourview}
\alias{contourview,km-method}
\alias{contourview,list-method}
\alias{contourview,function-method}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Plot a contour view of a kriging or modelPredict model including design points, or a function.}
\description{
  Plot a contour view of a kriging or modelPredict model. It is useful for a better understanding of a model behaviour.
}

\usage{
contourview(model, ...)
}


%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{model}{an object of class \code{"km"}, a list that can be used
    in a \code{"modelPredict"} call, or a function.}
  \item{\dots}{other arguments of the \code{contourview.km}, \code{contourview.list} or \code{contourview.fun} function }
}

%%\value{ No value.}

\author{Yann Richet, IRSN }

\seealso{\code{\link{sectionview3d}}}

\keyword{models}
\examples{
## A 2D example - Branin-Hoo function
## a 16-points factorial design, and the corresponding response
d <- 2; n <- 16
design.fact <- expand.grid(seq(0, 1, length = 4), seq(0, 1, length = 4))
design.fact <- data.frame(design.fact); names(design.fact) <- c("x1", "x2")
y <- branin(design.fact) 

## kriging model 1 : matern5_2 covariance structure, no trend, no nugget effect
m1 <- km(design = design.fact, response = y)

contourview(m1)

contourview(branin, dim = 2, add=TRUE)

}
