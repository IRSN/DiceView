% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sectionview.R
\name{sectionview.function}
\alias{sectionview.function}
\alias{sectionview,function,function-method}
\alias{sectionview.matrix}
\alias{sectionview,matrix,matrix-method}
\alias{sectionview.km}
\alias{sectionview,km,km-method}
\alias{sectionview.Kriging}
\alias{sectionview,Kriging,Kriging-method}
\alias{sectionview.NuggetKriging}
\alias{sectionview,NuggetKriging,NuggetKriging-method}
\alias{sectionview.NoiseKriging}
\alias{sectionview,NoiseKriging,NoiseKriging-method}
\alias{sectionview.glm}
\alias{sectionview,glm,glm-method}
\alias{sectionview.list}
\alias{sectionview,list,list-method}
\alias{sectionview}
\title{Plot a section view of a prediction model or function, including design points if available.}
\usage{
\method{sectionview}{`function`}(
  fun,
  vectorized = FALSE,
  dim = NULL,
  center = NULL,
  axis = NULL,
  npoints = 100,
  col_surf = "blue",
  conf_blend = 0.5,
  mfrow = NULL,
  Xlab = NULL,
  ylab = NULL,
  Xlim = NULL,
  ylim = NULL,
  title = NULL,
  add = FALSE,
  ...
)

\method{sectionview}{matrix}(
  X,
  y,
  sdy = NULL,
  center = NULL,
  axis = NULL,
  col_points = "red",
  conf_blend = NULL,
  bg_blend = 5,
  mfrow = NULL,
  Xlab = NULL,
  ylab = NULL,
  Xlim = NULL,
  ylim = NULL,
  title = NULL,
  add = FALSE,
  ...
)

\method{sectionview}{km}(
  km_model,
  type = "UK",
  center = NULL,
  axis = NULL,
  npoints = 100,
  col_points = "red",
  col_surf = "blue",
  conf_level = 0.95,
  conf_blend = 0.5,
  bg_blend = 5,
  mfrow = NULL,
  Xlab = NULL,
  ylab = NULL,
  Xlim = NULL,
  ylim = NULL,
  title = NULL,
  add = FALSE,
  ...
)

\method{sectionview}{Kriging}(
  Kriging_model,
  center = NULL,
  axis = NULL,
  npoints = 100,
  col_points = "red",
  col_surf = "blue",
  conf_level = 0.95,
  conf_blend = 0.5,
  bg_blend = 5,
  mfrow = NULL,
  Xlab = NULL,
  ylab = NULL,
  Xlim = NULL,
  ylim = NULL,
  title = NULL,
  add = FALSE,
  ...
)

\method{sectionview}{NuggetKriging}(
  NuggetKriging_model,
  center = NULL,
  axis = NULL,
  npoints = 100,
  col_points = "red",
  col_surf = "blue",
  conf_level = c(0.5, 0.8, 0.9, 0.95, 0.99),
  conf_blend = NULL,
  bg_blend = 5,
  mfrow = NULL,
  Xlab = NULL,
  ylab = NULL,
  Xlim = NULL,
  ylim = NULL,
  title = NULL,
  add = FALSE,
  ...
)

\method{sectionview}{NoiseKriging}(
  NoiseKriging_model,
  center = NULL,
  axis = NULL,
  npoints = 100,
  col_points = "red",
  col_surf = "blue",
  conf_level = 0.95,
  conf_blend = 0.5,
  bg_blend = 5,
  mfrow = NULL,
  Xlab = NULL,
  ylab = NULL,
  Xlim = NULL,
  ylim = NULL,
  title = NULL,
  add = FALSE,
  ...
)

\method{sectionview}{glm}(
  glm_model,
  center = NULL,
  axis = NULL,
  npoints = 100,
  col_points = "red",
  col_surf = "blue",
  conf_level = 0.95,
  conf_blend = 0.5,
  bg_blend = 5,
  mfrow = NULL,
  Xlab = NULL,
  ylab = NULL,
  Xlim = NULL,
  ylim = NULL,
  title = NULL,
  add = FALSE,
  ...
)

\method{sectionview}{list}(
  modelFit_model,
  center = NULL,
  axis = NULL,
  npoints = 100,
  col_points = "red",
  col_surf = "blue",
  bg_blend = 5,
  mfrow = NULL,
  Xlab = NULL,
  ylab = NULL,
  Xlim = NULL,
  ylim = NULL,
  title = NULL,
  add = FALSE,
  ...
)

sectionview(...)
}
\arguments{
\item{fun}{a function or 'predict()'-like function that returns a simple numeric or mean and standard error: list(mean=...,se=...).}

\item{vectorized}{is fun vectorized?}

\item{dim}{input variables dimension of the model or function.}

\item{center}{optional coordinates (as a list or data frame) of the center of the section view if the model's dimension is > 2.}

\item{axis}{optional matrix of 2-axis combinations to plot, one by row. The value \code{NULL} leads to all possible combinations i.e. \code{choose(D, 2)}.}

\item{npoints}{an optional number of points to discretize plot of response  surface and uncertainties.}

\item{col_surf}{color for the surface.}

\item{conf_blend}{an optional factor of alpha (color channel) blending used to plot confidence intervals.}

\item{mfrow}{an optional list to force \code{par(mfrow = ...)} call. The default value  \code{NULL} is automatically set for compact view.}

\item{Xlab}{an optional list of string to overload names for X.}

\item{ylab}{an optional string to overload name for y.}

\item{Xlim}{an optional list to force x range for all plots. The default value \code{NULL} is automatically set to include all design points.}

\item{ylim}{an optional list to force y range for all plots.}

\item{title}{an optional overload of main title.}

\item{add}{to print graphics on an existing window.}

\item{...}{arguments of the \code{sectionview.km}, \code{sectionview.glm}, \code{sectionview.Kriging} or \code{sectionview.function} function}

\item{X}{the matrix of input design.}

\item{y}{the array of output values.}

\item{sdy}{optional array of output standard error.}

\item{col_points}{color of points.}

\item{bg_blend}{an optional factor of alpha (color channel) blending used to plot design points outside from this section.}

\item{km_model}{an object of class \code{"km"}.}

\item{type}{the kriging type to use for model prediction.}

\item{conf_level}{an optional list of confidence interval values to display.}

\item{Kriging_model}{an object of class \code{"Kriging"}.}

\item{NuggetKriging_model}{an object of class \code{"Kriging"}.}

\item{NoiseKriging_model}{an object of class \code{"Kriging"}.}

\item{glm_model}{an object of class \code{"glm"}.}

\item{modelFit_model}{an object returned by DiceEval::modelFit.}
}
\description{
Plot a section view of a prediction model or function, including design points if available.
}
\details{
If available, experimental points are plotted with fading colors. Points that fall in the specified section (if any) have the color specified \code{col_points} while points far away from the center have shaded versions of the same color. The amount of fading is determined using the Euclidean distance between the plotted point and \code{center}.
}
\examples{
x1 <- rnorm(15)
x2 <- rnorm(15)

y <- x1 + x2 + rnorm(15)

model <- lm(y ~ x1 + x2)

sectionview(function(x) sum(x),
                     dim=2, center=c(0,0), Xlim=cbind(range(x1),range(x2)), col='black')

sectionview(function(x) {
                      x = as.data.frame(x)
                      colnames(x) <- names(model$coefficients[-1])
                      p = predict.lm(model, newdata=x, se.fit=TRUE)
                      list(mean=p$fit, se=p$se.fit)
                    }, vectorized=TRUE,
                    dim=2, center=c(0,0), Xlim=cbind(range(x1),range(x2)), add=TRUE)

X = matrix(runif(15*2),ncol=2)
y = apply(X,1,branin)

sectionview(X,y, center=c(.5,.5))

if (requireNamespace("DiceKriging")) { library(DiceKriging)

X = matrix(runif(15*2),ncol=2)
y = apply(X,1,branin)

model <- km(design = X, response = y, covtype="matern3_2")

sectionview(model, center=c(.5,.5))

}

if (requireNamespace("rlibkriging")) { library(rlibkriging)

X = matrix(runif(15*2),ncol=2)
y = apply(X,1,branin)

model <- Kriging(X = X, y = y, kernel="matern3_2")

sectionview(model, center=c(.5,.5))

}

if (requireNamespace("rlibkriging")) { library(rlibkriging)

X = matrix(runif(15*2),ncol=2)
y = apply(X,1,branin) + 5*rnorm(15)

model <- NuggetKriging(X = X, y = y, kernel="matern3_2")

sectionview(model, center=c(.5,.5))

}

if (requireNamespace("rlibkriging")) { library(rlibkriging)

X = matrix(runif(15*2),ncol=2)
y = apply(X,1,branin) + 5*rnorm(15)

model <- NoiseKriging(X = X, y = y, kernel="matern3_2", noise=rep(5^2,15))

sectionview(model, center=c(.5,.5))

}

x1 <- rnorm(15)
x2 <- rnorm(15)

y <- x1 + x2^2 + rnorm(15)
model <- glm(y ~ x1 + I(x2^2))

sectionview(model, center=c(.5,.5))

if (requireNamespace("DiceEval")) { library(DiceEval)

X = matrix(runif(15*2),ncol=2)
y = apply(X,1,branin)

model <- modelFit(X, y, type = "StepLinear")

sectionview(model, center=c(.5,.5))

}

## A 2D example - Branin-Hoo function
sectionview(branin, center= c(.5,.5), col='black')

\dontrun{
## a 16-points factorial design, and the corresponding response
d <- 2; n <- 16
design.fact <- expand.grid(seq(0, 1, length = 4), seq(0, 1, length = 4))
design.fact <- data.frame(design.fact); names(design.fact) <- c("x1", "x2")
y <- branin(design.fact); names(y) <- "y"

if (requireNamespace("DiceKriging")) { library(DiceKriging)
## model: km
model <- DiceKriging::km(design = design.fact, response = y)
sectionview(model, center= c(.5,.5))
sectionview(branin, center= c(.5,.5), col='red', add=TRUE)
}

if (requireNamespace("rlibkriging")) { library(rlibkriging)
## model: Kriging
model <- Kriging(X = as.matrix(design.fact), y = as.matrix(y), kernel="matern3_2")
sectionview(model, center= c(.5,.5))
sectionview(branin, center= c(.5,.5), col='red', add=TRUE)
}

## model: glm
model <- glm(y ~ 1+ x1 + x2 + I(x1^2) + I(x2^2) + x1*x2, data=cbind(y,design.fact))
sectionview(model, center= c(.5,.5))
sectionview(branin, center= c(.5,.5), col='red', add=TRUE)

if (requireNamespace("DiceEval")) { library(DiceEval)
## model: StepLinear
model <- modelFit(design.fact, y, type = "StepLinear")
sectionview(model, center= c(.5,.5))
sectionview(branin, center= c(.5,.5), col='red', add=TRUE)
}
}

}
\seealso{
\code{\link{sectionview.function}} for a section plot, and \code{\link{sectionview3d.function}} for a 2D section plot.
\code{\link{Vectorize.function}} to wrap as vectorized a non-vectorized function.

\code{\link{sectionview.matrix}} for a section plot, and \code{\link{sectionview3d.matrix}} for a 2D section plot.

\code{\link{sectionview.km}} for a section plot, and \code{\link{sectionview3d.km}} for a 2D section plot.

\code{\link{sectionview.Kriging}} for a section plot, and \code{\link{sectionview3d.Kriging}} for a 2D section plot.

\code{\link{sectionview.NuggetKriging}} for a section plot, and \code{\link{sectionview3d.NuggetKriging}} for a 2D section plot.

\code{\link{sectionview.NoiseKriging}} for a section plot, and \code{\link{sectionview3d.NoiseKriging}} for a 2D section plot.

\code{\link{sectionview.glm}} for a section plot, and \code{\link{sectionview3d.glm}} for a 2D section plot.

\code{\link{sectionview.glm}} for a section plot, and \code{\link{sectionview3d.glm}} for a 2D section plot.
}
\author{
Yann Richet, IRSN
}
