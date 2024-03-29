#' @param center optional coordinates (as a list or data frame) of the center of the section view if the model's dimension is > 2.
#' @param axis optional matrix of 2-axis combinations to plot, one by row. The value \code{NULL} leads to all possible combinations i.e. \code{choose(D, 2)}.
#' @param npoints an optional number of points to discretize plot of response  surface and uncertainties.
#' @param col_surf color for the surface.
#' @param mfrow  an optional list to force \code{par(mfrow = ...)} call. The default value  \code{NULL} is automatically set for compact view.
#' @param Xlim an optional list to force x range for all plots. The default value \code{NULL} is automatically set to include all design points.
#' @param ylim an optional list to force y range for all plots.
#' @param Xlab an optional list of string to overload names for X.
#' @param ylab an optional string to overload name for y.
#' @param title an optional overload of main title.
#' @param add to print graphics on an existing window.
#' @param ... optional arguments passed to the first call of \code{plot}.
#' @author Yann Richet, IRSN
