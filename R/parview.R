#### parview.function ####

#' @param fun a function or 'predict()'-like function that returns a simple numeric,
#'   or a list(mean=...,se=...).
#' @param vectorized is fun vectorized?
#' @param n_points number of space-filling (LHS) points to sample for evaluation.
#' @param Xlim a matrix (2 x D) or vector \code{c(lo, hi)} giving input ranges.
#' @param Xlab optional character vector of axis labels for inputs.
#' @param ylab optional string label for the output axis.
#' @param ... extra arguments passed to \code{\link[parallelPlot]{parallelPlot}}.
#' @rdname parview
#' @method parview function
#' @export
#' @seealso \code{\link{sectionview.function}} for 1D section plots.
#' @examples
#' parview(branin, Xlim = rbind(c(0, 0), c(1, 1)))
parview.function <- function(fun, vectorized = FALSE,
                              n_points = 500,
                              Xlim = c(0, 1),
                              Xlab = NULL, ylab = "y",
                              ...) {
    if (!requireNamespace("parallelPlot", quietly = TRUE))
        stop("Package 'parallelPlot' is required. Install it with: install.packages('parallelPlot')")

    Xlim <- matrix(Xlim, nrow = 2)
    D <- ncol(Xlim)

    if (is.null(Xlab)) Xlab <- paste0("X", seq_len(D))

    lhs <- DiceDesign::lhsDesign(n_points, D)$design
    X <- sweep(sweep(lhs, 2, Xlim[2, ] - Xlim[1, ], "*"), 2, Xlim[1, ], "+")
    colnames(X) <- Xlab

    F_x <- EvalInterval.function(fun, as.data.frame(X), vectorized)

    df <- as.data.frame(X)
    df[[ylab]] <- F_x$y

    inputColumns <- as.list(c(rep(TRUE, D), FALSE))
    names(inputColumns) <- c(Xlab, ylab)

    parallelPlot::parallelPlot(
        data          = df,
        inputColumns  = inputColumns,
        refColumnDim  = ylab,
        rotateTitle   = TRUE,
        ...
    )
}


#### parview.matrix ####

#' @param X the matrix of input design.
#' @param y the array of output values.
#' @param Xlab optional character vector of axis labels for inputs.
#' @param ylab optional string label for the output axis.
#' @param ... extra arguments passed to \code{\link[parallelPlot]{parallelPlot}}.
#' @rdname parview
#' @method parview matrix
#' @export
#' @seealso \code{\link{sectionview.matrix}} for 1D section plots.
#' @examples
#' X <- matrix(runif(15 * 2), ncol = 2)
#' y <- apply(X, 1, branin)
#' parview(X, y)
parview.matrix <- function(X, y,
                            Xlab = NULL, ylab = NULL,
                            ...) {
    if (!requireNamespace("parallelPlot", quietly = TRUE))
        stop("Package 'parallelPlot' is required. Install it with: install.packages('parallelPlot')")

    D <- ncol(X)
    if (is.null(Xlab)) Xlab <- if (!is.null(colnames(X))) colnames(X) else paste0("X", seq_len(D))
    if (is.null(ylab)) ylab <- if (is.matrix(y) && !is.null(colnames(y))) colnames(y)[1] else "y"

    df <- as.data.frame(X)
    colnames(df) <- Xlab
    df[[ylab]] <- as.numeric(y)

    inputColumns <- as.list(c(rep(TRUE, D), FALSE))
    names(inputColumns) <- c(Xlab, ylab)

    parallelPlot::parallelPlot(
        data          = df,
        inputColumns  = inputColumns,
        refColumnDim  = ylab,
        rotateTitle   = TRUE,
        ...
    )
}


#### parview — libKriging shared helper ####

parview_libKriging <- function(libKriging_model,
                                n_points = 500,
                                conf_level = 0.95,
                                Xlab = NULL, ylab = NULL,
                                Xlim = NULL,
                                ...) {
    if (!requireNamespace("parallelPlot", quietly = TRUE))
        stop("Package 'parallelPlot' is required. Install it with: install.packages('parallelPlot')")

    X_doe <- libKriging_model$X()
    y_doe <- as.numeric(libKriging_model$y())
    D <- ncol(X_doe)

    if (is.null(Xlab)) Xlab <- if (!is.null(colnames(X_doe))) colnames(X_doe) else paste0("X", seq_len(D))
    if (is.null(ylab)) ylab <- "y"
    if (is.null(Xlim)) Xlim <- apply(X_doe, 2, range)
    Xlim <- matrix(Xlim, nrow = 2, ncol = D)

    lhs <- DiceDesign::lhsDesign(n_points, D)$design
    X_pred <- sweep(sweep(lhs, 2, Xlim[2, ] - Xlim[1, ], "*"), 2, Xlim[1, ], "+")
    colnames(X_pred) <- Xlab

    p <- rlibkriging::predict(libKriging_model, X_pred, return_stdev = TRUE)
    z <- qnorm(1 - (1 - conf_level) / 2)
    ylo <- paste0(ylab, "_low")
    yhi <- paste0(ylab, "_up")

    df_pred <- as.data.frame(X_pred)
    df_pred[[ylab]] <- p$mean
    df_pred[[ylo]]  <- p$mean - z * p$stdev
    df_pred[[yhi]]  <- p$mean + z * p$stdev

    df_doe <- as.data.frame(X_doe)
    colnames(df_doe) <- Xlab
    df_doe[[ylab]] <- y_doe
    df_doe[[ylo]]  <- NA_real_
    df_doe[[yhi]]  <- NA_real_

    df <- rbind(df_pred, df_doe)

    out_cols <- c(ylab, ylo, yhi)
    inputColumns <- as.list(c(rep(TRUE, D), rep(FALSE, 3)))
    names(inputColumns) <- c(Xlab, out_cols)

    parallelPlot::parallelPlot(
        data          = df,
        inputColumns  = inputColumns,
        refColumnDim  = ylab,
        rotateTitle   = TRUE,
        ...
    )
}


#### parview.Kriging ####

#' @param Kriging_model an object of class \code{"Kriging"}.
#' @param n_points number of LHS prediction points.
#' @param conf_level confidence level for uncertainty bands shown as extra axes.
#' @param Xlab optional character vector of axis labels for inputs.
#' @param ylab optional string label for the output axis.
#' @param Xlim optional input bounds matrix (2 x D); defaults to design range.
#' @param ... extra arguments passed to \code{\link[parallelPlot]{parallelPlot}}.
#' @rdname parview
#' @method parview Kriging
#' @export
#' @seealso \code{\link{sectionview.Kriging}} for 1D section plots.
#' @examples
#' if (requireNamespace("rlibkriging")) { library(rlibkriging)
#'   X <- matrix(runif(15 * 2), ncol = 2)
#'   y <- apply(X, 1, branin)
#'   model <- Kriging(X = X, y = as.matrix(y), kernel = "matern3_2")
#'   parview(model)
#' }
parview.Kriging <- function(Kriging_model, n_points = 500, conf_level = 0.95,
                             Xlab = NULL, ylab = NULL, Xlim = NULL, ...) {
    parview_libKriging(Kriging_model,
                       n_points = n_points, conf_level = conf_level,
                       Xlab = Xlab, ylab = ylab, Xlim = Xlim, ...)
}


#### parview.WarpKriging ####

#' @param WarpKriging_model an object of class \code{"WarpKriging"}.
#' @param n_points number of LHS prediction points.
#' @param conf_level confidence level for uncertainty bands shown as extra axes.
#' @param Xlab optional character vector of axis labels for inputs.
#' @param ylab optional string label for the output axis.
#' @param Xlim optional input bounds matrix (2 x D); defaults to design range.
#' @param ... extra arguments passed to \code{\link[parallelPlot]{parallelPlot}}.
#' @rdname parview
#' @method parview WarpKriging
#' @export
#' @seealso \code{\link{sectionview.WarpKriging}} for 1D section plots.
#' @examples
#' if (requireNamespace("rlibkriging")) { library(rlibkriging)
#'   X <- matrix(runif(15 * 2), ncol = 2)
#'   y <- apply(X, 1, branin) + 5 * rnorm(15)
#'   model <- WarpKriging(y = y, X = X, warping = c("affine", "affine"), kernel = "matern3_2")
#'   parview(model)
#' }
parview.WarpKriging <- function(WarpKriging_model, n_points = 500, conf_level = 0.95,
                                 Xlab = NULL, ylab = NULL, Xlim = NULL, ...) {
    parview_libKriging(WarpKriging_model,
                       n_points = n_points, conf_level = conf_level,
                       Xlab = Xlab, ylab = ylab, Xlim = Xlim, ...)
}


#### parview.km ####

#' @param km_model an object of class \code{"km"}.
#' @param type the kriging type to use for model prediction.
#' @param n_points number of LHS prediction points.
#' @param conf_level confidence level for uncertainty bands shown as extra axes.
#' @param Xlab optional character vector of axis labels for inputs.
#' @param ylab optional string label for the output axis.
#' @param Xlim optional input bounds matrix (2 x D); defaults to design range.
#' @param ... extra arguments passed to \code{\link[parallelPlot]{parallelPlot}}.
#' @rdname parview
#' @method parview km
#' @export
#' @seealso \code{\link{sectionview.km}} for 1D section plots.
#' @examples
#' if (requireNamespace("DiceKriging")) { library(DiceKriging)
#'   X <- matrix(runif(15 * 2), ncol = 2)
#'   y <- apply(X, 1, branin)
#'   model <- km(design = X, response = y, covtype = "matern3_2")
#'   parview(model)
#' }
parview.km <- function(km_model, type = "UK", n_points = 500, conf_level = 0.95,
                        Xlab = NULL, ylab = NULL, Xlim = NULL, ...) {
    if (!requireNamespace("parallelPlot", quietly = TRUE))
        stop("Package 'parallelPlot' is required. Install it with: install.packages('parallelPlot')")

    X_doe <- km_model@X
    y_doe <- as.numeric(km_model@y)
    D <- ncol(X_doe)

    if (is.null(Xlab)) Xlab <- if (!is.null(colnames(X_doe))) colnames(X_doe) else paste0("X", seq_len(D))
    if (is.null(ylab)) ylab <- if (!is.null(colnames(km_model@y))) colnames(km_model@y)[1] else "y"
    if (is.null(Xlim)) Xlim <- apply(X_doe, 2, range)
    Xlim <- matrix(Xlim, nrow = 2, ncol = D)

    lhs <- DiceDesign::lhsDesign(n_points, D)$design
    X_pred <- sweep(sweep(lhs, 2, Xlim[2, ] - Xlim[1, ], "*"), 2, Xlim[1, ], "+")
    colnames(X_pred) <- Xlab

    p <- DiceKriging::predict.km(km_model, type = type, newdata = X_pred, checkNames = FALSE)
    z <- qnorm(1 - (1 - conf_level) / 2)
    ylo <- paste0(ylab, "_low")
    yhi <- paste0(ylab, "_up")

    df_pred <- as.data.frame(X_pred)
    df_pred[[ylab]] <- p$mean
    df_pred[[ylo]]  <- p$mean - z * p$sd
    df_pred[[yhi]]  <- p$mean + z * p$sd

    df_doe <- as.data.frame(X_doe)
    colnames(df_doe) <- Xlab
    df_doe[[ylab]] <- y_doe
    df_doe[[ylo]]  <- NA_real_
    df_doe[[yhi]]  <- NA_real_

    df <- rbind(df_pred, df_doe)

    out_cols <- c(ylab, ylo, yhi)
    inputColumns <- as.list(c(rep(TRUE, D), rep(FALSE, 3)))
    names(inputColumns) <- c(Xlab, out_cols)

    parallelPlot::parallelPlot(
        data          = df,
        inputColumns  = inputColumns,
        refColumnDim  = ylab,
        rotateTitle   = TRUE,
        ...
    )
}


#### parview.glm ####

#' @param glm_model an object of class \code{"glm"}.
#' @param n_points number of LHS prediction points.
#' @param conf_level confidence level for uncertainty bands shown as extra axes.
#' @param Xlab optional character vector of axis labels for inputs.
#' @param ylab optional string label for the output axis.
#' @param Xlim optional input bounds matrix (2 x D); defaults to design range.
#' @param ... extra arguments passed to \code{\link[parallelPlot]{parallelPlot}}.
#' @rdname parview
#' @method parview glm
#' @export
#' @seealso \code{\link{sectionview.glm}} for 1D section plots.
#' @examples
#' x1 <- rnorm(15); x2 <- rnorm(15)
#' y <- x1 + x2^2 + rnorm(15)
#' model <- glm(y ~ x1 + I(x2^2))
#' parview(model)
parview.glm <- function(glm_model, n_points = 500, conf_level = 0.95,
                         Xlab = NULL, ylab = NULL, Xlim = NULL, ...) {
    if (!requireNamespace("parallelPlot", quietly = TRUE))
        stop("Package 'parallelPlot' is required. Install it with: install.packages('parallelPlot')")

    Xlab_model <- all.vars(glm_model$formula)[-1]
    ylab_model <- all.vars(glm_model$formula)[1]
    if (is.null(Xlab)) Xlab <- Xlab_model
    if (is.null(ylab)) ylab <- ylab_model

    X_doe <- do.call(cbind, lapply(Xlab_model, function(n) glm_model$data[[n]]))
    colnames(X_doe) <- Xlab
    y_doe <- glm_model$data[[ylab_model]]
    D <- ncol(X_doe)

    if (is.null(Xlim)) Xlim <- apply(X_doe, 2, range)
    Xlim <- matrix(Xlim, nrow = 2, ncol = D)

    lhs <- DiceDesign::lhsDesign(n_points, D)$design
    X_pred <- sweep(sweep(lhs, 2, Xlim[2, ] - Xlim[1, ], "*"), 2, Xlim[1, ], "+")
    X_pred_df <- as.data.frame(X_pred)
    colnames(X_pred_df) <- Xlab_model

    p <- predict.glm(glm_model, newdata = X_pred_df, se.fit = TRUE)
    z <- qnorm(1 - (1 - conf_level) / 2)
    ylo <- paste0(ylab, "_low")
    yhi <- paste0(ylab, "_up")

    df_pred <- as.data.frame(X_pred)
    colnames(df_pred) <- Xlab
    df_pred[[ylab]] <- p$fit
    df_pred[[ylo]]  <- p$fit - z * p$se.fit
    df_pred[[yhi]]  <- p$fit + z * p$se.fit

    df_doe <- as.data.frame(X_doe)
    df_doe[[ylab]] <- y_doe
    df_doe[[ylo]]  <- NA_real_
    df_doe[[yhi]]  <- NA_real_

    df <- rbind(df_pred, df_doe)

    out_cols <- c(ylab, ylo, yhi)
    inputColumns <- as.list(c(rep(TRUE, D), rep(FALSE, 3)))
    names(inputColumns) <- c(Xlab, out_cols)

    parallelPlot::parallelPlot(
        data          = df,
        inputColumns  = inputColumns,
        refColumnDim  = ylab,
        rotateTitle   = TRUE,
        ...
    )
}


#### parview.list (DiceEval modelFit) ####

#' @param modelFit_model an object returned by \code{DiceEval::modelFit}.
#' @param n_points number of LHS prediction points.
#' @param Xlab optional character vector of axis labels for inputs.
#' @param ylab optional string label for the output axis.
#' @param Xlim optional input bounds matrix (2 x D); defaults to design range.
#' @param ... extra arguments passed to \code{\link[parallelPlot]{parallelPlot}}.
#' @rdname parview
#' @method parview list
#' @export
#' @seealso \code{\link{sectionview.list}} for 1D section plots.
#' @examples
#' if (requireNamespace("DiceEval")) { library(DiceEval)
#'   X <- matrix(runif(15 * 2), ncol = 2)
#'   y <- apply(X, 1, branin)
#'   model <- modelFit(X, y, type = "StepLinear")
#'   parview(model)
#' }
parview.list <- function(modelFit_model, n_points = 500,
                          Xlab = NULL, ylab = NULL, Xlim = NULL, ...) {
    if (!requireNamespace("parallelPlot", quietly = TRUE))
        stop("Package 'parallelPlot' is required. Install it with: install.packages('parallelPlot')")

    X_doe <- modelFit_model$data$X
    y_doe <- modelFit_model$data$Y
    D <- ncol(X_doe)

    if (is.null(Xlab)) Xlab <- if (!is.null(colnames(X_doe))) colnames(X_doe) else paste0("X", seq_len(D))
    if (is.null(ylab)) ylab <- if (!is.null(colnames(y_doe))) colnames(y_doe)[1] else "y"
    if (is.null(Xlim)) Xlim <- apply(X_doe, 2, range)
    Xlim <- matrix(Xlim, nrow = 2, ncol = D)

    lhs <- DiceDesign::lhsDesign(n_points, D)$design
    X_pred <- sweep(sweep(lhs, 2, Xlim[2, ] - Xlim[1, ], "*"), 2, Xlim[1, ], "+")

    X_pred_df <- as.data.frame(X_pred)
    colnames(X_pred_df) <- if (!is.null(colnames(X_doe))) colnames(X_doe) else Xlab
    y_pred <- DiceEval::modelPredict(modelFit_model, X_pred_df)

    df_pred <- as.data.frame(X_pred)
    colnames(df_pred) <- Xlab
    df_pred[[ylab]] <- as.numeric(y_pred)

    df_doe <- as.data.frame(X_doe)
    colnames(df_doe) <- Xlab
    df_doe[[ylab]] <- as.numeric(y_doe)

    df <- rbind(df_pred, df_doe)

    inputColumns <- as.list(c(rep(TRUE, D), FALSE))
    names(inputColumns) <- c(Xlab, ylab)

    parallelPlot::parallelPlot(
        data          = df,
        inputColumns  = inputColumns,
        refColumnDim  = ylab,
        rotateTitle   = TRUE,
        ...
    )
}


#### S3 generic ####

#' @import methods
if (!isGeneric("parview")) {
    setGeneric("parview", def = function(...) standardGeneric("parview"))
}

#' @title Interactive parallel coordinates view of a prediction model or function.
#' @description Renders an interactive parallel coordinates chart (via the
#'   \pkg{parallelPlot} package) showing all input dimensions and the output
#'   simultaneously.  Lines are colored by the output value, making it easy to
#'   identify which input regions drive high or low responses.
#'
#'   For model-based methods (\code{km}, \code{Kriging}, \code{WarpKriging},
#'   \code{glm}, \code{list}) a space-filling LHS prediction grid is displayed
#'   together with the observed design points.  Kriging-based methods also add
#'   \code{y_low} / \code{y_up} axes for the predictive confidence interval.
#'
#' @param ... arguments of the relevant \code{parview.*} method.
#' @export
#' @examples
#' ## Simple function
#' parview(branin, Xlim = rbind(c(0, 0), c(1, 1)))
#'
#' ## Design points only
#' X <- matrix(runif(30 * 2), ncol = 2)
#' y <- apply(X, 1, branin)
#' parview(X, y)
parview <- function(...) {
    UseMethod("parview")
}
