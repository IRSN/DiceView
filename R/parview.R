#### Internal renderers ####

# Base-R static parallel coordinates renderer.
# df       : data frame, all columns already in display order
# n_pred   : first n_pred rows are predictions (colored by refColumnDim via col_fun);
#             remaining rows are observations (colored col_points, drawn on top)
# refColumnDim : column name used for the color scale
# col_fun      : base color for predictions (palette built with col.levels)
# col_points   : color for observation rows
# ...          : passed to plot()
parview_base <- function(df, n_pred, refColumnDim, col_fun, col_points, ...) {
    n_rows  <- nrow(df)
    col_names <- names(df)
    n_axes  <- length(col_names)

    # Per-axis ranges; expand if constant to avoid division by zero
    col_ranges <- lapply(df, function(x) {
        r <- range(x, na.rm = TRUE)
        if (diff(r) == 0) r + c(-1, 1) else r
    })

    # Normalize each column to [0, 1]
    df_norm <- as.data.frame(mapply(
        function(x, r) (x - r[1]) / diff(r),
        df, col_ranges, SIMPLIFY = FALSE
    ))

    x_pos <- seq_len(n_axes)

    # Palette from col_fun (same policy as contourview: saturation ramp in HSV)
    ref_vals  <- df[[refColumnDim]][seq_len(n_pred)]
    ref_range <- range(ref_vals, na.rm = TRUE)
    n_pal     <- 256L
    pal       <- col.levels(col_fun, n_pal)
    denom     <- if (diff(ref_range) == 0) 1 else diff(ref_range)
    idx       <- pmax(1L, pmin(n_pal,
                      as.integer((ref_vals - ref_range[1]) / denom * (n_pal - 1L)) + 1L))
    pred_colors <- pal[idx]

    op <- par(mar = c(5, 0.5, 2, 0.5))
    on.exit(par(op), add = TRUE)

    plot(NA, xlim = c(0.5, n_axes + 0.5), ylim = c(-0.1, 1.02),
         xaxt = "n", yaxt = "n", xlab = "", ylab = "", bty = "n", ...)

    # Prediction lines first (thin, behind)
    for (i in seq_len(n_pred))
        lines(x_pos, as.numeric(df_norm[i, ]), col = pred_colors[i], lwd = 0.5)

    # Observation lines on top (thicker, col_points)
    if (n_pred < n_rows)
        for (i in seq(n_pred + 1L, n_rows))
            lines(x_pos, as.numeric(df_norm[i, ]), col = col_points, lwd = 1.5)

    # Vertical axis lines, ticks, and labels
    for (j in x_pos) {
        segments(j, 0, j, 1, col = "black", lwd = 1.5)
        r     <- col_ranges[[j]]
        ticks <- pretty(r, n = 4)
        ticks <- ticks[ticks >= r[1] & ticks <= r[2]]
        tick_n <- (ticks - r[1]) / diff(r)
        segments(j - 0.04, tick_n, j + 0.04, tick_n, col = "black")
        text(j + 0.06, tick_n, labels = format(ticks, digits = 3),
             adj = c(0, 0.5), cex = 0.55)
        text(j, -0.08, labels = col_names[j],
             adj = c(0.5, 1), srt = 30, cex = 0.8, xpd = TRUE)
    }

    invisible(NULL)
}

# Map an R color to the closest parallelPlot continuousCS name using HSV hue.
col_fun_to_continuousCS <- function(col_fun) {
    rgb <- grDevices::col2rgb(col_fun) / 255
    hsv <- grDevices::rgb2hsv(rgb[1], rgb[2], rgb[3])
    s <- hsv[2]
    h <- hsv[1] * 360  # hue in degrees [0, 360)
    if (s < 0.15)          return("Greys")
    if (h < 15 || h >= 345) return("Reds")
    if (h < 45)            return("Oranges")
    if (h < 75)            return("YlOrBr")
    if (h < 150)           return("Greens")
    if (h < 195)           return("GnBu")
    if (h < 255)           return("Blues")
    if (h < 300)           return("Purples")
    return("RdPu")
}

# Dispatcher: routes to parallelPlot or base engine.
parview_render <- function(df, n_pred, refColumnDim, col_fun, col_points,
                            inputColumns, engine, ...) {
    engine <- match.arg(engine, c("parallelPlot", "base"))

    if (engine == "parallelPlot") {
        if (!requireNamespace("parallelPlot", quietly = TRUE))
            stop("Package 'parallelPlot' is required. Install it with: install.packages('parallelPlot')")

        extra <- list(...)
        args <- c(list(data = df, inputColumns = inputColumns,
                       refColumnDim = refColumnDim, rotateTitle = TRUE,
                       continuousCS = if (is.null(extra$continuousCS))
                                          col_fun_to_continuousCS(col_fun)
                                      else extra$continuousCS),
                  extra[setdiff(names(extra), "continuousCS")])

        if (n_pred < nrow(df)) {
            nth <- n_pred + 1L
            args$cssRules <- setNames(
                list(paste0("stroke:", col_points), paste0("stroke:", col_points)),
                c(paste0(".foreground path:nth-child(n+", nth, ")"),
                  paste0(".background path:nth-child(n+", nth, ")"))
            )
        }
        do.call(parallelPlot::parallelPlot, args)
    } else {
        parview_base(df = df, n_pred = n_pred, refColumnDim = refColumnDim,
                     col_fun = col_fun, col_points = col_points, ...)
    }
}


#### parview.function ####

#' @param fun a function or 'predict()'-like function that returns a simple numeric,
#'   or a list(mean=...,se=...).
#' @param vectorized is fun vectorized?
#' @param n_points number of space-filling (LHS) points to sample for evaluation.
#' @param Xlim a matrix (2 x D) or vector \code{c(lo, hi)} giving input ranges.
#' @param Xlab optional character vector of axis labels for inputs.
#' @param ylab optional string label for the output axis.
#' @param engine rendering engine: \code{"parallelPlot"} (interactive htmlwidget, default)
#'   or \code{"base"} (static base-R plot).
#' @param ... extra arguments passed to the rendering engine
#'   (\code{parallelPlot::parallelPlot} or \code{plot}).
#' @rdname parview
#' @method parview function
#' @export
#' @seealso \code{\link{sectionview.function}} for 1D section plots.
#' @examples
#' parview(branin, Xlim = rbind(c(0, 0), c(1, 1)), engine = "base")
parview.function <- function(fun, vectorized = FALSE,
                              n_points = 500,
                              Xlim = c(0, 1),
                              col_fun = if (!is.null(col)) col else "blue",
                              col = NULL,
                              Xlab = NULL, ylab = "y",
                              engine = "parallelPlot",
                              ...) {
    Xlim <- matrix(Xlim, nrow = 2)
    D    <- ncol(Xlim)
    if (is.null(Xlab)) Xlab <- paste0("X", seq_len(D))

    lhs <- DiceDesign::lhsDesign(n_points, D)$design
    X   <- sweep(sweep(lhs, 2, Xlim[2, ] - Xlim[1, ], "*"), 2, Xlim[1, ], "+")
    colnames(X) <- Xlab

    F_x <- EvalInterval.function(fun, as.data.frame(X), vectorized)

    df <- as.data.frame(X)
    df[[ylab]] <- F_x$y

    inputColumns <- as.list(c(rep(TRUE, D), FALSE))
    names(inputColumns) <- c(Xlab, ylab)

    parview_render(df = df, n_pred = n_points, refColumnDim = ylab,
                   col_fun = col_fun, col_points = NULL, inputColumns = inputColumns,
                   engine = engine, ...)
}


#### parview.matrix ####

#' @param X the matrix of input design.
#' @param y the array of output values.
#' @param col_points color of points.
#' @param Xlab optional character vector of axis labels for inputs.
#' @param ylab optional string label for the output axis.
#' @param engine rendering engine: \code{"parallelPlot"} or \code{"base"}.
#' @param ... extra arguments passed to the rendering engine.
#' @rdname parview
#' @method parview matrix
#' @export
#' @seealso \code{\link{sectionview.matrix}} for 1D section plots.
#' @examples
#' X <- matrix(runif(15 * 2), ncol = 2)
#' y <- apply(X, 1, branin)
#' parview(X, y, engine = "base")
parview.matrix <- function(X, y,
                            col_fun = if (!is.null(col)) col else "blue",
                            col_points = if (!is.null(col)) col else "red",
                            col = NULL,
                            Xlab = NULL, ylab = NULL,
                            engine = "parallelPlot",
                            ...) {
    D <- ncol(X)
    if (is.null(Xlab)) Xlab <- if (!is.null(colnames(X))) colnames(X) else paste0("X", seq_len(D))
    if (is.null(ylab)) ylab <- if (is.matrix(y) && !is.null(colnames(y))) colnames(y)[1] else "y"

    df <- as.data.frame(X)
    colnames(df) <- Xlab
    df[[ylab]] <- as.numeric(y)

    inputColumns <- as.list(c(rep(TRUE, D), FALSE))
    names(inputColumns) <- c(Xlab, ylab)

    # All rows are observations — color all by y (n_pred = nrow)
    parview_render(df = df, n_pred = nrow(df), refColumnDim = ylab,
                   col_fun = col_fun, col_points = col_points, inputColumns = inputColumns,
                   engine = engine, ...)
}


#### parview — libKriging shared helper ####

parview_libKriging <- function(libKriging_model,
                                n_points   = 500,
                                col_fun    = "blue",
                                col_points = "red",
                                conf_level = 0.95,
                                Xlab = NULL, ylab = NULL,
                                Xlim = NULL,
                                engine = "parallelPlot",
                                ...) {
    X_doe <- libKriging_model$X()
    y_doe <- as.numeric(libKriging_model$y())
    D     <- ncol(X_doe)

    if (is.null(Xlab)) Xlab <- if (!is.null(colnames(X_doe))) colnames(X_doe) else paste0("X", seq_len(D))
    if (is.null(ylab)) ylab <- "y"
    if (is.null(Xlim)) Xlim <- apply(X_doe, 2, range)
    Xlim <- matrix(Xlim, nrow = 2, ncol = D)

    lhs    <- DiceDesign::lhsDesign(n_points, D)$design
    X_pred <- sweep(sweep(lhs, 2, Xlim[2, ] - Xlim[1, ], "*"), 2, Xlim[1, ], "+")
    colnames(X_pred) <- Xlab

    p   <- rlibkriging::predict(libKriging_model, X_pred, return_stdev = TRUE)
    z   <- qnorm(1 - (1 - conf_level) / 2)
    ylo <- paste0(ylab, "_low")
    yhi <- paste0(ylab, "_up")

    df_pred <- as.data.frame(X_pred)
    df_pred[[ylab]] <- p$mean
    df_pred[[ylo]]  <- p$mean - z * p$stdev
    df_pred[[yhi]]  <- p$mean + z * p$stdev

    df_doe <- as.data.frame(X_doe)
    colnames(df_doe) <- Xlab
    df_doe[[ylab]] <- y_doe
    df_doe[[ylo]]  <- y_doe   # no CI at conditioning points
    df_doe[[yhi]]  <- y_doe

    df <- rbind(df_pred, df_doe)

    out_cols     <- c(ylab, ylo, yhi)
    inputColumns <- as.list(c(rep(TRUE, D), rep(FALSE, 3)))
    names(inputColumns) <- c(Xlab, out_cols)

    parview_render(df = df, n_pred = n_points, refColumnDim = ylab,
                   col_fun = col_fun, col_points = col_points, inputColumns = inputColumns,
                   engine = engine, ...)
}


#### parview.Kriging ####

#' @param Kriging_model an object of class \code{"Kriging"}.
#' @param n_points number of LHS prediction points.
#' @param col_points color of observed design points.
#' @param conf_level confidence level for uncertainty bands shown as extra axes.
#' @param Xlab optional character vector of axis labels for inputs.
#' @param ylab optional string label for the output axis.
#' @param Xlim optional input bounds matrix (2 x D); defaults to design range.
#' @param engine rendering engine: \code{"parallelPlot"} or \code{"base"}.
#' @param ... extra arguments passed to the rendering engine.
#' @rdname parview
#' @method parview Kriging
#' @export
#' @seealso \code{\link{sectionview.Kriging}} for 1D section plots.
#' @examples
#' if (requireNamespace("rlibkriging")) { library(rlibkriging)
#'   X <- matrix(runif(15 * 2), ncol = 2)
#'   y <- apply(X, 1, branin)
#'   model <- Kriging(X = X, y = as.matrix(y), kernel = "matern3_2")
#'   parview(model, engine = "base")
#' }
parview.Kriging <- function(Kriging_model, n_points = 500,
                             col_fun = if (!is.null(col)) col else "blue",
                             col_points = if (!is.null(col)) col else "red",
                             col = NULL,
                             conf_level = 0.95,
                             Xlab = NULL, ylab = NULL, Xlim = NULL,
                             engine = "parallelPlot", ...) {
    parview_libKriging(Kriging_model,
                       n_points = n_points, col_fun = col_fun, col_points = col_points,
                       conf_level = conf_level,
                       Xlab = Xlab, ylab = ylab, Xlim = Xlim,
                       engine = engine, ...)
}


#### parview.WarpKriging ####

#' @param WarpKriging_model an object of class \code{"WarpKriging"}.
#' @param n_points number of LHS prediction points.
#' @param col_points color of observed design points.
#' @param conf_level confidence level for uncertainty bands shown as extra axes.
#' @param Xlab optional character vector of axis labels for inputs.
#' @param ylab optional string label for the output axis.
#' @param Xlim optional input bounds matrix (2 x D); defaults to design range.
#' @param engine rendering engine: \code{"parallelPlot"} or \code{"base"}.
#' @param ... extra arguments passed to the rendering engine.
#' @rdname parview
#' @method parview WarpKriging
#' @export
#' @seealso \code{\link{sectionview.WarpKriging}} for 1D section plots.
#' @examples
#' if (requireNamespace("rlibkriging")) { library(rlibkriging)
#'   X <- matrix(runif(15 * 2), ncol = 2)
#'   y <- apply(X, 1, branin) + 5 * rnorm(15)
#'   model <- WarpKriging(y = y, X = X, warping = c("affine", "affine"), kernel = "matern3_2")
#'   parview(model, engine = "base")
#' }
parview.WarpKriging <- function(WarpKriging_model, n_points = 500,
                                 col_fun = if (!is.null(col)) col else "blue",
                                 col_points = if (!is.null(col)) col else "red",
                                 col = NULL,
                                 conf_level = 0.95,
                                 Xlab = NULL, ylab = NULL, Xlim = NULL,
                                 engine = "parallelPlot", ...) {
    parview_libKriging(WarpKriging_model,
                       n_points = n_points, col_fun = col_fun, col_points = col_points,
                       conf_level = conf_level,
                       Xlab = Xlab, ylab = ylab, Xlim = Xlim,
                       engine = engine, ...)
}


#### parview.km ####

#' @param km_model an object of class \code{"km"}.
#' @param type the kriging type to use for model prediction.
#' @param n_points number of LHS prediction points.
#' @param col_points color of observed design points.
#' @param conf_level confidence level for uncertainty bands shown as extra axes.
#' @param Xlab optional character vector of axis labels for inputs.
#' @param ylab optional string label for the output axis.
#' @param Xlim optional input bounds matrix (2 x D); defaults to design range.
#' @param engine rendering engine: \code{"parallelPlot"} or \code{"base"}.
#' @param ... extra arguments passed to the rendering engine.
#' @rdname parview
#' @method parview km
#' @export
#' @seealso \code{\link{sectionview.km}} for 1D section plots.
#' @examples
#' if (requireNamespace("DiceKriging")) { library(DiceKriging)
#'   X <- matrix(runif(15 * 2), ncol = 2)
#'   y <- apply(X, 1, branin)
#'   model <- km(design = X, response = y, covtype = "matern3_2")
#'   parview(model, engine = "base")
#' }
parview.km <- function(km_model, type = "UK", n_points = 500,
                        col_fun = if (!is.null(col)) col else "blue",
                        col_points = if (!is.null(col)) col else "red",
                        col = NULL,
                        conf_level = 0.95,
                        Xlab = NULL, ylab = NULL, Xlim = NULL,
                        engine = "parallelPlot", ...) {
    X_doe <- km_model@X
    y_doe <- as.numeric(km_model@y)
    D     <- ncol(X_doe)

    if (is.null(Xlab)) Xlab <- if (!is.null(colnames(X_doe))) colnames(X_doe) else paste0("X", seq_len(D))
    if (is.null(ylab)) ylab <- if (!is.null(colnames(km_model@y))) colnames(km_model@y)[1] else "y"
    if (is.null(Xlim)) Xlim <- apply(X_doe, 2, range)
    Xlim <- matrix(Xlim, nrow = 2, ncol = D)

    lhs    <- DiceDesign::lhsDesign(n_points, D)$design
    X_pred <- sweep(sweep(lhs, 2, Xlim[2, ] - Xlim[1, ], "*"), 2, Xlim[1, ], "+")
    colnames(X_pred) <- Xlab

    p   <- DiceKriging::predict.km(km_model, type = type, newdata = X_pred, checkNames = FALSE)
    z   <- qnorm(1 - (1 - conf_level) / 2)
    ylo <- paste0(ylab, "_low")
    yhi <- paste0(ylab, "_up")

    df_pred <- as.data.frame(X_pred)
    df_pred[[ylab]] <- p$mean
    df_pred[[ylo]]  <- p$mean - z * p$sd
    df_pred[[yhi]]  <- p$mean + z * p$sd

    df_doe <- as.data.frame(X_doe)
    colnames(df_doe) <- Xlab
    df_doe[[ylab]] <- y_doe
    df_doe[[ylo]]  <- y_doe
    df_doe[[yhi]]  <- y_doe

    df <- rbind(df_pred, df_doe)

    out_cols     <- c(ylab, ylo, yhi)
    inputColumns <- as.list(c(rep(TRUE, D), rep(FALSE, 3)))
    names(inputColumns) <- c(Xlab, out_cols)

    parview_render(df = df, n_pred = n_points, refColumnDim = ylab,
                   col_fun = col_fun, col_points = col_points, inputColumns = inputColumns,
                   engine = engine, ...)
}


#### parview.glm ####

#' @param glm_model an object of class \code{"glm"}.
#' @param n_points number of LHS prediction points.
#' @param col_points color of observed design points.
#' @param conf_level confidence level for uncertainty bands shown as extra axes.
#' @param Xlab optional character vector of axis labels for inputs.
#' @param ylab optional string label for the output axis.
#' @param Xlim optional input bounds matrix (2 x D); defaults to design range.
#' @param engine rendering engine: \code{"parallelPlot"} or \code{"base"}.
#' @param ... extra arguments passed to the rendering engine.
#' @rdname parview
#' @method parview glm
#' @export
#' @seealso \code{\link{sectionview.glm}} for 1D section plots.
#' @examples
#' x1 <- rnorm(15); x2 <- rnorm(15)
#' y <- x1 + x2^2 + rnorm(15)
#' model <- glm(y ~ x1 + I(x2^2))
#' parview(model, engine = "base")
parview.glm <- function(glm_model, n_points = 500,
                         col_fun = if (!is.null(col)) col else "blue",
                         col_points = if (!is.null(col)) col else "red",
                         col = NULL,
                         conf_level = 0.95,
                         Xlab = NULL, ylab = NULL, Xlim = NULL,
                         engine = "parallelPlot", ...) {
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

    lhs      <- DiceDesign::lhsDesign(n_points, D)$design
    X_pred   <- sweep(sweep(lhs, 2, Xlim[2, ] - Xlim[1, ], "*"), 2, Xlim[1, ], "+")
    X_pred_df <- as.data.frame(X_pred)
    colnames(X_pred_df) <- Xlab_model

    p   <- predict.glm(glm_model, newdata = X_pred_df, se.fit = TRUE)
    z   <- qnorm(1 - (1 - conf_level) / 2)
    ylo <- paste0(ylab, "_low")
    yhi <- paste0(ylab, "_up")

    df_pred <- as.data.frame(X_pred)
    colnames(df_pred) <- Xlab
    df_pred[[ylab]] <- p$fit
    df_pred[[ylo]]  <- p$fit - z * p$se.fit
    df_pred[[yhi]]  <- p$fit + z * p$se.fit

    df_doe <- as.data.frame(X_doe)
    df_doe[[ylab]] <- y_doe
    df_doe[[ylo]]  <- y_doe
    df_doe[[yhi]]  <- y_doe

    df <- rbind(df_pred, df_doe)

    out_cols     <- c(ylab, ylo, yhi)
    inputColumns <- as.list(c(rep(TRUE, D), rep(FALSE, 3)))
    names(inputColumns) <- c(Xlab, out_cols)

    parview_render(df = df, n_pred = n_points, refColumnDim = ylab,
                   col_fun = col_fun, col_points = col_points, inputColumns = inputColumns,
                   engine = engine, ...)
}


#### parview.list (DiceEval modelFit) ####

#' @param modelFit_model an object returned by \code{DiceEval::modelFit}.
#' @param n_points number of LHS prediction points.
#' @param col_points color of observed design points.
#' @param Xlab optional character vector of axis labels for inputs.
#' @param ylab optional string label for the output axis.
#' @param Xlim optional input bounds matrix (2 x D); defaults to design range.
#' @param engine rendering engine: \code{"parallelPlot"} or \code{"base"}.
#' @param ... extra arguments passed to the rendering engine.
#' @rdname parview
#' @method parview list
#' @export
#' @seealso \code{\link{sectionview.list}} for 1D section plots.
#' @examples
#' if (requireNamespace("DiceEval")) { library(DiceEval)
#'   X <- matrix(runif(15 * 2), ncol = 2)
#'   y <- apply(X, 1, branin)
#'   model <- modelFit(X, y, type = "StepLinear")
#'   parview(model, engine = "base")
#' }
parview.list <- function(modelFit_model, n_points = 500,
                          col_fun = if (!is.null(col)) col else "blue",
                          col_points = if (!is.null(col)) col else "red",
                          col = NULL,
                          Xlab = NULL, ylab = NULL, Xlim = NULL,
                          engine = "parallelPlot", ...) {
    X_doe <- modelFit_model$data$X
    y_doe <- modelFit_model$data$Y
    D     <- ncol(X_doe)

    if (is.null(Xlab)) Xlab <- if (!is.null(colnames(X_doe))) colnames(X_doe) else paste0("X", seq_len(D))
    if (is.null(ylab)) ylab <- if (!is.null(colnames(y_doe))) colnames(y_doe)[1] else "y"
    if (is.null(Xlim)) Xlim <- apply(X_doe, 2, range)
    Xlim <- matrix(Xlim, nrow = 2, ncol = D)

    lhs    <- DiceDesign::lhsDesign(n_points, D)$design
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

    parview_render(df = df, n_pred = n_points, refColumnDim = ylab,
                   col_fun = col_fun, col_points = col_points, inputColumns = inputColumns,
                   engine = engine, ...)
}


#### S3 generic ####

#' @import methods
if (!isGeneric("parview")) {
    setGeneric("parview", def = function(...) standardGeneric("parview"))
}

#' @title Parallel coordinates view of a prediction model or function.
#' @description Renders a parallel coordinates chart showing all input dimensions
#'   and the output simultaneously.  Lines are colored by the output value (viridis
#'   palette), making it easy to identify which input regions drive high or low
#'   responses.
#'
#'   For model-based methods (\code{km}, \code{Kriging}, \code{WarpKriging},
#'   \code{glm}, \code{list}) a space-filling LHS prediction grid is displayed
#'   together with the observed design points (in \code{col_points} color).
#'   Kriging-based methods also add \code{y_low} / \code{y_up} axes for the
#'   predictive confidence interval.
#'
#'   Two rendering engines are available via the \code{engine} argument:
#'   \describe{
#'     \item{\code{"parallelPlot"}}{Interactive htmlwidget (default), requires the
#'       \pkg{parallelPlot} package.}
#'     \item{\code{"base"}}{Static base-R plot, no extra dependency.}
#'   }
#'
#' @param ... arguments of the relevant \code{parview.*} method.
#' @export
#' @examples
#' ## Static base-R plot
#' parview(branin, Xlim = rbind(c(0, 0), c(1, 1)), engine = "base")
#'
#' ## Design points only
#' X <- matrix(runif(30 * 2), ncol = 2)
#' y <- apply(X, 1, branin)
#' parview(X, y, engine = "base")
parview <- function(...) {
    UseMethod("parview")
}
