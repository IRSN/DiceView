#' @param fun a function or 'predict()'-like function that returns a simple numeric, or an interval, or mean and standard error: list(mean=...,se=...).
#' @param vectorized is fun vectorized?
#' @param col_fun color of the function plot.
#' @param col_fading_interval an optional factor of alpha (color channel) fading used to plot function output intervals (if any).
#' @template sectionview3d-doc
#' @rdname sectionview3d
#' @method sectionview3d function
#' @aliases sectionview3d,function,function-method
#' @export
#' @seealso \code{\link{sectionview.function}} for a section plot, and \code{\link{sectionview3d.function}} for a 2D section plot.
#' @examples
#' x1 <- rnorm(15)
#' x2 <- rnorm(15)
#'
#' y <- x1 + x2 + rnorm(15)
#' model <- lm(y ~ x1 + x2)
#'
#' sectionview3d(function(x) sum(x),
#'                        Xlim=cbind(range(x1),range(x2)), col='black')
#' DiceView:::.plot3d(x1, x2, y)
#'
#' sectionview3d(function(x) {
#'                       x = as.data.frame(x)
#'                       colnames(x) <- all.vars(model$call)[-1]
#'                       p = predict.lm(model, newdata=x, se.fit=TRUE)
#'                       list(mean=p$fit, se=p$se.fit)
#'                     }, vectorized=TRUE,
#'               add=TRUE)
#'
sectionview3d.function <- function(fun, vectorized=FALSE,
                             center = NULL,
                             axis = NULL,
                             npoints = 21,
                             col_fun = if (!is.null(col)) col else "blue",
                             col = NULL,
                             col_fading_interval = 0.5,
                             mfrow = c(1,1),
                             Xlab = NULL, ylab = NULL,
                             Xlim = if (!add) matrix(c(0,1),2,2) else NULL, ylim = NULL,
                             title = NULL, title_sep = " | ",
                             add = FALSE,
                             engine3d = NULL,
                             ...) {
    engine3d = .load3d(engine3d)

    # setup for center/Xlim/axis/mfrow
    view = getView(2, add, center, Xlim, ylim, axis, mfrow)
    D = view$D
    center = view$center
    Xlim = view$Xlim
    ylim = view$ylim
    axis = view$axis
    mfrow = view$mfrow

    if (D == 1) stop("for a model with dim 1, use 'sectionview'")

    if (engine3d == "rgl" && D>2 && nrow(axis)!=1) {
        stop("only one plot supported for rgl engine. Set axis=c(.,.)")
    }

    if (!add && (is.null(ylim) || all(is.na(ylim)))) { # compute and set ylim
        ylim <- range(unlist(EvalInterval.function(fun,Xlim, vectorized)), na.rm = TRUE)
        setView_ylim(ylim)
    }

    npoints <- rep(npoints, length.out = D)
    drx <- unlist(Xlim[2, ]) - unlist(Xlim[1, ])

    ## define X & y labels
    if (is.null(ylab)) ylab <- "y"
    if (is.null(Xlab)) Xlab <- paste(sep = "", "X", 1:D)

    ## try to find a good formatted value 'fcenter' for 'center'
    fcenter <- tryFormat(x = center, drx = drx)

    ## Each 'id' will produce a RGL plot
    for (id in 1:dim(axis)[1]) {
        if (D>2 && engine3d != "rgl") screen(id, new=!add)

        d <- axis[id, ]

        npoints_all <- npoints[d[1]]*npoints[d[2]]

        ## ind.nonfix flags the non fixed dims
        ind.nonfix <- (1:D) %in% c(d[1], d[2])
        ind.nonfix <- !ind.nonfix

        x1lim <- Xlim[, d[1]]
        x2lim <- Xlim[, d[2]]

        xd1 <- seq(from = as.numeric(x1lim[1]), to = as.numeric(x1lim[2]), length.out = npoints[d[1]])
        xd2 <- seq(from = as.numeric(x2lim[1]), to = as.numeric(x2lim[2]), length.out = npoints[d[2]])

        x <- data.frame(t(matrix(as.numeric(center), nrow = D, ncol = npoints_all)))
        if (!is.null(center)) if(!is.null(names(center))) names(x) <- names(center)
        x[ , d] <- expand.grid(xd1, xd2)

        F_x = EvalInterval.function(fun, x, vectorized)
        F_x$yd <- matrix(F_x$y,ncol=npoints[2],nrow=npoints[1])
        F_x$yd_low <- matrix(F_x$y_low,ncol=npoints[2],nrow=npoints[1])
        F_x$yd_up <- matrix(F_x$y_up,ncol=npoints[2],nrow=npoints[1])

        if (is.null(title)){
            title_d <- paste(collapse = "~",sep = "~", ylab, paste(collapse = ",", sep = ",", Xlab[d[1]], Xlab[d[2]]))
            if (D>2) {
                title_d <-  paste(collapse =title_sep, sep =title_sep, title_d, paste(collapse=',',Xlab[ind.nonfix],'=', fcenter[ind.nonfix]))
            }
        } else {
            title_d <-  title
        }

        if (isTRUE(add)) {
            # do nothing
        } else {
            # empty plot
            .open3d()
            .plot3d(x = x[ , 1], y = x[ , 2], z = if (!all(is.na(F_x$y))) F_x$y else F_x$y_low,
                xlab = Xlab[d[1]], ylab = Xlab[d[2]], zlab = ylab,
                xlim = x1lim, ylim = x2lim, zlim = ylim,
                type = "n",
                main = title_d,
                col = col_fun,
                ...)
        }
        ## plot mean
	    if (!any(is.na(F_x$yd)))
	        .surface3d(x = xd1,y = xd2, z = F_x$yd,
                    col = col_fun, alpha = 0.5,
                    box = FALSE)

        ## 'confidence band' filled with the suitable color
	    if (!all(is.na(F_x$yd_low)) && !all(is.na(F_x$yd_up))) {
	        .surface3d(x = xd1,
                        y = xd2,
                        z =  F_x$yd_low,
                        col = col_fun,
                        alpha = col_fading_interval,
                        box = FALSE)

	        .surface3d(x = xd1,
                        y = xd2,
                        z = F_x$yd_up,
                        col = col_fun,
                        alpha = col_fading_interval,
                        box = FALSE)
	    }
    }
}


#' @param X the matrix of input design.
#' @param y the array of output values (two columns means an interval).
#' @param col_points color of points.
#' @param col_fading_interval an optional factor of alpha (color channel) fading used to plot confidence intervals.
#' @param bg_fading  an optional factor of alpha (color channel) fading used to plot design points outside from this section.
#' @template sectionview3d-doc
#' @rdname sectionview3d
#' @method sectionview3d matrix
#' @aliases sectionview3d,matrix,matrix-method
#' @export
#' @seealso \code{\link{sectionview.matrix}} for a section plot, and \code{\link{sectionview3d.matrix}} for a 2D section plot.
#' @examples
#' X = matrix(runif(15*2),ncol=2)
#' y = apply(X,1,branin)
#'
#' sectionview3d(X, y)
#'
sectionview3d.matrix <- function(X, y,
                             center = NULL,
                             axis = NULL,
                             col_points = if (!is.null(col)) col else "red",
                             col = NULL,
                             col_fading_interval = 0.5,
                             bg_fading = 1,
                             mfrow = c(1,1),
                             Xlab = NULL, ylab = NULL,
                             Xlim = if (!add) matrix(c(0,1),2,2) else NULL, ylim = NULL,
                             title = NULL, title_sep = " | ",
                             add = FALSE,
                             engine3d = NULL,
                             ...) {
    engine3d = .load3d(engine3d)

    # setup for center/Xlim/axis/mfrow
    view = getView(1, add, center, if (is.null(Xlim) && !add) apply(X, 2, range) else Xlim, if (is.null(ylim) && !add) ylim <- range(y) else ylim, axis, mfrow)
    D = view$D
    center = view$center
    Xlim = view$Xlim
    ylim = view$ylim
    axis = view$axis
    mfrow = view$mfrow

    if (D == 1) stop("for a model with dim 1, use 'sectionview'")

    if (engine3d == "rgl" && D>2 && nrow(axis)!=1) {
        stop("only one plot supported for rgl engine. Set axis=c(.,.)")
    }

    if (ncol(X) != D)
        stop(paste0("X must have ",D," columns."))
    n <- nrow(X)

    if (is.matrix(y) && ncol(y) == 2) {
        y_low <- y[ , 1]
        y_up <- y[ , 2]
        y <- NA
    } else {
        y_low <- NA
        y_up <- NA
    }

    drx <- unlist(Xlim[2, ]) - unlist(Xlim[1, ])

    ## define X & y labels
    if (is.null(ylab) && !is.null(names(y))) ylab <- names(y)[1]
    if (is.null(Xlab)) Xlab <- names(X)

    ## define X & y labels
    if (is.null(ylab)) ylab <- "y"
    if (is.null(Xlab)) Xlab <- paste(sep = "", "X", 1:D)

    fcenter <- tryFormat(x = center, drx = drx)

    ## Each 'id' will produce a plot
    for (id in 1:dim(axis)[1]) {
        if (D>2 && engine3d != "rgl") screen(id, new=!add)

        d <- axis[id,]

        ## ind.nonfix flags the non fixed dims
        ind.nonfix <- (1:D) %in% c(d[1], d[2])
        ind.nonfix <- !ind.nonfix

        x1lim = Xlim[, d[1]]
        x2lim = Xlim[, d[2]]

        if (is.null(title)){
            title_d <- paste(collapse = "~",sep = "~", ylab, paste(collapse = ",", sep = ",", Xlab[d[1]], Xlab[d[2]]))
            if (D>2) {
                title_d <-  paste(collapse =title_sep, sep =title_sep, title_d, paste(collapse=',',Xlab[ind.nonfix],'=', fcenter[ind.nonfix]))
            }
        } else {
            title_d <- title
        }

        ## fading colors for points
        if (D>2) {
            xrel <- scale(x = as.matrix(X),
                          center = center,
                          scale = drx)

            ## ind.nonfix flags the non fixed dims
            ind.nonfix <- (1:D) %in% c(d[1], d[2])
            ind.nonfix <- !ind.nonfix

            alpha <- pmax(0,apply(X = xrel[ , ind.nonfix, drop = FALSE],
                           MARGIN = 1,
                           FUN = function(x) (1 - sqrt(sum(x^2)/D))^bg_fading))
        } else {
            alpha <- rep(1, n)
        }

        if (isTRUE(add)) {
            .points3d(x = X[ , d[1]], y = X[ , d[2]], z = if (!all(is.na(y))) y else y_low,
                     col = col_points,
                     alpha = alpha,
                     pch = 20, size=if (!all(is.na(y))) 3 else 0, box = FALSE,
                     xlim=x1lim, ylim=x2lim, zlim=ylim)
        } else {
            .open3d()
            .plot3d(x = X[ , d[1]], y = X[ , d[2]], z = if (!all(is.na(y))) y else y_low,
                     col = col_points,
                     alpha = alpha,
                     pch = 20, size=if (!all(is.na(y))) 3 else 0, box = TRUE,
                     xlab=Xlab[d], ylab=ylab,
                     xlim=x1lim, ylim=x2lim, zlim=ylim)
        }

        if (!all(is.na(y_low)) && !all(is.na(y_up)))
            #for (p in 1:length(conf_level)) {
                for (i in 1:n) {
                    .lines3d(x = c(X[i, d[1]], X[i, d[1]]),
                    y = c(X[i, d[2]], X[i, d[2]]),
                    z = c(y_low[i], y_up[i]),
                    col = col_points,
                    alpha = alpha[i]*col_fading_interval,
                    lwd = 5, lend = 1, box = FALSE)
                }
            #}
        # else
        #     for (p in 1:length(conf_level)) {
        #         for (i in 1:n) {
        #             .points3d(x = X_doe[i, d[1]],
        #              y = X_doe[i, d[2]],
        #              z = y_doe[i],
        #              col = col_points,
        #              alpha = alpha[i]*conf_fading,
        #              pch = 15, box = FALSE)
        #         }}
    }
}


#' @param eval_str the expression to evaluate in each subplot.
#' @param axis optional matrix of 2-axis combinations to plot, one by row. The value \code{NULL} leads to all possible combinations i.e. \code{choose(D, 2)}.
#' @param mfrow  an optional list to force \code{par(mfrow = ...)} call. The default value  \code{NULL} is automatically set for compact view.
#' @rdname sectionview3d
#' @method sectionview3d character
#' @aliases sectionview3d,character,character-method
#' @export
#' @seealso \code{\link{sectionview3d.matrix}} for a section plot.
#' @examples
#' x1 <- rnorm(15)
#' x2 <- rnorm(15)
#'
#' y <- x1 + x2^2 + rnorm(15)
#' model <- glm(y ~ x1 + I(x2^2))
#'
#' sectionview3d(model)
#'
#' sectionview3d("abline(h=0.25,col='red')")
sectionview3d.character <- function(eval_str,
                                  axis = NULL,
                                  mfrow = c(1,1),
                                  ...) {

    if (!exists(".split.screen.lim",envir=DiceView.env))
        stop(paste0("Cannot eval '",eval_str,"' when no previous sectionview() was called."))

    # setup for center/Xlim/axis/mfrow
    view = getView(2, TRUE, NA, NA, NA, axis, mfrow)
    D = view$D
    center = view$center
    Xlim = view$Xlim
    ylim = view$ylim
    axis = view$axis
    mfrow = view$mfrow

    ## Each 'id' will produce a plot
    for (id in 1:dim(axis)[1]) {

        d <- axis[id,]

        e = parent.frame()
        assign("d",d,envir=e)
        assign("xlim",Xlim[,d[1]],envir=e)
        assign("ylim",Xlim[,d[2]],envir=e)
        assign("zlim",ylim,envir=e)

        if (D>2) {
            screen(id, new=FALSE)
            .plot3d(x=Xlim[,d[1]], y=Xlim[,d[2]],z=ylim,
                 type='n', box = FALSE,
                 xlab="",ylab="",zlab="",
                 xlim=Xlim[,d[1]], ylim=Xlim[,d[2]], zlim=ylim,
                 ...)
        }

        eval(parse(text = eval_str), envir = e)
    }
}

#' @param km_model an object of class \code{"km"}.
#' @param type the kriging type to use for model prediction.
#' @param col_points color of points.
#' @param conf_level confidence intervals to display.
#' @param conf_fading an optional factor of alpha (color channel) fading used to plot confidence intervals.
#' @param bg_fading  an optional factor of alpha (color channel) fading used to plot design points outside from this section.
#' @template sectionview3d-doc
#' @rdname sectionview3d
#' @method sectionview3d km
#' @aliases sectionview3d,km,km-method
#' @export
#' @seealso \code{\link{sectionview.km}} for a section plot, and \code{\link{sectionview3d.km}} for a 2D section plot.
#' @examples
#' if (requireNamespace("DiceKriging")) { library(DiceKriging)
#'
#' X = matrix(runif(15*2),ncol=2)
#' y = apply(X,1,branin)
#'
#' model <- km(design = X, response = y, covtype="matern3_2")
#'
#' sectionview3d(model)
#'
#' }
#'
sectionview3d.km <- function(km_model, type = "UK",
                           center = NULL,
                           axis = NULL,
                           npoints = 21,
                           col_points = if (!is.null(col)) col else "red",
                           col_fun = if (!is.null(col)) col else "blue",
                           col = NULL,
                           conf_level = 0.95,
                           conf_fading = 0.5,
                           bg_fading = 1,
                           mfrow = c(1,1),
                           Xlab = NULL, ylab = NULL,
                           Xlim = NULL, ylim = NULL,
                           title = NULL, title_sep = " | ",
                           add = FALSE,
                           engine3d = NULL,
                           ...) {
    X_doe <- km_model@X
    y_doe <- km_model@y

    D <- ncol(X_doe)
    n <- nrow(X_doe)

    if (km_model@noise.flag) {
        sdy_doe <- sqrt(km_model@noise.var)
    } else if (km_model@covariance@nugget.flag) {
        sdy_doe <- rep(sqrt(km_model@covariance@nugget), n)
    } else {
        sdy_doe <- 0
    }

    ## find limits
    if (is.null(Xlim) && !add)
        Xlim <- apply(X_doe, 2, range)
    else if (!is.null(Xlim))
        Xlim <- matrix(Xlim,nrow=2,ncol=D)

    if (is.null(ylim) && !add) {
        ymin <- min(y_doe-3*sdy_doe)
        ymax <- max(y_doe+3*sdy_doe)
        ylim <- c(ymin, ymax)
    }

    ## define X & y labels
    if (is.null(ylab)) ylab <- names(y_doe)
    if (is.null(Xlab)) Xlab <- names(X_doe)

    if (is.null(conf_fading) ||
        length(conf_fading) != length(conf_level)) {
        conf_fading <- rep(0.5/length(conf_level), length(conf_level))
    }

    # plot mean
    sectionview3d.function(fun = function(x) {
        DiceKriging::predict.km(km_model,type=type,newdata=x,checkNames=FALSE)$mean
    }, vectorized=TRUE,
    center = center, axis = axis, mfrow = mfrow, Xlim = Xlim, ylim=ylim,
    npoints = npoints,
    col_fun = col_fun, #conf_fading=conf_fading,
    Xlab = Xlab, ylab = ylab,
    title = title, title_sep  = title_sep, add = add, engine3d=engine3d, ...)

    if (!km_model@noise.flag)
        # plot design points
        sectionview3d.matrix(X = X_doe, y = y_doe,
                       col_points = col_points,
                       col_fading_interval = conf_fading, bg_fading = bg_fading,
                       add=TRUE)

    # plot confidence bands
    for (l in conf_level) {
        sectionview3d.function(fun = function(x) {
                p = DiceKriging::predict.km(km_model,type=type,newdata=x,checkNames=FALSE)
                cbind(p$mean-qnorm(1-(1-l)/2) * p$sd, p$mean+qnorm(1-(1-l)/2) * p$sd)
            }, vectorized=TRUE,
            npoints = npoints,
            col_fun = col_fun,
            col_fading_interval=conf_fading,
            add = TRUE, engine3d=engine3d)

        if (km_model@noise.flag)
            sectionview3d.matrix(X = X_doe, y = cbind(y_doe-qnorm(1-(1-l)/2) * sdy_doe, y_doe+qnorm(1-(1-l)/2) * sdy_doe),
                           col_points = col_points,
                           col_fading_interval = conf_fading, bg_fading = bg_fading,
                           add=TRUE, engine3d=engine3d)
    }

}


#' @param libKriging_model an object of class \code{"Kriging"}, \code{"NuggetKriging"} or \code{"NoiseKriging"}.
#' @param col_points color of points.
#' @param bg_fading  an optional factor of alpha (color channel) fading used to plot design points outside from this section.
sectionview3d_libKriging <- function(libKriging_model,
                           center = NULL,
                           axis = NULL,
                           npoints = 21,
                           col_points = if (!is.null(col)) col else "red",
                           col_fun = if (!is.null(col)) col else "blue",
                           col = NULL,
                           conf_level = 0.95,
                           conf_fading = 0.5,
                           bg_fading = 1,
                           mfrow = c(1,1),
                           Xlab = NULL, ylab = NULL,
                           Xlim = NULL, ylim = NULL,
                           title = NULL, title_sep = " | ",
                           add = FALSE,
                           engine3d = NULL,
                           ...) {
    X_doe <- libKriging_model$X()
    y_doe <- libKriging_model$y()

    D <- ncol(X_doe)
    n <- nrow(X_doe)

    if (inherits(libKriging_model, "Kriging")) {
        sdy_doe <- 0
    } else if (inherits(libKriging_model, "NuggetKriging")) {
        sdy_doe <- rep(sqrt(libKriging_model$nugget()),n)
    } else if (inherits(libKriging_model, "NoiseKriging")) {
        sdy_doe <- sqrt(libKriging_model$noise())
    } else
        stop(paste0("Kriging model of class ",class(libKriging_model)," is not yet supported."))

    ## find limits
    if (is.null(Xlim) && !add)
        Xlim <- apply(X_doe, 2, range)
    else if (!is.null(Xlim))
        Xlim <- matrix(Xlim,nrow=2,ncol=D)

    if (is.null(ylim) && !add) {
        if (is.null(sdy_doe))
            ylim = range(y_doe)
        else {
            ymin <- min(y_doe-3*sdy_doe)
            ymax <- max(y_doe+3*sdy_doe)
            ylim <- c(ymin, ymax)
        }
    }

    ## define X & y labels
    if (is.null(ylab)) ylab <- names(y_doe)
    if (is.null(ylab)) ylab <- "y"
    if (is.null(Xlab)) Xlab <- names(X_doe)
    if (is.null(Xlab)) Xlab <- paste(sep = "", "X", 1:D)

    if (is.null(conf_fading) ||
        length(conf_fading) != length(conf_level))
        conf_fading <- rep(0.5/length(conf_level), length(conf_level))

    sectionview3d.function(fun = function(x) {
            rlibkriging::predict(libKriging_model,x,return_stdev=FALSE)$mean
        }, vectorized=TRUE,
        center = center, axis = axis, mfrow = mfrow, Xlim = Xlim, ylim=ylim,
        npoints = npoints,
        col_fun = col_fun, #conf_fading=conf_fading,
        Xlab = Xlab, ylab = ylab,
        title = title, title_sep  = title_sep, add = add, engine3d=engine3d, ...)

    if (! inherits(libKriging_model, "NoiseKriging"))
        # plot design points
        sectionview3d.matrix(X = X_doe, y = y_doe,
                       col_points = col_points,
                       col_fading_interval = conf_fading, bg_fading = bg_fading,
                       add=TRUE, engine3d=engine3d)

    # plot confidence bands
    for (l in conf_level) {
        sectionview3d.function(fun = function(x) {
                p = rlibkriging::predict(libKriging_model,x,return_stdev=TRUE)
                cbind(p$mean-qnorm(1-(1-l)/2) * p$stdev, p$mean+qnorm(1-(1-l)/2) * p$stdev)
            }, vectorized=TRUE,
            npoints = npoints,
            col_fun = col_fun,
            col_fading_interval=conf_fading,
            add = TRUE, engine3d=engine3d)

        if (inherits(libKriging_model, "NoiseKriging")) # so sdy_doe=0
            sectionview3d.matrix(X = X_doe, y = cbind(y_doe-qnorm(1-(1-l)/2) * sdy_doe, y_doe+qnorm(1-(1-l)/2) * sdy_doe),
                           col_points = col_points,
                           col_fading_interval = conf_fading, bg_fading = bg_fading,
                           add=TRUE, engine3d=engine3d)
    }

}

#' @param Kriging_model an object of class \code{"Kriging"}.
#' @param col_points color of points.
#' @param conf_level confidence intervals to display.
#' @param conf_fading an optional factor of alpha (color channel) fading used to plot confidence intervals.
#' @param bg_fading  an optional factor of alpha (color channel) fading used to plot design points outside from this section.
#' @template sectionview3d-doc
#' @rdname sectionview3d
#' @method sectionview3d Kriging
#' @aliases sectionview3d,Kriging,Kriging-method
#' @export
#' @seealso \code{\link{sectionview.Kriging}} for a section plot, and \code{\link{sectionview3d.Kriging}} for a 2D section plot.
#' @examples
#' if (requireNamespace("rlibkriging")) { library(rlibkriging)
#'
#' X = matrix(runif(15*2),ncol=2)
#' y = apply(X,1,branin)
#'
#' model <- Kriging(X = X, y = y, kernel="matern3_2")
#'
#' sectionview3d(model)
#'
#' }
#'
sectionview3d.Kriging <- function(Kriging_model,
                                   center = NULL,
                                   axis = NULL,
                                   npoints = 21,
                                   col_points = if (!is.null(col)) col else "red",
                                   col_fun = if (!is.null(col)) col else "blue",
                                   col = NULL,
                                   conf_level = 0.95,
                                   conf_fading = 0.5,
                                   bg_fading = 1,
                                   mfrow = c(1,1),
                                   Xlab = NULL, ylab = NULL,
                                   Xlim = NULL, ylim = NULL,
                                   title = NULL, title_sep = " | ",
                                   add = FALSE,
                                   engine3d = NULL,
                                   ...) {
    sectionview3d_libKriging(Kriging_model,center,axis,npoints,
                             col_points=col_points,col_fun=col_fun,col=col,
                             conf_level,conf_fading,bg_fading,
                             mfrow,Xlab, ylab,Xlim,ylim,title,title_sep,add,engine3d,...)
}

#' @param NuggetKriging_model an object of class \code{"Kriging"}.
#' @param col_points color of points.
#' @param conf_level an optional list of confidence intervals to display.
#' @param conf_fading an optional factor of alpha (color channel) fading used to plot confidence intervals.
#' @param bg_fading  an optional factor of alpha (color channel) fading used to plot design points outside from this section.
#' @template sectionview3d-doc
#' @rdname sectionview3d
#' @method sectionview3d NuggetKriging
#' @aliases sectionview3d,NuggetKriging,NuggetKriging-method
#' @export
#' @seealso \code{\link{sectionview.NuggetKriging}} for a section plot, and \code{\link{sectionview3d.NuggetKriging}} for a 2D section plot.
#' @examples
#' if (requireNamespace("rlibkriging")) { library(rlibkriging)
#'
#' X = matrix(runif(15*2),ncol=2)
#' y = apply(X,1,branin) + 5*rnorm(15)
#'
#' model <- NuggetKriging(X = X, y = y, kernel="matern3_2")
#'
#' sectionview3d(model)
#'
#' }
#'
sectionview3d.NuggetKriging <- function(NuggetKriging_model,
                                center = NULL,
                                axis = NULL,
                                npoints = 21,
                                col_points = if (!is.null(col)) col else "red",
                                col_fun = if (!is.null(col)) col else "blue",
                                col = NULL,
                                conf_level = 0.95,
                                conf_fading = 0.5,
                                bg_fading = 1,
                                mfrow = c(1,1),
                                Xlab = NULL, ylab = NULL,
                                Xlim = NULL, ylim = NULL,
                                title = NULL, title_sep = " | ",
                                add = FALSE,
                                engine3d = NULL,
                                ...) {
    sectionview3d_libKriging(NuggetKriging_model,center,axis,npoints,
                             col_points=col_points,col_fun=col_fun,col=col,
                             conf_level,conf_fading,bg_fading,
                             mfrow,Xlab, ylab,Xlim,ylim,title,title_sep,add,engine3d,...)
}

#' @param NoiseKriging_model an object of class \code{"Kriging"}.
#' @param col_points color of points.
#' @param conf_level an optional list of confidence intervals to display.
#' @param conf_fading an optional factor of alpha (color channel) fading used to plot confidence intervals.
#' @param bg_fading  an optional factor of alpha (color channel) fading used to plot design points outside from this section.
#' @template sectionview3d-doc
#' @rdname sectionview3d
#' @method sectionview3d NoiseKriging
#' @aliases sectionview3d,NoiseKriging,NoiseKriging-method
#' @export
#' @seealso \code{\link{sectionview.NoiseKriging}} for a section plot, and \code{\link{sectionview3d.NoiseKriging}} for a 2D section plot.
#' @examples
#' if (requireNamespace("rlibkriging")) { library(rlibkriging)
#'
#' X = matrix(runif(15*2),ncol=2)
#' y = apply(X,1,branin) + 5*rnorm(15)
#'
#' model <- NoiseKriging(X = X, y = y, kernel="matern3_2", noise=rep(5^2,15))
#'
#' sectionview3d(model)
#'
#' }
#'
sectionview3d.NoiseKriging <- function(NoiseKriging_model,
                                      center = NULL,
                                      axis = NULL,
                                      npoints = 21,
                                      col_points = if (!is.null(col)) col else "red",
                                      col_fun = if (!is.null(col)) col else "blue",
                                      col = NULL,
                                      conf_level = 0.95,
                                      conf_fading = 0.5,
                                      bg_fading = 1,
                                      mfrow = c(1,1),
                                      Xlab = NULL, ylab = NULL,
                                      Xlim = NULL, ylim = NULL,
                                      title = NULL, title_sep = " | ",
                                      add = FALSE,
                                      engine3d = NULL,
                                      ...) {
    sectionview3d_libKriging(NoiseKriging_model,center,axis,npoints,
                             col_points=col_points,col_fun=col_fun,col=col,
                             conf_level,conf_fading,bg_fading,
                             mfrow,Xlab, ylab,Xlim,ylim,title,title_sep,add,engine3d,...)
}

#' @param glm_model an object of class \code{"glm"}.
#' @param col_points color of points.
#' @param conf_level an optional list of confidence intervals to display.
#' @param conf_fading an optional factor of alpha (color channel) fading used to plot confidence intervals.
#' @param bg_fading  an optional factor of alpha (color channel) fading used to plot design points outside from this section.
#' @template sectionview3d-doc
#' @rdname sectionview3d
#' @method sectionview3d glm
#' @aliases sectionview3d,glm,glm-method
#' @export
#' @seealso \code{\link{sectionview.glm}} for a section plot, and \code{\link{sectionview3d.glm}} for a 2D section plot.
#' @examples
#' x1 <- rnorm(15)
#' x2 <- rnorm(15)
#'
#' y <- x1 + x2^2 + rnorm(15)
#' model <- glm(y ~ x1 + I(x2^2))
#'
#' sectionview3d(model)
#'
sectionview3d.glm <- function(glm_model,
                           center = NULL,
                           axis = NULL,
                           npoints = 21,
                           col_points = if (!is.null(col)) col else "red",
                           col_fun = if (!is.null(col)) col else "blue",
                           col = NULL,
                           conf_level = 0.95,
                           conf_fading = 0.5,
                           bg_fading = 1,
                           mfrow = c(1,1),
                           Xlab = NULL, ylab = NULL,
                           Xlim = NULL, ylim = NULL,
                           title = NULL, title_sep = " | ",
                           add = FALSE,
                           engine3d = NULL,
                           ...) {

    # Get X & y labels
    if (is.null(Xlab)) Xlab <- all.vars(glm_model$formula)[-1] # assume just one y
    if (is.null(ylab)) {
        factors_names = all.vars(glm_model$formula)
        for (n in Xlab) {
            factors_names = factors_names[-which(factors_names==n)]
        }
        ylab = factors_names
    }

    D <- length(Xlab)
    n <- length(glm_model$residuals)

    X_doe <- do.call(cbind,lapply(Xlab,function(Xn)glm_model$data[[Xn]]))
    colnames(X_doe) <- Xlab
    y_doe <- do.call(cbind,lapply(ylab,function(yn)glm_model$data[[yn]]))
    colnames(y_doe) <- ylab

    ## find limits
    if (is.null(Xlim) && !add)
        Xlim <- apply(X_doe, 2, range)
    else if (!is.null(Xlim))
        Xlim <- matrix(Xlim,nrow=2,ncol=D)

    if (is.null(conf_fading) ||
        length(conf_fading) != length(conf_level))
        conf_fading <- rep(0.5/length(conf_level), length(conf_level))

    # plot mean
    sectionview3d.function(
        fun = function(x) {
            x = as.data.frame(x)
            colnames(x) <- Xlab
            predict.glm(glm_model, newdata=x, se.fit=FALSE)
        }, vectorized=TRUE,
        center = center,axis = axis, mfrow = mfrow, Xlim = Xlim, ylim=range(y_doe),
        npoints = npoints,
        col_fun = col_fun,
        Xlab = Xlab, ylab = ylab,
        title = title, title_sep  = title_sep, add = add, engine3d=engine3d, ...)

    # plot design points
    sectionview3d.matrix(X = X_doe, y = y_doe,
                         col_points = col_points,
                         col_fading_interval = conf_fading, bg_fading = bg_fading,
                         add=TRUE, engine3d=engine3d)

    # plot confidence bands
    for (l in conf_level) {
        sectionview3d.function(fun = function(x) {
                x = as.data.frame(x)
                colnames(x) <- Xlab
                p = predict.glm(glm_model, newdata=x, se.fit=TRUE)
                cbind(p$fit-qnorm(1-(1-l)/2) * p$se.fit, p$fit+qnorm(1-(1-l)/2) * p$se.fit)
            }, vectorized=TRUE,
            npoints = npoints,
            col_fun = col_fun,
            col_fading_interval=conf_fading,
            add = TRUE)
    }
}


#' @param modelFit_model an object returned by DiceEval::modelFit.
#' @param col_points color of points.
#' @param bg_fading  an optional factor of alpha (color channel) fading used to plot design points outside from this section.
#' @template sectionview3d-doc
#' @rdname sectionview3d
#' @method sectionview3d list
#' @aliases sectionview3d,list,list-method
#' @export
#' @seealso \code{\link{sectionview.glm}} for a section plot, and \code{\link{sectionview3d.glm}} for a 2D section plot.
#' @examples
#' if (requireNamespace("DiceEval")) { library(DiceEval)
#'
#' X = matrix(runif(15*2),ncol=2)
#' y = apply(X,1,branin)
#'
#' model <- modelFit(X, y, type = "StepLinear")
#'
#' sectionview3d(model)
#'
#' }
#'
sectionview3d.list <- function(modelFit_model,
                            center = NULL,
                            axis = NULL,
                            npoints = 21,
                            col_points = if (!is.null(col)) col else "red",
                            col_fun = if (!is.null(col)) col else "blue",
                            col = NULL,
                            bg_fading = 1,
                            mfrow = c(1,1),
                            Xlab = NULL, ylab = NULL,
                            Xlim = NULL, ylim = NULL,
                            title = NULL, title_sep = " | ",
                            add = FALSE,
                            engine3d = NULL,
                            ...) {

    X_doe <- modelFit_model$data$X
    y_doe <- modelFit_model$data$Y

    D <- ncol(X_doe)
    n <- nrow(X_doe)

    ## define X & y labels
    if (is.null(ylab)) ylab <- names(y_doe)
    if (is.null(ylab)) ylab <- "y"
    if (is.null(Xlab)) Xlab <- names(X_doe)
    if (is.null(Xlab)) Xlab <- paste(sep = "", "X", 1:D)

    ## find limits
    if (is.null(Xlim) && !add)
        Xlim <- apply(X_doe, 2, range)
    else if (!is.null(Xlim))
        Xlim <- matrix(Xlim,nrow=2,ncol=D)

    # plot mean
    sectionview3d.function(
        fun = function(x) {
            x = as.data.frame(x)
            colnames(x) <- Xlab
            DiceEval::modelPredict(modelFit_model, x)
        }, vectorized=TRUE,
        center = center,axis = axis,mfrow = mfrow,Xlim = Xlim, ylim=range(y_doe),
        npoints = npoints,
        col_fun = col_fun, #conf_fading=conf_fading,
        Xlab = Xlab, ylab = ylab,
        title = title, title_sep  = title_sep, add = add, engine3d=engine3d, ...)

    sectionview3d.matrix(X = X_doe, y = y_doe,
                         col_points = col_points,
                         bg_fading = bg_fading,
                         add=TRUE, engine3d=engine3d)
}



#### Wrapper for sectionview3d ####

#' @import methods
if(!isGeneric("sectionview3d")) {
    setGeneric(name = "sectionview3d",
               def = function(...) standardGeneric("sectionview3d")
    )
}

#' @title Plot a contour view of a prediction model or function, including design points if available.
#' @details If available, experimental points are plotted with fading colors. Points that fall in the specified section (if any) have the color specified \code{col_points} while points far away from the center have shaded versions of the same color. The amount of fading is determined using the Euclidean distance between the plotted point and \code{center}.
#' @param ... arguments of the \code{sectionview3d.km}, \code{sectionview3d.glm}, \code{sectionview3d.Kriging} or \code{sectionview3d.function} function
#' @export
#' @examples
#' ## A 2D example - Branin-Hoo function
#' sectionview3d(branin, col='black')
#'
#' \dontrun{
#' ## a 16-points factorial design, and the corresponding response
#' d <- 2; n <- 16
#' design.fact <- expand.grid(seq(0, 1, length = 4), seq(0, 1, length = 4))
#' design.fact <- data.frame(design.fact); names(design.fact) <- c("x1", "x2")
#' y <- branin(design.fact); names(y) <- "y"
#'
#' if (requireNamespace("DiceKriging")) { library(DiceKriging)
#' ## model: km
#' model <- DiceKriging::km(design = design.fact, response = y)
#' sectionview3d(model)
#' sectionview3d(branin, col='red', add=TRUE)
#' }
#'
#' if (requireNamespace("rlibkriging")) { library(rlibkriging)
#' ## model: Kriging
#' model <- Kriging(X = as.matrix(design.fact), y = as.matrix(y), kernel="matern3_2")
#' sectionview3d(model)
#' sectionview3d(branin, col='red', add=TRUE)
#' }
#'
#' ## model: glm
#' model <- glm(y ~ 1+ x1 + x2 + I(x1^2) + I(x2^2) + x1*x2, data=cbind(y,design.fact))
#' sectionview3d(model)
#' sectionview3d(branin, col='red', add=TRUE)
#'
#' if (requireNamespace("DiceEval")) { library(DiceEval)
#' ## model: StepLinear
#' model <- modelFit(design.fact, y, type = "StepLinear")
#' sectionview3d(model)
#' sectionview3d(branin, col='red', add=TRUE)
#' }
#' }
#'
sectionview3d <- function(...) {
    UseMethod("sectionview3d")
}
