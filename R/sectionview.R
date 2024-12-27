#' @param fun a function or 'predict()'-like function that returns a simple numeric, or an interval, or mean and standard error: list(mean=...,se=...).
#' @param vectorized is fun vectorized?
#' @param col_fun color of the function plot.
#' @param col_fading_interval an optional factor of alpha (color channel) fading used to plot function output intervals (if any).
#' @template sectionview-doc
#' @rdname sectionview
#' @method sectionview function
#' @aliases sectionview,function,function-method
#' @export
#' @seealso \code{\link{sectionview.function}} for a section plot, and \code{\link{sectionview3d.function}} for a 2D section plot.
#' @examples
#' x1 <- rnorm(15)
#' x2 <- rnorm(15)
#'
#' y <- x1 + x2 + rnorm(15)
#' model <- lm(y ~ x1 + x2)
#'
#' sectionview(function(x) sum(x),
#'                      center=c(0,0), Xlim=cbind(range(x1),range(x2)), col='black')
#'
#' sectionview(function(x) {
#'                       x = as.data.frame(x)
#'                       colnames(x) <- all.vars(model$call)[-1]
#'                       p = predict.lm(model, newdata=x, se.fit=TRUE)
#'                       cbind(p$fit-1.96 * p$se.fit, p$fit+1.96 * p$se.fit)
#'                     }, vectorized=TRUE, add=TRUE)
#'
sectionview.function <- function(fun, vectorized=FALSE,
                             center = NULL,
                             lty_center = 2,
                             col_center = "black",
                             axis = NULL,
                             npoints = 101,
                             col_fun = if (!is.null(col)) col else "blue",
                             col = NULL,
                             col_fading_interval = 0.5,
                             mfrow = NULL,
                             Xlab = NULL, ylab = NULL,
                             Xlim = if (!add) c(0,1) else NULL, ylim = NULL,
                             title = NULL, title_sep = " | ",
                             add = FALSE,
                             ...) {
    # setup for center/Xlim/axis/mfrow
    view = getView(1, add, center, Xlim, ylim, axis, mfrow)
    D = view$D
    center = view$center
    Xlim = view$Xlim
    ylim = view$ylim
    axis = view$axis
    mfrow = view$mfrow

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

    ## Each 'id' will produce a plot
    for (id in 1:dim(axis)[1]) {
        if (D>1) screen(id, new=!add)

        d <- axis[id, ]

        xlim = Xlim[, d]

        xd <- seq(from = xlim[1], to = xlim[2], length.out = npoints[d])

        x <- data.frame(t(matrix(as.numeric(center), nrow = D, ncol = npoints[d])))
        if (!is.null(center)) if(!is.null(names(center))) names(x) <- names(center)
        x[ , d] <- xd

        F_x = EvalInterval.function(fun, x, vectorized)

        if (is.null(title)){
            title_d <- paste(collapse = "~",sep = "~", ylab, Xlab[d])
            if (D>1) {
                title_d <-  paste(collapse =title_sep, sep =title_sep, title_d, paste(collapse=',',Xlab[-d], '=', fcenter[-d]))
            }
        } else {
            title_d <- title
        }

        ## plot mean
        if (isTRUE(add)) {
            if (D>1) {
                plot(xd, if (!all(is.na(F_x$y))) F_x$y else F_x$y_low,
                     xlim = xlim, ylim  =ylim,
                     type = if (!all(is.na(F_x$y))) "l" else "n",
                     col = col_fun, xlab="", ylab="",
                     bty='n', xaxt='n', yaxt='n', ann=FALSE,# remove all text, that should be already displayed
                     ...)
            } else { # not using screen(), so need for a non reset plotting method
                lines(xd, if (!all(is.na(F_x$y))) F_x$y else F_x$y_low,
                      xlim = xlim, ylim = ylim,
                      type = if (!all(is.na(F_x$y))) "l" else "n",
                      col = col_fun,
                      ...)
            }
        } else {
            plot(xd, if (!all(is.na(F_x$y))) F_x$y else F_x$y_low,
                 xlab = Xlab[d], ylab = ylab,
                 xlim = xlim, ylim = ylim,
                 main = title_d,
                 type = if (!all(is.na(F_x$y))) "l" else "n",
                 col = col_fun,
                 ...)
            if(D>1) abline(v=center[d],col=col_center,lty=lty_center)
        }

        ## 'confidence band' filled with the suitable color
	    if (!all(is.na(F_x$y_low)) && !all(is.na(F_x$y_up))) {
                polygon(c(xd,rev(xd)),
                        c(F_x$y_low,rev(F_x$y_up)),
                        col = translude(col_fun, alpha = col_fading_interval),
                        border = NA)
	    }
    }
}


#' @param X the matrix of input design.
#' @param y the array of output values (two columns means an interval).
#' @param col_points color of points.
#' @param col_fading_interval an optional factor of alpha (color channel) fading used to plot confidence intervals.
#' @param bg_fading  an optional factor of alpha (color channel) fading used to plot design points outside from this section.
#' @template sectionview-doc
#' @rdname sectionview
#' @method sectionview matrix
#' @aliases sectionview,matrix,matrix-method
#' @export
#' @seealso \code{\link{sectionview.matrix}} for a section plot, and \code{\link{sectionview3d.matrix}} for a 2D section plot.
#' @examples
#' X = matrix(runif(15*2),ncol=2)
#' y = apply(X,1,branin)
#'
#' sectionview(X,y, center=c(.5,.5))
#'
sectionview.matrix <- function(X, y,
                               center = NULL,
                               lty_center = 2,
                               col_center = "black",
                               axis = NULL,
                               col_points = if (!is.null(col)) col else "red",
                               col = NULL,
                               col_fading_interval = 0.5,
                               bg_fading = 5,
                               mfrow = NULL,
                               Xlab = NULL, ylab = NULL,
                               Xlim = if (!add) c(0,1) else NULL, ylim=NULL,
                               title = NULL, title_sep = " | ",
                               add = FALSE,
                               ...) {
    # setup for center/Xlim/axis/mfrow
    view = getView(1, add, center, if (is.null(Xlim) && !add) apply(X, 2, range) else Xlim, if (is.null(ylim) && !add) ylim <- range(y) else ylim, axis, mfrow)
    D = view$D
    center = view$center
    Xlim = view$Xlim
    ylim = view$ylim
    axis = view$axis
    mfrow = view$mfrow

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
        if (D>1) screen(id, new=!add)

        d <- axis[id,]

        xlim = Xlim[, d]

        if (is.null(title)){
            title_d <- paste(collapse = "~",sep = "~", ylab, Xlab[d])
            if (D>1) {
                title_d <-  paste(collapse =title_sep, sep =title_sep, title_d, paste(Xlab[-d], '=', fcenter[-d]))
            }
        } else {
            title_d <- title
        }

        ## fading colors for points
        if (D>1) {
            xrel <- scale(x = as.matrix(X),
                          center = center,
                          scale = drx)

            ## ind.nonfix flags the non fixed dims
            ind.nonfix <- (1:D) %in% d[1] #c(d[1], d[2])
            ind.nonfix <- !ind.nonfix

            alpha <- pmax(0,apply(X = xrel[ , ind.nonfix, drop = FALSE],
                           MARGIN = 1,
                           FUN = function(x) (1 - sqrt(sum(x^2)/D))^bg_fading))
        } else {
            alpha <- rep(1, n)
        }

        if (isTRUE(add) && !all(is.na(y))) {
            if (D>1)
                plot(x=X[,d], y=y, # Cannot use 'points' so use 'plot' with these neutral args
                       col = fade(color = col_points, alpha = alpha),
                       pch = 20,type='p',
                       xlab="",ylab="", main="", xlim=xlim, ylim=ylim,
                     bty='n', xaxt='n', yaxt='n', ann=FALSE, # remove all text, that should be already displayed
                     ...)
            else
                points(x=X[,d], y=y,
                 col = fade(color = col_points, alpha = alpha),
                 pch = 20,type='p',
                 xlab="",ylab="", xlim=xlim, ylim=ylim,
                 ...)
        } else {
            plot(X[,d], if (!all(is.na(y))) y else y_low,
                 xlab=Xlab[d], ylab=ylab, xlim=xlim, ylim=ylim,
                 main=title_d,
                 pch = 20, type = if (!all(is.na(y))) "l" else "n",
                 col = fade(color = col_points, alpha = alpha),
                 ...)
            if(D>1) abline(v=center[d],col=col_center,lty=lty_center)
        }

        if (!all(is.na(y_low)) && !all(is.na(y_up))) {
            #for (p in 1:length(conf_level)) {
                for (i in 1:n) {
                        lines(x=c(X[i,d],X[i,d]),
                              y=c(y_low[i], y_up[i]),
                              col = fade(color = col_points, alpha = alpha[i]*col_fading_interval),
                              lwd = 5, lend = 1)
                }
            #}
        }
    }
}


#' @param eval_str the expression to evaluate in each subplot.
#' @param axis optional matrix of 2-axis combinations to plot, one by row. The value \code{NULL} leads to all possible combinations i.e. \code{choose(D, 2)}.
#' @param mfrow  an optional list to force \code{par(mfrow = ...)} call. The default value  \code{NULL} is automatically set for compact view.
#' @rdname sectionview
#' @method sectionview character
#' @aliases sectionview,character,character-method
#' @export
#' @seealso \code{\link{sectionview.matrix}} for a section plot, and \code{\link{sectionview3d.matrix}} for a 2D section plot.
#' @examples
#' x1 <- rnorm(15)
#' x2 <- rnorm(15)
#'
#' y <- x1 + x2^2 + rnorm(15)
#' model <- glm(y ~ x1 + I(x2^2))
#'
#' sectionview(model, center=c(.5,.5))
#'
#' sectionview("abline(h=5)")
sectionview.character <- function(eval_str,
                              axis = NULL,
                              mfrow = NULL,
                              ...) {

    if (!exists(".split.screen.lim",envir=DiceView.env))
        stop(paste0("Cannot eval '",eval_str,"' when no previous sectionview() was called."))

    # setup for center/Xlim/axis/mfrow
    view = getView(1, TRUE, NA, NA, NA, axis, mfrow)
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
        assign("xlim",Xlim[,d],envir=e)
        assign("ylim",ylim,envir=e)

        if (D>1) {
            screen(id, new=FALSE)
            plot(x=Xlim[,d], y=ylim,
                 type='n',
                 xlab="",ylab="", main="",
                 bty='n', xaxt='n', yaxt='n', ann=FALSE, # remove all text, that should be already displayed
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
#' @template sectionview-doc
#' @rdname sectionview
#' @method sectionview km
#' @aliases sectionview,km,km-method
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
#' sectionview(model, center=c(.5,.5))
#'
#' }
#'
sectionview.km <- function(km_model, type = "UK",
                           center = NULL,
                           axis = NULL,
                           npoints = 101,
                           col_points = if (!is.null(col)) col else "red",
                           col_fun = if (!is.null(col)) col else "blue",
                           col = NULL,
                           conf_level = 0.95,
                           conf_fading = 0.5,
                           bg_fading = 5,
                           mfrow = NULL,
                           Xlab = NULL, ylab = NULL,
                           Xlim = NULL, ylim=NULL,
                           title = NULL, title_sep = " | ",
                           add = FALSE,
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

    if (is.null(conf_fading) || length(conf_fading) != length(conf_level)) {
        conf_fading <- rep(0.5/length(conf_level), length(conf_level))
    }

    # plot mean
    sectionview.function(fun = function(x) {
        DiceKriging::predict.km(km_model,type=type,newdata=x,checkNames=FALSE)$mean
    }, vectorized=TRUE,
    center = center, axis = axis, mfrow = mfrow, Xlim = Xlim, ylim=ylim,
    npoints = npoints,
    col_fun = col_fun, #conf_fading=conf_fading,
    Xlab = Xlab, ylab = ylab,
    title = title, title_sep  = title_sep, add = add, ...)

    # plot design points
    sectionview.matrix(X = X_doe, y = y_doe,
                       col_points = col_points,
                       col_fading_interval = conf_fading, bg_fading = bg_fading,
                       add=TRUE)

    # plot confidence bands
    for (l in conf_level) {
        sectionview.function(fun = function(x) {
                p = DiceKriging::predict.km(km_model,type=type,newdata=x,checkNames=FALSE)
                cbind(p$mean-qnorm(1-(1-l)/2) * p$sd, p$mean+qnorm(1-(1-l)/2) * p$sd)
            }, vectorized=TRUE,
            npoints = npoints,
            col_fun = col_fun,
            col_fading_interval=conf_fading,
            add = TRUE)

        if (km_model@noise.flag)
            sectionview.matrix(X = X_doe, y = cbind(y_doe-qnorm(1-(1-l)/2) * sdy_doe, y_doe+qnorm(1-(1-l)/2) * sdy_doe),
                           col_points = col_points,
                           col_fading_interval = conf_fading, bg_fading = bg_fading,
                           add=TRUE)
    }

}

#' @param libKriging_model an object of class \code{"Kriging"}, \code{"NuggetKriging"} or \code{"NoiseKriging"}.
#' @param col_points color of points.
#' @param bg_fading  an optional factor of alpha (color channel) fading used to plot design points outside from this section.
sectionview_libKriging <- function(libKriging_model,
                           center = NULL,
                           axis = NULL,
                           npoints = 101,
                           col_points = if (!is.null(col)) col else "red",
                           col_fun = if (!is.null(col)) col else "blue",
                           col = NULL,
                           conf_level = 0.95,
                           conf_fading = 0.5,
                           bg_fading = 5,
                           mfrow = NULL,
                           Xlab = NULL, ylab = NULL,
                           Xlim = NULL, ylim=NULL,
                           title = NULL, title_sep = " | ",
                           add = FALSE,
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

    # plot mean
    sectionview.function(fun = function(x) {
        rlibkriging::predict(libKriging_model,x,return_stdev=FALSE)$mean
    }, vectorized=TRUE,
    center = center, axis = axis, mfrow = mfrow, Xlim = Xlim, ylim=ylim,
    npoints = npoints,
    col_fun = col_fun, #conf_fading=conf_fading,
    Xlab = Xlab, ylab = ylab,
    title = title, title_sep = title_sep, add = add, ...)

    # plot design points
    sectionview.matrix(X = X_doe, y = y_doe,
                       col_points = col_points,
                       col_fading_interval = conf_fading, bg_fading = bg_fading,
                       add=TRUE)

    # plot confidence bands
    for (l in conf_level) {
        sectionview.function(fun = function(x) {
                p = rlibkriging::predict(libKriging_model,x,return_stdev=TRUE)
                cbind(p$mean-qnorm(1-(1-l)/2) * p$stdev, p$mean+qnorm(1-(1-l)/2) * p$stdev)
            }, vectorized=TRUE,
            npoints = npoints,
            col_fun = col_fun,
            col_fading_interval=conf_fading,
            add = TRUE)

        if (inherits(libKriging_model, "NoiseKriging")) # so sdy_doe!=0
            sectionview.matrix(X = X_doe, y = cbind(y_doe-qnorm(1-(1-l)/2) * sdy_doe, y_doe+qnorm(1-(1-l)/2) * sdy_doe),
                           col_points = col_points,
                           col_fading_interval = conf_fading, bg_fading = bg_fading,
                           add=TRUE)
    }

}

#' @param Kriging_model an object of class \code{"Kriging"}.
#' @param col_points color of points.
#' @param conf_level confidence intervals to display.
#' @param conf_fading an optional factor of alpha (color channel) fading used to plot confidence intervals.
#' @param bg_fading  an optional factor of alpha (color channel) fading used to plot design points outside from this section.
#' @template sectionview-doc
#' @rdname sectionview
#' @method sectionview Kriging
#' @aliases sectionview,Kriging,Kriging-method
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
#' sectionview(model, center=c(.5,.5))
#'
#' }
#'
sectionview.Kriging <- function(Kriging_model,
                                   center = NULL,
                                   axis = NULL,
                                   npoints = 101,
                                   col_points = if (!is.null(col)) col else "red",
                                   col_fun = if (!is.null(col)) col else "blue",
                                   col = NULL,
                                   conf_level = 0.95,
                                   conf_fading = 0.5,
                                   bg_fading = 5,
                                   mfrow = NULL,
                                   Xlab = NULL, ylab = NULL,
                                   Xlim = NULL, ylim=NULL,
                                   title = NULL, title_sep = " | ",
                                   add = FALSE,
                                   ...) {
    sectionview_libKriging(Kriging_model,center,axis,npoints,
                           col_points=col_points,col_fun=col_fun,col=col,
                           conf_level,conf_fading,bg_fading,
                           mfrow,Xlab, ylab,Xlim,ylim,title,title_sep,add,...)
}

#' @param NuggetKriging_model an object of class \code{"Kriging"}.
#' @param col_points color of points.
#' @param conf_level an optional list of confidence intervals to display.
#' @param conf_fading an optional factor of alpha (color channel) fading used to plot confidence intervals.
#' @param bg_fading  an optional factor of alpha (color channel) fading used to plot design points outside from this section.
#' @template sectionview-doc
#' @rdname sectionview
#' @method sectionview NuggetKriging
#' @aliases sectionview,NuggetKriging,NuggetKriging-method
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
#' sectionview(model, center=c(.5,.5))
#'
#' }
#'
sectionview.NuggetKriging <- function(NuggetKriging_model,
                                center = NULL,
                                axis = NULL,
                                npoints = 101,
                                col_points = if (!is.null(col)) col else "red",
                                col_fun = if (!is.null(col)) col else "blue",
                                col = NULL,
                                conf_level = 0.95,
                                conf_fading = 0.5,
                                bg_fading = 5,
                                mfrow = NULL,
                                Xlab = NULL, ylab = NULL,
                                Xlim = NULL, ylim=NULL,
                                title = NULL, title_sep = " | ",
                                add = FALSE,
                                ...) {
    sectionview_libKriging(NuggetKriging_model,center,axis,npoints,
                           col_points=col_points,col_fun=col_fun,col=col,
                           conf_level,conf_fading,bg_fading,
                           mfrow,Xlab, ylab,Xlim,ylim,title,title_sep,add,...)
}

#' @param NoiseKriging_model an object of class \code{"Kriging"}.
#' @param col_points color of points.
#' @param conf_level an optional list of confidence intervals to display.
#' @param conf_fading an optional factor of alpha (color channel) fading used to plot confidence intervals.
#' @param bg_fading  an optional factor of alpha (color channel) fading used to plot design points outside from this section.
#' @template sectionview-doc
#' @rdname sectionview
#' @method sectionview NoiseKriging
#' @aliases sectionview,NoiseKriging,NoiseKriging-method
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
#' sectionview(model, center=c(.5,.5))
#'
#' }
#'
sectionview.NoiseKriging <- function(NoiseKriging_model,
                                      center = NULL,
                                      axis = NULL,
                                      npoints = 101,
                                      col_points = if (!is.null(col)) col else "red",
                                      col_fun = if (!is.null(col)) col else "blue",
                                      col = NULL,
                                      conf_level = 0.95,
                                      conf_fading = 0.5,
                                      bg_fading = 5,
                                      mfrow = NULL,
                                      Xlab = NULL, ylab = NULL,
                                      Xlim = NULL, ylim=NULL,
                                      title = NULL, title_sep = " | ",
                                      add = FALSE,
                                      ...) {
    sectionview_libKriging(NoiseKriging_model,center,axis,npoints,
                           col_points=col_points,col_fun=col_fun,col=col,
                           conf_level,conf_fading,bg_fading,
                           mfrow,Xlab, ylab,Xlim,ylim,title,title_sep,add,...)
}

#' @param glm_model an object of class \code{"glm"}.
#' @param col_points color of points.
#' @param conf_level an optional list of confidence intervals to display.
#' @param conf_fading an optional factor of alpha (color channel) fading used to plot confidence intervals.
#' @param bg_fading  an optional factor of alpha (color channel) fading used to plot design points outside from this section.
#' @template sectionview-doc
#' @rdname sectionview
#' @method sectionview glm
#' @aliases sectionview,glm,glm-method
#' @export
#' @seealso \code{\link{sectionview.glm}} for a section plot, and \code{\link{sectionview3d.glm}} for a 2D section plot.
#' @examples
#' x1 <- rnorm(15)
#' x2 <- rnorm(15)
#'
#' y <- x1 + x2^2 + rnorm(15)
#' model <- glm(y ~ x1 + I(x2^2))
#'
#' sectionview(model, center=c(.5,.5))
#'
sectionview.glm <- function(glm_model,
                           center = NULL,
                           axis = NULL,
                           npoints = 101,
                           col_points = if (!is.null(col)) col else "red",
                           col_fun = if (!is.null(col)) col else "blue",
                           col = NULL,
                           conf_level = 0.95,
                           conf_fading = 0.5,
                           bg_fading = 5,
                           mfrow = NULL,
                           Xlab = NULL, ylab = NULL,
                           Xlim = NULL, ylim=NULL,
                           title = NULL, title_sep = " | ",
                           add = FALSE,
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
    sectionview.function(fun = function(x) {
        x = as.data.frame(x)
            colnames(x) <- Xlab
            predict.glm(glm_model, newdata=x, se.fit=FALSE)
    }, vectorized=TRUE,
    center = center,axis = axis, mfrow = mfrow, Xlim = Xlim, ylim=range(y_doe),
    npoints = npoints,
    col_fun = col_fun,
    Xlab = Xlab, ylab = ylab,
    title = title, title_sep  = title_sep, add = add, ...)

    # plot design points
    sectionview.matrix(X = X_doe, y = y_doe,
                   col_points = col_points,
                   col_fading_interval = conf_fading, bg_fading = bg_fading,
                   add=TRUE)

    # plot confidence bands
    for (l in conf_level) {
        sectionview.function(fun = function(x) {
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
#' @template sectionview-doc
#' @rdname sectionview
#' @method sectionview list
#' @aliases sectionview,list,list-method
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
#' sectionview(model, center=c(.5,.5))
#'
#' }
#'
sectionview.list <- function(modelFit_model,
                            center = NULL,
                            axis = NULL,
                            npoints = 101,
                            col_points = if (!is.null(col)) col else "red",
                            col_fun = if (!is.null(col)) col else "blue",
                            col = NULL,
                            bg_fading = 5,
                            mfrow = NULL,
                            Xlab = NULL, ylab = NULL,
                            Xlim = NULL, ylim=NULL,
                            title = NULL, title_sep = " | ",
                            add = FALSE,
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
    sectionview.function(fun = function(x) {
        x = as.data.frame(x)
            colnames(x) <- Xlab
            DiceEval::modelPredict(modelFit_model, x)
    }, vectorized=TRUE,
    center = center,axis = axis,mfrow = mfrow,Xlim = Xlim, ylim=range(y_doe),
    npoints = npoints,
    col_fun = col_fun, #conf_fading=conf_fading,
    Xlab = Xlab, ylab = ylab,
    title = title, title_sep = title_sep, add = add, ...)

    # plot design points
    sectionview.matrix(X = X_doe, y = y_doe,
                   col_points = col_points,
                   bg_fading = bg_fading,
                   add=TRUE)
}



#### Wrapper for sectionview ####

#' @import methods
if(!isGeneric("sectionview")) {
    setGeneric(name = "sectionview",
               def = function(...) standardGeneric("sectionview")
    )
}

#' @title Plot a section view of a prediction model or function, including design points if available.
#' @details If available, experimental points are plotted with fading colors. Points that fall in the specified section (if any) have the color specified \code{col_points} while points far away from the center have shaded versions of the same color. The amount of fading is determined using the Euclidean distance between the plotted point and \code{center}.
#' @param ... arguments of the \code{sectionview.km}, \code{sectionview.glm}, \code{sectionview.Kriging} or \code{sectionview.function} function
#' @export
#' @examples
#' ## A 2D example - Branin-Hoo function
#' sectionview(branin, center= c(.5,.5), col='black')
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
#' sectionview(model, center= c(.5,.5))
#' sectionview(branin, center= c(.5,.5), col='red', add=TRUE)
#' }
#'
#' if (requireNamespace("rlibkriging")) { library(rlibkriging)
#' ## model: Kriging
#' model <- Kriging(X = as.matrix(design.fact), y = as.matrix(y), kernel="matern3_2")
#' sectionview(model, center= c(.5,.5))
#' sectionview(branin, center= c(.5,.5), col='red', add=TRUE)
#' }
#'
#' ## model: glm
#' model <- glm(y ~ 1+ x1 + x2 + I(x1^2) + I(x2^2) + x1*x2, data=cbind(y,design.fact))
#' sectionview(model, center= c(.5,.5))
#' sectionview(branin, center= c(.5,.5), col='red', add=TRUE)
#'
#' if (requireNamespace("DiceEval")) { library(DiceEval)
#' ## model: StepLinear
#' model <- modelFit(design.fact, y, type = "StepLinear")
#' sectionview(model, center= c(.5,.5))
#' sectionview(branin, center= c(.5,.5), col='red', add=TRUE)
#' }
#' }
#'
sectionview <- function(...){
    UseMethod("sectionview")
}
