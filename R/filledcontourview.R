#' @param fun a function or 'predict()'-like function that returns a simple numeric or mean and standard error: list(mean=...,se=...).
#' @param vectorized is fun vectorized?
#' @param dim input variables dimension of the model or function.
#' @param conf_blend alpha shadow to apply on fun standard error (se) if returned.
#' @template filledcontourview-doc
#' @rdname filledcontourview
#' @method filledcontourview function
#' @aliases filledcontourview,function,function-method
#' @export
#' @seealso \code{\link{sectionview.function}} for a section plot, and \code{\link{sectionview3d.function}} for a 2D section plot.
#' \code{\link{Vectorize.function}} to wrap as vectorized a non-vectorized function.
#' @examples
#' x1 <- rnorm(15)
#' x2 <- rnorm(15)
#'
#' y <- x1 + x2 + rnorm(15)
#'
#' model <- lm(y ~ x1 + x2)
#'
#' filledcontourview(function(x) sum(x),
#'                      dim=2, Xlim=cbind(range(x1),range(x2)), col='black')
#' points(x1,x2)
#'
#' filledcontourview(function(x) {
#'                       x = as.data.frame(x)
#'                       colnames(x) <- names(model$coefficients[-1])
#'                       p = predict.lm(model, newdata=x, se.fit=TRUE)
#'                       list(mean=p$fit, se=p$se.fit)
#'                     }, vectorized=TRUE, dim=2, Xlim=cbind(range(x1),range(x2)), add=TRUE)
#'
filledcontourview.function <- function(fun, vectorized=FALSE,
                                dim = NULL,
                             center = NULL,
                             lty_center = 2,
                             col_center = "black",
                             axis = NULL,
                             npoints = 20,
                             nlevels = if (is.null(levels)) 10 else length(levels),
                             levels = NULL,
                             lty_levels = 1,
                             col_levels = if (!is.null(col) & length(col)==1) col.levels(col,nlevels+1) else col.levels("blue",nlevels+1),
                             col = NULL,
                             conf_blend = 0.5,
                             mfrow = NULL,
                             Xlab = NULL, ylab = NULL,
                             Xlim = NULL,
                             title = NULL,
                             add = FALSE,
                             ...) {
    if (!is.null(dim)) {
        D <- dim
        if (is.null(center))
            if (D != 2) stop("Section center in 'section' required for >2-D model.")
    } else {
        if (is.null(center))
            stop("dim or center must be specified.")
        else
            D <- length(center)
    }

    if (!vectorized)
        Fun = Vectorize.function(fun, D)
    else
        Fun = fun

    if (D == 1) stop("for a model with dim 1, use 'sectionview'")

    if (is.null(axis)) {
        axis <- t(utils::combn(D, 2))
    } else {
        ## added by YD for the vector case
        axis <- matrix(axis, ncol = 2)
    }

    if (is.null(mfrow)) {
        nc <- round(sqrt(nrow(axis)))
        nl <- ceiling(nrow(axis)/nc)
        mfrow <- c(nc, nl)
    }

    if (length(col_levels) == (nlevels+1))
        col_fills = col_levels
    else if (length(col_levels) == 1)
        col_fills = colorRampPalette(c(rgb(1,1,1,0),col_levels), alpha=TRUE)(nlevels+1) # from white transparent to col_level
    else if (length(col_levels) == 2)
        col_fills =  colorRampPalette(col_levels)(nlevels+1)
    else
        stop("col_levels must be a vector of length 1, 2 or nlevels+1.")

    if (!isTRUE(add)) {
        #if (D>2) {
            close.screen( all.screens = TRUE )
            split.screen(figs = mfrow)
        #}
        assign(".split.screen.lim",matrix(NaN,ncol=6,nrow=D),envir=DiceView.env) # xmin,xmax,ymin,ymax matrix of limits, each row for one dim combination
    }

    if (!exists(".split.screen.lim",envir=DiceView.env))
        assign(".split.screen.lim",matrix(NaN,ncol=6,nrow=D),envir=DiceView.env)

    ## Changed by YD: a vector
    ## if (is.null(dim(npoints))) { npoints <- rep(npoints,D) }
    npoints <- rep(npoints, length.out = D)

    ## find limits: 'rx' is matrix with min in row 1 and max in row 2
    if(!is.null(Xlim))
        rx <- matrix(Xlim,nrow=2,ncol=D)
    else {
        rx <- matrix(c(0,1),nrow=2,ncol=D)
        if (!is.null(center)) # ensure center is included in Xlim
            for (i in 1:D) {
                rx[1,i] <- min(rx[1,i],center[i])
                rx[2,i] <- max(rx[2,i],center[i])
            }
        }
    rownames(rx) <- c("min", "max")
    drx <- unlist(rx["max", ]) - unlist(rx["min", ])

    ## define X & y labels
    if (is.null(ylab)) ylab <- "y"
    if (is.null(Xlab)) Xlab <- paste(sep = "", "X", 1:D)

    ## Added by YD (as in sectionview3d.km)
    if (is.null(center)) {
        center <- rep(0, D)
        names(center) <- Xlab
    }

    ## try to find a good formatted value 'fcenter' for 'center'
    fcenter <- tryFormat(x = center, drx = drx)

    ## Each 'id' will produce a RGL plot
    for (id in 1:dim(axis)[1]) {
        if (D>2) screen(id, new=!add)

        d <- axis[id, ]

        npoints_all <- npoints[d[1]]*npoints[d[2]]

        ## ind.nonfix flags the non fixed dims
        ind.nonfix <- (1:D) %in% c(d[1], d[2])
        ind.nonfix <- !ind.nonfix

        xdmin <- unlist(rx["min", d])
        xdmax <- unlist(rx["max", d])

        xd1 <- seq(from = xdmin[1], to = xdmax[1], length.out = npoints[1])
        xd2 <- seq(from = xdmin[2], to = xdmax[2], length.out = npoints[2])

        x <- data.frame(t(matrix(as.numeric(center), nrow = D, ncol = npoints_all)))
        if (!is.null(center)) if(!is.null(names(center))) names(x) <- names(center)
        x[ , d] <- expand.grid(xd1, xd2)

        y_mean <- array(NA, npoints_all)
        y_sd <- array(0, npoints_all)
        yd_mean <- matrix(NA,npoints[1], npoints[2])
        yd_sd <- matrix(0,npoints[1], npoints[2])

        y <- Fun(as.matrix(x))
        y_has_sd = FALSE
        if (is.list(y)) {
            y = lapply(as.list(as.data.frame(y)),unlist)
            if (!("mean" %in% names(y)) || !("se" %in% names(y)))
                stop(paste0("If function returns a list, it must have 'mean' and 'se', while was ",paste0(collapse="\n",utils::capture.output(print(y)))))
            y_mean <- as.numeric(y$mean)
            if (!is.numeric(y_mean))
                stop("If function returns a list, 'mean' must be (as) numeric:",paste0(y_mean,collapse="\n"))
            y_sd <- as.numeric(y$se)
            if (!is.numeric(y_sd))
                stop("If function returns a list, 'se' must be (as) numeric:",paste0(y_sd,collapse="\n"))
            y_has_sd = TRUE
        } else if (is.matrix(y) && ncol(y)==2) {
            y_mean <- as.numeric(y[,1])
            y_sd <- as.numeric(y[,2])
            if (!is.numeric(y_mean))
                stop("If function returns a matrix, first column must be (as) numeric.")
            if (!is.numeric(y_sd))
                stop("If function returns a matrix, second column must be (as) numeric.")
        } else { # simple function, not a list
            if (!is.numeric(y))
                stop("If function does not returns a list, it must be numeric.")
            y_mean <- as.numeric(y)
            y_sd <- 0
        }
        yd_mean <- matrix(y_mean,ncol=npoints[2],nrow=npoints[1])
        yd_sd <- matrix(y_sd,ncol=npoints[2],nrow=npoints[1])

        if (is.null(levels)) levels = pretty(y_mean,nlevels)

        if (is.null(title)){
            title_d <- paste(collapse = "~",sep = "~", ylab, paste(collapse = ",", sep = ",", Xlab[d[1]], Xlab[d[2]]))
            if (D>2) {
                title_d <-  paste(collapse = " | ", sep = " | ", title_d, paste(collapse=',',Xlab[ind.nonfix],'=', fcenter[ind.nonfix]))
            }
        } else {
            title_d <-  title
        }

        ## plot mean surface two steps required to use alpha =
        if (isTRUE(add)) {
            # re-use global settings for limits of this screen
            .split.screen.lim = get(x=".split.screen.lim",envir=DiceView.env)
            xlim <- c(.split.screen.lim[id,1],.split.screen.lim[id,2])
            ylim <- c(.split.screen.lim[id,3],.split.screen.lim[id,4])
            zlim <- c(.split.screen.lim[id,5],.split.screen.lim[id,6])
            .filled.contour(x = xd1,y = xd2, z = yd_mean,
                            col = col_fills,
                            levels = levels)
            if (lty_levels>0)
                contour(x = xd1,y = xd2, z = yd_mean,
                    xlim = xlim, ylim = ylim, zlim = zlim,
                    col = col_fills,  lty = lty_levels,
                    levels = levels,
                    add=TRUE,
                    ...)
        } else {
            xlim = rx[,d[1]]
            ylim = rx[,d[2]]
            zlim = range(yd_mean)
            eval(parse(text=paste(".split.screen.lim[",id,",] = matrix(c(",xlim[1],",",xlim[2],",",ylim[1],",",ylim[2],",",zlim[1],",",zlim[2],"),nrow=1)")),envir=DiceView.env)
            contour(x = xd1, y = xd2, z = yd_mean,
                    xlab = Xlab[d[1]], ylab = Xlab[d[2]],
                    xlim = xlim, ylim = ylim, zlim = zlim,
                    main = title_d,
                    col = col_fills,  lty = lty_levels,
                    levels = levels,
                    add=FALSE,
                    ...)
            .filled.contour(x = xd1, y = xd2, z = yd_mean,
                    col = col_fills,
                    levels = levels)
            if(D>2) {
                abline(v=center[d[1]],col=col_center,lty=lty_center)
                abline(h=center[d[2]],col=col_center,lty=lty_center)
            }
        }

        if (y_has_sd && any(y_sd>0)) {
            col_fills_rgba = col2rgb("white")
            col_sd = rgb(col_fills_rgba[1]/255,col_fills_rgba[2]/255,col_fills_rgba[3]/255,seq(from=0,to=1,length=nlevels-1))
            image(x = xd1,y = xd2, z = yd_sd,
                  col = col_sd, breaks=seq(from=min(yd_sd),to=max(yd_sd),length=length(col_sd)+1),
                  add=TRUE)
        }
    }
}

#' @param km_model an object of class \code{"km"}.
#' @param type the kriging type to use for model prediction.
#' @param col_points color of points.
#' @param bg_blend  an optional factor of alpha (color channel) blending used to plot design points outside from this section.
#' @param conf_level confidence level hull to display.
#' @param conf_blend an optional factor of alpha (color channel) blending used to plot confidence hull.
#' @template filledcontourview-doc
#' @rdname filledcontourview
#' @method filledcontourview km
#' @aliases filledcontourview,km,km-method
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
#' filledcontourview(model)
#'
#' }
#'
filledcontourview.km <- function(km_model, type = "UK",
                           center = NULL,
                           axis = NULL,
                           npoints = 20,
                           nlevels = if (is.null(levels)) 10 else length(levels),
                           levels = NULL,
                           col_points = if (!is.null(col) & length(col)==1) col else "red",
                           col_levels = if (!is.null(col) & length(col)==1) col.levels(col,nlevels+1) else col.levels("blue",nlevels+1),
                           col = NULL,
                           conf_level = 0.5,
                           conf_blend = conf_level,
                           bg_blend = 1,
                           mfrow = NULL,
                           Xlab = NULL, ylab = NULL,
                           Xlim = NULL,
                           title = NULL,
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
        sdy_doe <- rep(0, n)
    }

    ## find limits: rx is matrix with min in row 1 and max in row 2
    rx <- apply(X_doe, 2, range)
    if(!is.null(Xlim)) rx <- matrix(Xlim,nrow=2,ncol=D)
    rownames(rx) <- c("min", "max")
    drx <- unlist(rx["max", ]) - unlist(rx["min", ])

    ## define X & y labels
    if (is.null(ylab)) ylab <- names(y_doe)
    if (is.null(Xlab)) Xlab <- names(X_doe)

    if (is.null(axis)) {
        axis <- t(utils::combn(D, 2))
    } else {
        axis <- matrix(axis, ncol = 2)
    }

    filledcontourview.function(
        fun = function(x) {
            p = DiceKriging::predict.km(km_model,type=type,newdata=x,checkNames=FALSE)
            list(mean=p$mean, se=qnorm(1-(1-conf_level)/2) * p$sd) # to dosplay gaussian conf interval
        }, vectorized=TRUE,
    dim = D, center = center,axis = axis,npoints = npoints, nlevels = nlevels, levels = levels,
    col_levels = col_levels,conf_blend = conf_blend,
    mfrow = mfrow, Xlab = Xlab, ylab = ylab,
    Xlim = rx, title = title, add = add, ...)

    contourview.matrix(X = X_doe, y = y_doe, sdy = sdy_doe,
                       dim = D, center = center, axis = axis,
                       col_points = col_points,
                       bg_blend = bg_blend,
                       mfrow = mfrow,
                       Xlim = rx,
                       add=TRUE)

}

#' @param libKriging_model an object of class \code{"Kriging"}, \code{"NuggetKriging"} or \code{"NoiseKriging"}.
#' @param col_points color of points.
#' @param conf_level confidence level hull to display.
#' @param conf_blend an optional factor of alpha (color channel) blending used to plot confidence hull.
#' @param bg_blend  an optional factor of alpha (color channel) blending used to plot design points outside from this section.
filledcontourview_libKriging <- function(libKriging_model,
                           center = NULL,
                           axis = NULL,
                           npoints = 20,
                           nlevels = if (is.null(levels)) 10 else length(levels),
                           levels = NULL,
                           col_points = if (!is.null(col) & length(col)==1) col else "red",
                           col_levels = if (!is.null(col) & length(col)==1) col.levels(col,nlevels+1) else col.levels("blue",nlevels+1),
                           col = NULL,
                           conf_level = 0.5,
                           conf_blend = conf_level,
                           bg_blend = 1,
                           mfrow = NULL,
                           Xlab = NULL, ylab = NULL,
                           Xlim = NULL,
                           title = NULL,
                           add = FALSE,
                           ...) {
    X_doe <- libKriging_model$X()
    y_doe <- libKriging_model$y()

    D <- ncol(X_doe)
    n <- nrow(X_doe)

    if (inherits(libKriging_model, "Kriging")) {
        sdy_doe <- rep(0, n)
    } else if (inherits(libKriging_model, "NuggetKriging")) {
        sdy_doe <- rep(sqrt(libKriging_model$nugget()),n)
    } else if (inherits(libKriging_model, "NoiseKriging")) {
        sdy_doe <- sqrt(libKriging_model$noise())
    } else
        stop(paste0("Kriging model of class ",class(libKriging_model)," is not yet supported."))

    ## find limits: rx is matrix with min in row 1 and max in row 2
    rx <- apply(X_doe, 2, range)
    if(!is.null(Xlim)) rx <- matrix(Xlim,nrow=2,ncol=D)
    rownames(rx) <- c("min", "max")
    drx <- unlist(rx["max", ]) - unlist(rx["min", ])

    ## define X & y labels
    if (is.null(ylab)) ylab <- names(y_doe)
    if (is.null(ylab)) ylab <- "y"
    if (is.null(Xlab)) Xlab <- names(X_doe)
    if (is.null(Xlab)) Xlab <- paste(sep = "", "X", 1:D)

    if (is.null(axis)) {
        axis <- t(utils::combn(D, 2))
    } else {
        axis <- matrix(axis, ncol = 2)
    }

    filledcontourview.function(fun = function(x) {
            p = rlibkriging::predict(libKriging_model,x,return_stdev=TRUE)
            list(mean=p$mean, se=qnorm(1-(1-conf_level)/2) * p$stdev) # to display gaussian conf interval
        }, vectorized=TRUE,
        dim = D, center = center,axis = axis,npoints = npoints,nlevels = nlevels,
        col_levels = col_levels, conf_blend = conf_blend,
        mfrow = mfrow, Xlab = Xlab, ylab = ylab,
        Xlim = rx, title = title, add = add, ...)

    contourview.matrix(X = X_doe, y = y_doe, sdy = sdy_doe,
                       dim = D, center = center, axis = axis,
                       col_points = col_points,
                       bg_blend = bg_blend,
                       mfrow = mfrow,
                       Xlim = rx,
                       add=TRUE)
}

#' @param Kriging_model an object of class \code{"Kriging"}.
#' @param col_points color of points.
#' @param bg_blend  an optional factor of alpha (color channel) blending used to plot design points outside from this section.
#' @param conf_level confidence level hull to display.
#' @param conf_blend an optional factor of alpha (color channel) blending used to plot confidence hull.
#' @template filledcontourview-doc
#' @rdname filledcontourview
#' @method filledcontourview Kriging
#' @aliases filledcontourview,Kriging,Kriging-method
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
#' filledcontourview(model)
#'
#' }
#'
filledcontourview.Kriging <- function(Kriging_model,
                                   center = NULL,
                                   axis = NULL,
                                   npoints = 20,
                                   nlevels = if (is.null(levels)) 10 else length(levels),
                                   levels = NULL,
                                   col_points = if (!is.null(col) & length(col)==1) col else "red",
                                   col_levels = if (!is.null(col) & length(col)==1) col.levels(col,nlevels+1) else col.levels("blue",nlevels+1),
                                   col = NULL,
                                   conf_level = 0.5,
                                   conf_blend = conf_level,
                                   bg_blend = 1,
                                   mfrow = NULL,
                                   Xlab = NULL, ylab = NULL,
                                   Xlim = NULL,
                                   title = NULL,
                                   add = FALSE,
                                   ...) {
    filledcontourview_libKriging(libKriging_model = Kriging_model,
                           center = center,
                           axis = axis,
                           npoints = npoints,
                           nlevels = nlevels,
                           levels = levels,
                           col_points = col_points,
                           col_levels = col_levels,
                           col = col,
                           conf_level = conf_level,
                           conf_blend = conf_blend,
                           bg_blend = bg_blend,
                           mfrow = mfrow,
                           Xlab = Xlab, ylab = ylab,
                           Xlim = Xlim,
                           title = title,
                           add = add,
                           ...)
}

#' @param NuggetKriging_model an object of class \code{"Kriging"}.
#' @param col_points color of points.
#' @param bg_blend  an optional factor of alpha (color channel) blending used to plot design points outside from this section.
#' @param conf_level confidence level hull to display.
#' @param conf_blend an optional factor of alpha (color channel) blending used to plot confidence hull.
#' @template filledcontourview-doc
#' @rdname filledcontourview
#' @method filledcontourview NuggetKriging
#' @aliases filledcontourview,NuggetKriging,NuggetKriging-method
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
#' filledcontourview(model)
#'
#' }
#'
filledcontourview.NuggetKriging <- function(NuggetKriging_model,
                                center = NULL,
                                axis = NULL,
                                npoints = 20,
                                nlevels = if (is.null(levels)) 10 else length(levels),
                                levels = NULL,
                                col_points = if (!is.null(col) & length(col)==1) col else "red",
                                col_levels = if (!is.null(col) & length(col)==1) col.levels(col,nlevels+1) else col.levels("blue",nlevels+1),
                                col = NULL,
                                conf_level = 0.5,
                                conf_blend = conf_level,
                                bg_blend = 1,
                                mfrow = NULL,
                                Xlab = NULL, ylab = NULL,
                                Xlim = NULL,
                                title = NULL,
                                add = FALSE,
                                ...) {
    filledcontourview_libKriging(libKriging_model = NuggetKriging_model,
                           center = center,
                           axis = axis,
                           npoints = npoints,
                           nlevels = nlevels,
                           levels = levels,
                           col_points = col_points,
                           col_levels = col_levels,
                           col = col,
                           conf_level = conf_level,
                           conf_blend = conf_blend,
                           bg_blend = bg_blend,
                           mfrow = mfrow,
                           Xlab = Xlab, ylab = ylab,
                           Xlim = Xlim,
                           title = title,
                           add = add,
                           ...)
    }

#' @param NoiseKriging_model an object of class \code{"Kriging"}.
#' @param col_points color of points.
#' @param bg_blend  an optional factor of alpha (color channel) blending used to plot design points outside from this section.
#' @param conf_level confidence level hull to display.
#' @param conf_blend an optional factor of alpha (color channel) blending used to plot confidence hull.
#' @template filledcontourview-doc
#' @rdname filledcontourview
#' @method filledcontourview NoiseKriging
#' @aliases filledcontourview,NoiseKriging,NoiseKriging-method
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
#' filledcontourview(model)
#'
#' }
#'
filledcontourview.NoiseKriging <- function(NoiseKriging_model,
                                      center = NULL,
                                      axis = NULL,
                                      npoints = 20,
                                      nlevels = if (is.null(levels)) 10 else length(levels),
                                      levels = NULL,
                                      col_points = if (!is.null(col) & length(col)==1) col else "red",
                                      col_levels = if (!is.null(col) & length(col)==1) col.levels(col,nlevels+1) else col.levels("blue",nlevels+1),
                                      col = NULL,
                                      conf_level = 0.5,
                                      conf_blend = conf_level,
                                      bg_blend = 1,
                                      mfrow = NULL,
                                      Xlab = NULL, ylab = NULL,
                                      Xlim = NULL,
                                      title = NULL,
                                      add = FALSE,
                                      ...) {
    filledcontourview_libKriging(libKriging_model = NoiseKriging_model,
                           center = center,
                           axis = axis,
                           npoints = npoints,
                           nlevels = nlevels,
                           levels = levels,
                           col_points = col_points,
                           col_levels = col_levels,
                           col = col,
                           conf_level = conf_level,
                           conf_blend = conf_blend,
                           bg_blend = bg_blend,
                           mfrow = mfrow,
                           Xlab = Xlab, ylab = ylab,
                           Xlim = Xlim,
                           title = title,
                           add = add,
                           ...)
    }

#' @param glm_model an object of class \code{"glm"}.
#' @param col_points color of points.
#' @param bg_blend  an optional factor of alpha (color channel) blending used to plot design points outside from this section.
#' @param conf_level confidence level hull to display.
#' @param conf_blend an optional factor of alpha (color channel) blending used to plot confidence hull.
#' @template filledcontourview-doc
#' @rdname filledcontourview
#' @method filledcontourview glm
#' @aliases filledcontourview,glm,glm-method
#' @export
#' @seealso \code{\link{sectionview.glm}} for a section plot, and \code{\link{sectionview3d.glm}} for a 2D section plot.
#' @examples
#' x1 <- rnorm(15)
#' x2 <- rnorm(15)
#'
#' y <- x1 + x2^2 + rnorm(15)
#' model <- glm(y ~ x1 + I(x2^2))
#'
#' filledcontourview(model)
#'
filledcontourview.glm <- function(glm_model,
                           center = NULL,
                           axis = NULL,
                           npoints = 20,
                           nlevels = if (is.null(levels)) 10 else length(levels),
                           levels = NULL,
                           col_points = if (!is.null(col) & length(col)==1) col else "red",
                           col_levels = if (!is.null(col) & length(col)==1) col.levels(col,nlevels+1) else col.levels("blue",nlevels+1),
                           col = NULL,
                           conf_level = 0.5,
                           conf_blend = conf_level,
                           bg_blend = 1,
                           mfrow = NULL,
                           Xlab = NULL, ylab = NULL,
                           Xlim = NULL,
                           title = NULL,
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

    ## find limits: rx is matrix with min in row 1 and max in row 2
    rx <- apply(X_doe, 2, range)
    if(!is.null(Xlim)) rx <- matrix(Xlim,nrow=2,ncol=D)
    rownames(rx) <- c("min", "max")
    drx <- unlist(rx["max", ]) - unlist(rx["min", ])

    if (is.null(axis)) {
        axis <- t(utils::combn(D, 2))
    } else {
        axis <- matrix(axis, ncol = 2)
    }

    filledcontourview.function(
        fun = function(x) {
            x = as.data.frame(x)
            colnames(x) <- Xlab
            p = predict.glm(glm_model, newdata=x, se.fit=TRUE)
            list(mean=p$fit, se=qnorm(1-(1-conf_level)/2) * p$se.fit)
        }, vectorized=TRUE,
        dim = D, center = center,axis = axis, npoints = npoints, nlevels = nlevels, levels = levels,
        col_levels = col_levels, conf_blend = conf_blend,
        mfrow = mfrow, Xlab = Xlab, ylab = ylab,
        Xlim = rx, title = title, add = add, ...)

    contourview.matrix(X = X_doe, y = y_doe, sdy = NULL,
                       dim = D, center = center, axis = axis,
                       col_points = col_points,
                       bg_blend = bg_blend,
                       mfrow = mfrow,
                       Xlim = rx,
                       add=TRUE)
}


#' @param modelFit_model an object returned by DiceEval::modelFit.
#' @param col_points color of points.
#' @param bg_blend  an optional factor of alpha (color channel) blending used to plot design points outside from this section.
#' @param conf_blend an optional factor of alpha (color channel) blending used to plot standard error (if any) hull.
#' @template filledcontourview-doc
#' @rdname filledcontourview
#' @method filledcontourview list
#' @aliases filledcontourview,list,list-method
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
#' filledcontourview(model)
#'
#' }
#'
filledcontourview.list <- function(modelFit_model,
                            center = NULL,
                            axis = NULL,
                            npoints = 20,
                            nlevels = if (is.null(levels)) 10 else length(levels),
                            levels = NULL,
                            col_points = if (!is.null(col) & length(col)==1) col else "red",
                            col_levels = if (!is.null(col) & length(col)==1) col.levels(col,nlevels+1) else col.levels("blue",nlevels+1),
                            col = NULL,
                            conf_blend = 0.5,
                            bg_blend = 1,
                            mfrow = NULL,
                            Xlab = NULL, ylab = NULL,
                            Xlim = NULL,
                            title = NULL,
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

    ## find limits: rx is matrix with min in row 1 and max in row 2
    rx <- apply(X_doe, 2, range)
    if(!is.null(Xlim)) rx <- matrix(Xlim,nrow=2,ncol=D)
    rownames(rx) <- c("min", "max")
    drx <- unlist(rx["max", ]) - unlist(rx["min", ])

    if (is.null(axis)) {
        axis <- t(utils::combn(D, 2))
    } else {
        axis <- matrix(axis, ncol = 2)
    }

    filledcontourview.function(
        fun = function(x) {
            x = as.data.frame(x)
            colnames(x) <- Xlab
            DiceEval::modelPredict(modelFit_model, x)
        }, vectorized=TRUE,
        dim = D, center = center,axis = axis, npoints = npoints, nlevels = nlevels, levels = levels,
        col_levels = col_levels, conf_blend=conf_blend,
        mfrow = mfrow, Xlab = Xlab, ylab = ylab,
        Xlim = rx, title = title, add = add, ...)

    contourview.matrix(X = X_doe, y = y_doe, sdy = NULL,
                       dim = D, center = center, axis = axis,
                       col_points = col_points,
                       bg_blend = bg_blend,
                       mfrow = mfrow,
                       Xlim = rx,
                       add=TRUE)
}





#### Wrapper for filledcontourview ####

#' @import methods
if(!isGeneric("filledcontourview")) {
    setGeneric(name = "filledcontourview",
               def = function(...) standardGeneric("filledcontourview")
    )
}

#' @title Plot a contour view of a prediction model or function, including design points if available.
#' @details If available, experimental points are plotted with fading colors. Points that fall in the specified section (if any) have the color specified \code{col_points} while points far away from the center have shaded versions of the same color. The amount of fading is determined using the Euclidean distance between the plotted point and \code{center}.
#' @param ... arguments of the \code{filledcontourview.km}, \code{filledcontourview.glm}, \code{filledcontourview.Kriging} or \code{filledcontourview.function} function
#' @export
#' @examples
#' ## A 2D example - Branin-Hoo function
#' filledcontourview(branin, dim=2, nlevels=30, col='black')
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
#' filledcontourview(model, nlevels=30)
#' filledcontourview(branin, dim=2, nlevels=30, col='red', add=TRUE)
#' }
#'
#' ## model: Kriging
#' if (requireNamespace("rlibkriging")) { library(rlibkriging)
#' model <- Kriging(X = as.matrix(design.fact), y = as.matrix(y), kernel="matern3_2")
#' filledcontourview(model, nlevels=30)
#' filledcontourview(branin, dim=2, nlevels=30, col='red', add=TRUE)
#' }
#'
#' ## model: glm
#' model <- glm(y ~ 1+ x1 + x2 + I(x1^2) + I(x2^2) + x1*x2, data=cbind(y,design.fact))
#' filledcontourview(model, nlevels=30)
#' filledcontourview(branin, dim=2, nlevels=30, col='red', add=TRUE)
#'
#' if (requireNamespace("DiceEval")) { library(DiceEval)
#' ## model: StepLinear
#' model <- modelFit(design.fact, y, type = "StepLinear")
#' filledcontourview(model, nlevels=30)
#' filledcontourview(branin, dim=2, nlevels=30, col='red', add=TRUE)
#' }
#' }
#'
filledcontourview <- function(...){
    UseMethod("filledcontourview")
}
