#' @param fun a function or 'predict()'-like function that returns a simple numeric or mean and standard error: list(mean=...,se=...).
#' @param vectorized is fun vectorized?
#' @param dim input variables dimension of the model or function.
#' @param conf_blend alpha shadow to apply on fun standard error (se) if returned.
#' @template contourview-doc
#' @rdname contourview
#' @method contourview function
#' @aliases contourview,function,function-method
#' @export
#' @seealso \code{\link{sectionview.function}} for a section plot, and \code{\link{sectionview3d.function}} for a 2D section plot.
#' \code{\link{Vectorize.function}} to wrap as vectorized a non-vectorized function.
#' @examples
#' x1 <- rnorm(15)
#' x2 <- rnorm(15)
#'
#' contourview(function(x) sum(x),
#'                      dim=2, Xlim=cbind(range(x1),range(x2)), col_levels='black')
#'
#' y <- x1 + x2 + rnorm(15)
#' model <- lm(y ~ x1 + x2)
#'
#' points(x1,x2)
#' contourview(function(x) {
#'                       x = as.data.frame(x)
#'                       colnames(x) <- names(model$coefficients[-1])
#'                       p = predict.lm(model, newdata=x, se.fit=TRUE)
#'                       list(mean=p$fit, se=p$se.fit)
#'                     }, vectorized=TRUE, dim=2, Xlim=cbind(range(x1),range(x2)), add=TRUE)
#'
contourview.function <- function(fun, vectorized=FALSE,
                                dim = NULL,
                             center = NULL,
                             lty_center = 2,
                             col_center = "black",
                             axis = NULL,
                             npoints = 20,
                             nlevels = if (is.null(levels)) 10 else length(levels),
                             levels = NULL,
                             lty_levels = 3,
                             col_levels = if (!is.null(col)) col else "blue",
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
            contour(x = xd1,y = xd2, z = yd_mean,
                    xlim = xlim, ylim = ylim, zlim = zlim,
                    col = col_levels,  lty = lty_levels,
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
                    col = col_levels,  lty = lty_levels,
                    levels = levels,
                    add=FALSE,
                    ...)
            if(D>2) {
                abline(v=center[d[1]],col=col_center,lty=lty_center)
                abline(h=center[d[2]],col=col_center,lty=lty_center)
            }
        }

        if (y_has_sd && any(y_sd>0)) {
            for (i in 1:length(levels)) {
                .filled.contour(x = xd1, y = xd2, z = abs(yd_mean - levels[i])-yd_sd,
                                #xlim = xlim, ylim = ylim, zlim = zlim,
                                col = fade(col_levels,alpha = 1-conf_blend),
                                levels = c(-max(y_sd),0))
                #.filled.contour(x = xd1, y = xd2, z = abs(yd_mean - levels[i])-0.25*yd_sd,
                #                #xlim = xlim, ylim = ylim, zlim = zlim,
                #                col = fade(col_levels,alpha = (1-0.25)*conf_blend),
                #                levels = c(-max(y_sd),0))

            }


        }
    }
}


#' @param X the matrix of input design.
#' @param y the array of output values.
#' @param sdy optional array of output standard error.
#' @param col_points color of points.
#' @param bg_blend  an optional factor of alpha (color channel) blending used to plot design points outside from this section.
#' @template contourview-doc
#' @rdname contourview
#' @method contourview matrix
#' @aliases contourview,matrix,matrix-method
#' @export
#' @seealso \code{\link{sectionview.matrix}} for a section plot, and \code{\link{sectionview3d.matrix}} for a 2D section plot.
#' @examples
#' X = matrix(runif(15*2),ncol=2)
#' y = apply(X,1,branin)
#'
#' contourview(X, y)
#'
contourview.matrix <- function(X, y, sdy=NULL,
                           center = NULL,
                           lty_center = 2,
                           col_center = "black",
                           axis = NULL,
                           col_points = if (!is.null(col)) col else "red",
                           col = NULL,
                           bg_blend = 1,
                           mfrow = NULL,
                           Xlab = NULL, ylab = NULL,
                           Xlim = NULL,
                           title = NULL,
                           add = FALSE,
                           ...) {
    X_doe <- X
    y_doe <- y

    D <- ncol(X_doe)
    n <- nrow(X_doe)

    if (is.null(sdy)) {
        sdy_doe <- rep(0, n)
    } else {
        sdy_doe <- rep_len(sdy, n)
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

    if (is.null(mfrow)) {
        nc <- round(sqrt(D))
        nl <- ceiling(D/nc)
        mfrow <- c(nc, nl)
    }

    if (!isTRUE(add)) {
        #if(D>1){
            close.screen( all.screens = TRUE )
            split.screen(figs = mfrow)
        #}
        assign(".split.screen.lim",matrix(NaN,ncol=6,nrow=D),envir=DiceView.env) # xmin,xmax,ymin,ymax matrix of limits, each row for one dim combination
    }

    if (!exists(".split.screen.lim",envir=DiceView.env))
        assign(".split.screen.lim",matrix(NaN,ncol=6,nrow=D),envir=DiceView.env)

    ## Each 'id' will produce a plot
    for (id in 1:dim(axis)[1]) {
        if (D>2) screen(id, new=!add)

        d <- axis[id,]

        ## fading colors for points
        if (D>2) {
            xrel <- scale(x = as.matrix(X_doe),
                          center = center,
                          scale = drx)

            ## ind.nonfix flags the non fixed dims
            ind.nonfix <- (1:D) %in% c(d[1], d[2])
            ind.nonfix <- !ind.nonfix

            alpha <- pmax(0,apply(X = xrel[ , ind.nonfix, drop = FALSE],
                           MARGIN = 1,
                           FUN = function(x) (1 - sqrt(sum(x^2)/D))^bg_blend))
        } else {
            alpha <- rep(1, n)
        }

        if (isTRUE(add)) {
            # re-use global settings for limits of this screen
            .split.screen.lim = get(x=".split.screen.lim",envir=DiceView.env)
            xlim <- c(.split.screen.lim[id,1],.split.screen.lim[id,2])
            ylim <- c(.split.screen.lim[id,3],.split.screen.lim[id,4])
            zlim <- c(.split.screen.lim[id,5],.split.screen.lim[id,6])
        } else {
            xlim = rx[,d[1]]
            ylim = rx[,d[2]]
            zlim = range(y)
            eval(parse(text=paste(".split.screen.lim[",id,",] = matrix(c(",xlim[1],",",xlim[2],",",ylim[1],",",ylim[2],",",zlim[1],",",zlim[2],"),nrow=1)")),envir=DiceView.env)

            if(D>2) {
                abline(v=center[d[1]],col=col_center,lty=lty_center)
                abline(h=center[d[2]],col=col_center,lty=lty_center)
            }
        }

        points(X_doe[,d],
               col = fade(color = col_points, alpha = alpha),
               xlim=xlim,ylim=ylim,
               pch = 20)
    }
}

#' @param eval_str the expression to evaluate in each subplot.
#' @param axis optional matrix of 2-axis combinations to plot, one by row. The value \code{NULL} leads to all possible combinations i.e. \code{choose(D, 2)}.
#' @param mfrow  an optional list to force \code{par(mfrow = ...)} call. The default value  \code{NULL} is automatically set for compact view.
#' @rdname contourview
#' @method contourview character
#' @aliases contourview,character,character-method
#' @export
#' @seealso \code{\link{contourview.matrix}} for a section plot.
#' @examples
#' x1 <- rnorm(15)
#' x2 <- rnorm(15)
#'
#' y <- x1 + x2^2 + rnorm(15)
#' model <- glm(y ~ x1 + I(x2^2))
#'
#' contourview(model)
#'
#' contourview("abline(h=0.25,col='red')")
contourview.character <- function(eval_str,
                                 axis = NULL,
                                 mfrow = NULL,
                                 ...) {

    if (!exists(".split.screen.lim",envir=DiceView.env))
        stop(paste0("Cannot eval '",eval_str,"' when no previous sectionview() was called."))
    else
        .split.screen.lim = get(x=".split.screen.lim",envir=DiceView.env)

    D <- nrow(.split.screen.lim)

    if (is.null(axis)) {
        axis <- matrix(1:D, ncol = 1)
    } else {
        ## added by YD for the vector case
        axis <- matrix(axis, ncol = 1)
    }

    if (is.null(mfrow) && (D>1)) {
        nc <- round(sqrt(D))
        nl <- ceiling(D/nc)
        mfrow <- c(nc, nl)
    }

    ## Each 'id' will produce a plot
    for (id in 1:dim(axis)[1]) {

        d <- axis[id,]

        e = parent.frame()
        assign("d",d,envir=e)
        assign("xlim",c(.split.screen.lim[d,1],.split.screen.lim[d,2]),envir=e)
        assign("ylim",c(.split.screen.lim[d,3],.split.screen.lim[d,4]),envir=e)
        assign("zlim",c(.split.screen.lim[d,5],.split.screen.lim[d,6]),envir=e)

        if (D>2) {
            screen(id, new=FALSE)
            plot(x=c(.split.screen.lim[d,1],.split.screen.lim[d,2]), y=c(.split.screen.lim[d,3],.split.screen.lim[d,4]),
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
#' @param bg_blend  an optional factor of alpha (color channel) blending used to plot design points outside from this section.
#' @param conf_level confidence level hull to display.
#' @param conf_blend an optional factor of alpha (color channel) blending used to plot confidence hull.
#' @template contourview-doc
#' @rdname contourview
#' @method contourview km
#' @aliases contourview,km,km-method
#' @export
#' @seealso \code{\link{sectionview.km}} for a section plot, and \code{\link{sectionview3d.km}} for a 2D section plot.
#' @examples
#' if (requireNamespace("DiceKriging")) { library(DiceKriging)
#'
#' X = matrix(runif(15*2),ncol=2)
#' y = apply(X,1,branin)
#'
#' model <- DiceKriging::km(design = X, response = y, covtype="matern3_2")
#'
#' contourview(model)
#'
#' }
#'
contourview.km <- function(km_model, type = "UK",
                           center = NULL,
                           axis = NULL,
                           npoints = 20,
                           nlevels = if (is.null(levels)) 10 else length(levels),
                           levels = NULL,
                           col_points = if (!is.null(col)) col else "red",
                           col_levels = if (!is.null(col)) col else "blue",
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

    contourview.function(
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
contourview_libKriging <- function(libKriging_model,
                           center = NULL,
                           axis = NULL,
                           npoints = 20,
                           nlevels = if (is.null(levels)) 10 else length(levels),
                           levels = NULL,
                           col_points = if (!is.null(col)) col else "red",
                           col_levels = if (!is.null(col)) col else "blue",
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

    contourview.function(fun = function(x) {
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
#' @template contourview-doc
#' @rdname contourview
#' @method contourview Kriging
#' @aliases contourview,Kriging,Kriging-method
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
#' contourview(model)
#'
#' }
#'
contourview.Kriging <- function(Kriging_model,
                                   center = NULL,
                                   axis = NULL,
                                   npoints = 20,
                                   nlevels = if (is.null(levels)) 10 else length(levels),
                                   levels = NULL,
                                   col_points = if (!is.null(col)) col else "red",
                                   col_levels = if (!is.null(col)) col else "blue",
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
    contourview_libKriging(libKriging_model = Kriging_model,
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
#' @template contourview-doc
#' @rdname contourview
#' @method contourview NuggetKriging
#' @aliases contourview,NuggetKriging,NuggetKriging-method
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
#' contourview(model)
#'
#' }
#'
contourview.NuggetKriging <- function(NuggetKriging_model,
                                center = NULL,
                                axis = NULL,
                                npoints = 20,
                                nlevels = if (is.null(levels)) 10 else length(levels),
                                levels = NULL,
                                col_points = if (!is.null(col)) col else "red",
                                col_levels = if (!is.null(col)) col else "blue",
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
    contourview_libKriging(libKriging_model = NuggetKriging_model,
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
#' @template contourview-doc
#' @rdname contourview
#' @method contourview NoiseKriging
#' @aliases contourview,NoiseKriging,NoiseKriging-method
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
#' contourview(model)
#'
#' }
#'
contourview.NoiseKriging <- function(NoiseKriging_model,
                                      center = NULL,
                                      axis = NULL,
                                      npoints = 20,
                                      nlevels = if (is.null(levels)) 10 else length(levels),
                                      levels = NULL,
                                      col_points = if (!is.null(col)) col else "red",
                                      col_levels = if (!is.null(col)) col else "blue",
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
    contourview_libKriging(libKriging_model = NoiseKriging_model,
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
#' @template contourview-doc
#' @rdname contourview
#' @method contourview glm
#' @aliases contourview,glm,glm-method
#' @export
#' @seealso \code{\link{sectionview.glm}} for a section plot, and \code{\link{sectionview3d.glm}} for a 2D section plot.
#' @examples
#' x1 <- rnorm(15)
#' x2 <- rnorm(15)
#'
#' y <- x1 + x2^2 + rnorm(15)
#' model <- glm(y ~ x1 + I(x2^2))
#'
#' contourview(model)
#'
contourview.glm <- function(glm_model,
                           center = NULL,
                           axis = NULL,
                           npoints = 20,
                           nlevels = if (is.null(levels)) 10 else length(levels),
                           levels = NULL,
                           col_points = if (!is.null(col)) col else "red",
                           col_levels = if (!is.null(col)) col else "blue",
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

    contourview.function(
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
#' @template contourview-doc
#' @rdname contourview
#' @method contourview list
#' @aliases contourview,list,list-method
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
#' contourview(model)
#'
#' }
#'
contourview.list <- function(modelFit_model,
                            center = NULL,
                            axis = NULL,
                            npoints = 20,
                            nlevels = if (is.null(levels)) 10 else length(levels),
                            levels = NULL,
                            col_points = if (!is.null(col)) col else "red",
                            col_levels = if (!is.null(col)) col else "blue",
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

    contourview.function(
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





#### Wrapper for contourview ####

#' @import methods
if(!isGeneric("contourview")) {
    setGeneric(name = "contourview",
               def = function(...) standardGeneric("contourview")
    )
}

#' @title Plot a contour view of a prediction model or function, including design points if available.
#' @details If available, experimental points are plotted with fading colors. Points that fall in the specified section (if any) have the color specified \code{col_points} while points far away from the center have shaded versions of the same color. The amount of fading is determined using the Euclidean distance between the plotted point and \code{center}.
#' @param ... arguments of the \code{contourview.km}, \code{contourview.glm}, \code{contourview.Kriging} or \code{contourview.function} function
#' @export
#' @examples
#' ## A 2D example - Branin-Hoo function
#' contourview(branin, dim=2, nlevels=30, col='black')
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
#' contourview(model, nlevels=30)
#' contourview(branin, dim=2, nlevels=30, col='red', add=TRUE)
#' }
#'
#' ## model: Kriging
#' if (requireNamespace("rlibkriging")) { library(rlibkriging)
#' model <- Kriging(X = as.matrix(design.fact), y = as.matrix(y), kernel="matern3_2")
#' contourview(model, nlevels=30)
#' contourview(branin, dim=2, nlevels=30, col='red', add=TRUE)
#' }
#'
#' ## model: glm
#' model <- glm(y ~ 1+ x1 + x2 + I(x1^2) + I(x2^2) + x1*x2, data=cbind(y,design.fact))
#' contourview(model, nlevels=30)
#' contourview(branin, dim=2, nlevels=30, col='red', add=TRUE)
#'
#' if (requireNamespace("DiceEval")) { library(DiceEval)
#' ## model: StepLinear
#' model <- modelFit(design.fact, y, type = "StepLinear")
#' contourview(model, nlevels=30)
#' contourview(branin, dim=2, nlevels=30, col='red', add=TRUE)
#' }
#' }
#'
contourview <- function(...){
    UseMethod("contourview")
}
