#' @param fun a function or 'predict()'-like function that returns a simple numeric or mean and standard error: list(mean=...,se=...).
#' @param vectorized is fun vectorized?
#' @param dim input variables dimension of the model or function.
#' @param conf_lev an optional list of confidence interval values to display.
#' @param conf_blend an optional factor of alpha (color channel) blending used to plot confidence intervals.
#' @template sectionview-doc
#' @rdname sectionview
#' @method sectionview function
#' @aliases sectionview,function,function-method
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
#' sectionview(function(x) sum(x),
#'                      dim=2, center=c(0,0), Xlim=cbind(range(x1),range(x2)), col='black')
#'
#' sectionview(function(x) {
#'                       x = as.data.frame(x)
#'                       colnames(x) <- names(model$coefficients[-1])
#'                       p = predict.lm(model, newdata=x, se.fit=TRUE)
#'                       list(mean=p$fit, se=p$se.fit)
#'                     }, vectorized=TRUE,
#'                     dim=2, center=c(0,0), Xlim=cbind(range(x1),range(x2)), add=TRUE)
#'
sectionview.function <- function(fun, vectorized=FALSE,
                                dim = NULL,
                             center = NULL,
                             axis = NULL,
                             npoints = 100,
                             col_surf = "blue",
                             conf_lev = c(0.5, 0.8, 0.9, 0.95, 0.99),
                             conf_blend = NULL,
                             mfrow = NULL,
                             Xlab = NULL, ylab = NULL,
                             Xlim = NULL, ylim=NULL,
                             title = NULL,
                             add = FALSE,
                             ...) {
    if (!is.null(dim)) {
        D <- dim
        if (is.null(center))
            if (D != 1) stop("Section center in 'section' required for >2-D model.")
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

    if (is.null(axis)) {
        axis <- matrix(1:D, ncol = 1)
    } else {
        ## added by YD for the vector case
        axis <- matrix(axis, ncol = 1)
    }

    if (is.null(conf_blend) ||
        length(conf_blend) != length(conf_lev))
        conf_blend <- rep(0.5/length(conf_lev), length(conf_lev))

    if (is.null(mfrow) && (D>1)) {
        nc <- round(sqrt(D))
        nl <- ceiling(D/nc)
        mfrow <- c(nc, nl)
    }

    if (!isTRUE(add)) {
        if(D>1){
            close.screen( all.screens = TRUE )
            split.screen(figs = mfrow)
        }
        assign(".split.screen.lim",matrix(NaN,ncol=6,nrow=D),envir=DiceView.env) # xmin,xmax,ymin,ymax matrix of limits, each row for one dim combination
    }

    npoints <- rep(npoints, length.out = D)

    ## find limits: 'rx' is matrix with min in row 1 and max in row 2
    if(!is.null(Xlim))
        rx <- matrix(Xlim,nrow=2,ncol=D)
    else
        rx <- matrix(c(0,1),nrow=2,ncol=D)
    rownames(rx) <- c("min", "max")
    drx <- rx["max", ] - rx["min", ]

    ## define X & y labels
    if (is.null(ylab)) ylab <- "y"
    if (is.null(Xlab)) Xlab <- paste(sep = "", "X", 1:D)

    ## try to find a good formatted value 'fcenter' for 'center'
    fcenter <- tryFormat(x = center, drx = drx)

    ## Each 'id' will produce a RGL plot
    for (id in 1:dim(axis)[1]) {
        if (D>1) screen(id, new=!add)

        d <- axis[id, ]

        xdmin <- rx["min", d]
        xdmax <- rx["max", d]
        xlim = c(xdmin,xdmax)

        xd <- seq(from = xdmin, to = xdmax, length.out = npoints[d])
        x <- data.frame(t(matrix(as.numeric(center), nrow = D, ncol = npoints[d])))
        if (!is.null(center)) if(!is.null(names(center))) names(x) <- names(center)
        x[ , d] <- xd

        y_mean <- array(NA, npoints)
        y_sd <- array(0, npoints)

        y <- Fun(as.matrix(x))
        if (is.list(y)) {
            if (!("mean" %in% names(y)) || !("se" %in% names(y)))
                stop(paste0("If function returns a list, it must have 'mean' and 'se', while had ",paste0(collapse=",",names(y))))
            y_mean <- as.numeric(y$mean)
            y_sd <- as.numeric(y$se)
        } else { # simple function, not a list
            if (!is.numeric(y))
                stop("If function does not returns a list, it must be numeric.")
            y_mean <- as.numeric(y)
            y_sd <- 0
        }

        if (is.null(title)){
            if (D>1) {
                title_d <- paste(collapse = ", ", paste(Xlab[-d], '=', fcenter[-d]))
            } else {
                title_d <- paste(collapse = "~", ylab, Xlab[d])}
        } else {
            title_d <- title
        }

        if (is.null(ylim)) {
            ylim <- c(min(y_mean-3*y_sd),max(y_mean+3*y_sd))
        }
        zlim <- c(NA,NA) #Not used for this kind of plot

        ## plot mean surface two steps required to use alpha =
        if (isTRUE(add)) {
            # re-use global settings for limits of this screen
            .split.screen.lim = get(x=".split.screen.lim",envir=DiceView.env)
            xlim <- c(.split.screen.lim[d,1],.split.screen.lim[d,2])
            ylim <- c(.split.screen.lim[d,3],.split.screen.lim[d,4])
            zlim <- c(.split.screen.lim[d,5],.split.screen.lim[d,6])
            if (D>1) {
                plot(xd, y_mean,
                     xlim=xlim, ylim=ylim,
                     type = "l",
                     col = col_surf, xlab="", ylab="",
                     ...)
            } else { # not using screen(), so need for a non reset plotting method
                lines(xd, y_mean,
                      xlim=xlim, ylim=ylim,
                      col = col_surf,
                      ...)
            }
        } else {
            eval(parse(text=paste(".split.screen.lim[",d,",] = matrix(c(",xlim[1],",",xlim[2],",",ylim[1],",",ylim[2],",",zlim[1],",",zlim[2],"),nrow=1)")),envir=DiceView.env)
            plot(xd, y_mean,
                 xlab = Xlab[d], ylab = ylab,
                 xlim = xlim, ylim = ylim,
                 main = title_d,
                 type = "l",
                 col = col_surf,
                 ...)
            if(D>1) abline(v=center[d],col='black',lty=2)
        }

        ## 'confidence band' filled with the suitable color
        for (p in 1:length(conf_lev)) {
            polygon(c(xd,rev(xd)),
                    c(qnorm((1+conf_lev[p])/2, y_mean, y_sd),
                      rev(qnorm((1-conf_lev[p])/2, y_mean, y_sd))),
                    col = translude(col_surf, alpha = conf_blend[p]),
                    border = NA)
        }
    }
}


#' @param X the matrix of input design.
#' @param y the array of output values.
#' @param sdy optional array of output standard error.
#' @param col_points color of points.
#' @param conf_lev an optional list of confidence interval values to display.
#' @param conf_blend an optional factor of alpha (color channel) blending used to plot confidence intervals.
#' @param bg_blend  an optional factor of alpha (color channel) blending used to plot design points outside from this section.
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
sectionview.matrix<- function(X, y, sdy=NULL,
                              center = NULL,
                              axis = NULL,
                              npoints = 100,
                              col_points = "red",
                              conf_lev = c(0.5, 0.8, 0.9, 0.95, 0.99),
                              conf_blend = NULL,
                              bg_blend = 5,
                              mfrow = NULL,
                              Xlab = NULL, ylab = NULL,
                              Xlim = NULL, ylim=NULL,
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
    drx <- rx["max", ] - rx["min", ]

    if (is.null(ylim)) {
        ymin <- min(y_doe-3*sdy_doe)
        ymax <- max(y_doe+3*sdy_doe)
        ylim <- c(ymin, ymax)
    }

    ## define X & y labels
    if (is.null(ylab)) ylab <- names(y_doe)
    if (is.null(Xlab)) Xlab <- names(X_doe)

    if (is.null(axis)) {
        axis <- matrix(1:D, ncol = 1)
    } else {
        ## added by YD for the vector case
        axis <- matrix(axis, ncol = 1)
    }

    if (is.null(conf_blend) ||
        length(conf_blend) != length(conf_lev))
        conf_blend <- rep(0.5/length(conf_lev), length(conf_lev))

    if (is.null(mfrow) && (D>1)) {
        nc <- round(sqrt(D))
        nl <- ceiling(D/nc)
        mfrow <- c(nc, nl)
    }

    if (!isTRUE(add)) {
        if(D>1){
            close.screen( all.screens = TRUE )
            split.screen(figs = mfrow)
        }
        assign(".split.screen.lim",matrix(NaN,ncol=6,nrow=D),envir=DiceView.env) # xmin,xmax,ymin,ymax matrix of limits, each row for one dim combination
    }

    ## define X & y labels
    if (is.null(ylab)) ylab <- "y"
    if (is.null(Xlab)) Xlab <- paste(sep = "", "X", 1:D)

    fcenter <- tryFormat(x = center, drx = drx)

    ## Each 'id' will produce a plot
    for (id in 1:dim(axis)[1]) {
        if (D>1) screen(id, new=!add)

        d <- axis[id,]

        xdmin <- rx["min", d]
        xdmax <- rx["max", d]
        xlim = c(xdmin,xdmax)

        if (is.null(title)){
            if (D>1) {
                title_d <- paste(collapse = ", ", paste(Xlab[-d], '=', fcenter[-d]))
            } else {
                title_d <- paste(collapse = "~", ylab, Xlab[d])}
        } else {
            title_d <- title
        }

        if (is.null(ylim)) {
            ylim <- c(min(y-3*sdy),max(y+3*sdy))
        }
        zlim <- c(NA,NA) #Not used for this kind of plot

        ## fading colors for points
        if (D>1) {
            xrel <- scale(x = as.matrix(X_doe),
                          center = center,
                          scale = drx)

            ## ind.nonfix flags the non fixed dims
            ind.nonfix <- (1:D) %in% d[1] #c(d[1], d[2])
            ind.nonfix <- !ind.nonfix

            alpha <- apply(X = xrel[ , ind.nonfix, drop = FALSE],
                           MARGIN = 1,
                           FUN = function(x) (1 - sqrt(sum(x^2)/D))^bg_blend)
        } else {
            alpha <- rep(1, n)
        }

        if (add) {
            .split.screen.lim = get(x=".split.screen.lim",envir=DiceView.env)
            xlim <- c(.split.screen.lim[d,1],.split.screen.lim[d,2])
            ylim <- c(.split.screen.lim[d,3],.split.screen.lim[d,4])
            zlim <- c(.split.screen.lim[d,5],.split.screen.lim[d,6])
            if (D>1)
                plot(x=X_doe[,d], y=y_doe,
                       col = fade(color = col_points, alpha = alpha),
                       pch = 20,type=if (is.null(sdy)) 'p' else 'n',
                       xlab="",ylab="", xlim=xlim, ylim=ylim) # Cannot use 'points' so use 'plot' with these neutral args
            else
                points(x=X_doe[,d], y=y_doe,
                 col = fade(color = col_points, alpha = alpha),
                 pch = 20,type=if (is.null(sdy)) 'p' else 'n',
                 xlab="",ylab="", xlim=xlim, ylim=ylim)
        } else {
            eval(parse(text=paste(".split.screen.lim[",d,",] = matrix(c(",xlim[1],",",xlim[2],",",ylim[1],",",ylim[2],",",zlim[1],",",zlim[2],"),nrow=1)")),envir=DiceView.env)
            plot(X_doe[,d], y_doe,
                 col = fade(color = col_points, alpha = alpha),
                 pch = 20,type='p',
                 xlab=Xlab[d], ylab=ylab, xlim=xlim, ylim=ylim)
        }

        if (!is.null(sdy))
            for (p in 1:length(conf_lev)) {
                for (i in 1:n) {
                    if (sdy_doe[i]>0)
                        lines(x=c(X_doe[i,d],X_doe[i,d]),
                              y=c(qnorm((1+conf_lev[p])/2, y_doe[i], sdy_doe[i]),
                                  qnorm((1-conf_lev[p])/2, y_doe[i], sdy_doe[i])),
                              col = rgb(1,1-alpha[i], 1-alpha[i], alpha[i]*conf_blend[p]),
                              lwd = 5, lend = 1)
                    else
                        points(x=X_doe[i,d],y=y_doe[i],
                               col = rgb(1,1-alpha[i], 1-alpha[i], alpha[i]*conf_blend[p]),
                               pch = 15, lwd = 5)
                }
            }
    }
}

#' @param km_model an object of class \code{"km"}.
#' @param type the kriging type to use for model prediction.
#' @param col_points color of points.
#' @param conf_lev an optional list of confidence interval values to display.
#' @param conf_blend an optional factor of alpha (color channel) blending used to plot confidence intervals.
#' @param bg_blend  an optional factor of alpha (color channel) blending used to plot design points outside from this section.
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
                           npoints = 100,
                           col_points = "red",
                           col_surf = "blue",
                           conf_lev = c(0.5, 0.8, 0.9, 0.95, 0.99),
                           conf_blend = NULL,
                           bg_blend = 5,
                           mfrow = NULL,
                           Xlab = NULL, ylab = NULL,
                           Xlim = NULL, ylim=NULL,
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
    drx <- rx["max", ] - rx["min", ]

    if (is.null(ylim)) {
        ymin <- min(y_doe-3*sdy_doe)
        ymax <- max(y_doe+3*sdy_doe)
        ylim <- c(ymin, ymax)
    }

    ## define X & y labels
    if (is.null(ylab)) ylab <- names(y_doe)
    if (is.null(Xlab)) Xlab <- names(X_doe)

    if (is.null(axis)) {
        axis <- matrix(1:D, ncol = 1)
    } else {
        ## added by YD for the vector case
        axis <- matrix(axis, ncol = 1)
    }

    if (is.null(conf_blend) ||
        length(conf_blend) != length(conf_lev))
        conf_blend <- rep(0.5/length(conf_lev), length(conf_lev))

    sectionview.function(
        fun = function(x) {
            p = DiceKriging::predict.km(km_model,type=type,newdata=x,checkNames=FALSE)
            list(mean=p$mean, se=p$sd)
        }, vectorized=TRUE,
    dim = D, center = center,axis = axis,npoints = npoints,
    col_surf = col_surf, conf_lev=conf_lev, conf_blend=conf_blend,
    mfrow = mfrow, Xlab = Xlab, ylab = ylab,
    Xlim = rx, ylim=ylim, title = title, add = add, ...)

    sectionview.matrix(X = X_doe, y = y_doe, sdy = sdy_doe,
                       dim = D, center = center, axis = axis,
                       col_points = col_points, conf_lev = conf_lev, conf_blend = conf_blend, bg_blend = bg_blend,
                       mfrow = mfrow,
                       Xlim = rx, ylim=ylim,
                       add=TRUE)

}

#' @param libKriging_model an object of class \code{"Kriging"}, \code{"NuggetKriging"} or \code{"NoiseKriging"}.
#' @param col_points color of points.
#' @param bg_blend  an optional factor of alpha (color channel) blending used to plot design points outside from this section.
sectionview.libKriging <- function(libKriging_model,
                           center = NULL,
                           axis = NULL,
                           npoints = 100,
                           col_points = "red",
                           col_surf = "blue",
                           conf_lev = c(0.5, 0.8, 0.9, 0.95, 0.99),
                           conf_blend = NULL,
                           bg_blend = 5,
                           mfrow = NULL,
                           Xlab = NULL, ylab = NULL,
                           Xlim = NULL, ylim=NULL,
                           title = NULL,
                           add = FALSE,
                           ...) {
    X_doe <- libKriging_model$X()
    y_doe <- libKriging_model$y()

    D <- ncol(X_doe)
    n <- nrow(X_doe)

    if (inherits(libKriging_model, "Kriging")) {
        sdy_doe <- NULL #rep(0, n)
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
    drx <- rx["max", ] - rx["min", ]

    if (is.null(ylim)) {
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

    if (is.null(axis)) {
        axis <- matrix(1:D, ncol = 1)
    } else {
        ## added by YD for the vector case
        axis <- matrix(axis, ncol = 1)
    }

    if (is.null(conf_blend) ||
        length(conf_blend) != length(conf_lev))
        conf_blend <- rep(0.5/length(conf_lev), length(conf_lev))

    sectionview.function(fun = function(x) {
            p = rlibkriging::predict(libKriging_model,x,stdev=TRUE)
            list(mean=p$mean, se=p$stdev)
        }, vectorized=TRUE,
        dim = D, center = center,axis = axis,npoints = npoints,
        col_surf = col_surf,conf_lev=conf_lev,conf_blend=conf_blend,
        mfrow = mfrow, Xlab = Xlab, ylab = ylab,
        Xlim = rx, ylim=ylim, title = title, add = add, ...)

    sectionview.matrix(X = X_doe, y = y_doe, sdy = sdy_doe,
                       dim = D, center = center, axis = axis,
                       col_points = col_points, conf_lev = conf_lev, conf_blend = conf_blend, bg_blend = bg_blend,
                       mfrow = mfrow,
                       Xlim = rx, ylim=ylim,
                       add=TRUE)
}

#' @param Kriging_model an object of class \code{"Kriging"}.
#' @param col_points color of points.
#' @param conf_lev an optional list of confidence interval values to display.
#' @param conf_blend an optional factor of alpha (color channel) blending used to plot confidence intervals.
#' @param bg_blend  an optional factor of alpha (color channel) blending used to plot design points outside from this section.
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
                                   npoints = 100,
                                   col_points = "red",
                                   col_surf = "blue",
                                   conf_lev = c(0.5, 0.8, 0.9, 0.95, 0.99),
                                   conf_blend = NULL,
                                   bg_blend = 5,
                                   mfrow = NULL,
                                   Xlab = NULL, ylab = NULL,
                                   Xlim = NULL, ylim=NULL,
                                   title = NULL,
                                   add = FALSE,
                                   ...) {
    sectionview.libKriging(Kriging_model,center,axis,npoints,col_points,col_surf,conf_lev,conf_blend,bg_blend,mfrow,Xlab, ylab,Xlim,ylim,title,add,...)
}

#' @param NuggetKriging_model an object of class \code{"Kriging"}.
#' @param col_points color of points.
#' @param conf_lev an optional list of confidence interval values to display.
#' @param conf_blend an optional factor of alpha (color channel) blending used to plot confidence intervals.
#' @param bg_blend  an optional factor of alpha (color channel) blending used to plot design points outside from this section.
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
                                npoints = 100,
                                col_points = "red",
                                col_surf = "blue",
                                conf_lev = c(0.5, 0.8, 0.9, 0.95, 0.99),
                                conf_blend = NULL,
                                bg_blend = 5,
                                mfrow = NULL,
                                Xlab = NULL, ylab = NULL,
                                Xlim = NULL, ylim=NULL,
                                title = NULL,
                                add = FALSE,
                                ...) {
    sectionview.libKriging(NuggetKriging_model,center,axis,npoints,col_points,col_surf,conf_lev,conf_blend,bg_blend,mfrow,Xlab, ylab,Xlim,ylim,title,add,...)
}

#' @param NoiseKriging_model an object of class \code{"Kriging"}.
#' @param col_points color of points.
#' @param conf_lev an optional list of confidence interval values to display.
#' @param conf_blend an optional factor of alpha (color channel) blending used to plot confidence intervals.
#' @param bg_blend  an optional factor of alpha (color channel) blending used to plot design points outside from this section.
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
                                      npoints = 100,
                                      col_points = "red",
                                      col_surf = "blue",
                                      conf_lev = c(0.5, 0.8, 0.9, 0.95, 0.99),
                                      conf_blend = NULL,
                                      bg_blend = 5,
                                      mfrow = NULL,
                                      Xlab = NULL, ylab = NULL,
                                      Xlim = NULL, ylim=NULL,
                                      title = NULL,
                                      add = FALSE,
                                      ...) {
    sectionview.libKriging(NoiseKriging_model,center,axis,npoints,col_points,col_surf,conf_lev,conf_blend,bg_blend,mfrow,Xlab, ylab,Xlim,ylim,title,add,...)
}

#' @param glm_model an object of class \code{"glm"}.
#' @param col_points color of points.
#' @param conf_lev an optional list of confidence interval values to display.
#' @param conf_blend an optional factor of alpha (color channel) blending used to plot confidence intervals.
#' @param bg_blend  an optional factor of alpha (color channel) blending used to plot design points outside from this section.
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
                           npoints = 100,
                           col_points = "red",
                           col_surf = "blue",
                           conf_lev = c(0.5, 0.8, 0.9, 0.95, 0.99),
                           conf_blend = NULL,
                           bg_blend = 5,
                           mfrow = NULL,
                           Xlab = NULL, ylab = NULL,
                           Xlim = NULL, ylim=NULL,
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
    drx <- rx["max", ] - rx["min", ]

    if (is.null(axis)) {
        axis <- matrix(1:D, ncol = 1)
    } else {
        ## added by YD for the vector case
        axis <- matrix(axis, ncol = 1)
    }

    if (is.null(conf_blend) ||
        length(conf_blend) != length(conf_lev))
        conf_blend <- rep(0.5/length(conf_lev), length(conf_lev))

    sectionview.function(
        fun = function(x) {
            x = as.data.frame(x)
            colnames(x) <- Xlab
            p = predict.glm(glm_model, newdata=x, se.fit=TRUE)
            list(mean=p$fit, se=p$se.fit)
        }, vectorized=TRUE,
        dim = D, center = center,axis = axis, npoints = npoints,
        col_surf = col_surf,conf_lev=conf_lev,conf_blend=conf_blend,
        mfrow = mfrow, Xlab = Xlab, ylab = ylab,
        Xlim = rx, ylim=range(y_doe), title = title, add = add, ...)

    sectionview.matrix(X = X_doe, y = y_doe, sdy = NULL,
                       dim = D, center = center, axis = axis,
                       col_points = col_points, conf_lev = conf_lev, conf_blend = conf_blend, bg_blend = bg_blend,
                       mfrow = mfrow,
                       Xlim = rx, ylim=range(y_doe),
                       add=TRUE)
}

#' @param modelFit_model an object returned by DiceEval::modelFit.
#' @param col_points color of points.
#' @param bg_blend  an optional factor of alpha (color channel) blending used to plot design points outside from this section.
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
                            npoints = 100,
                            col_points = "red",
                            col_surf = "blue",
                            bg_blend = 5,
                            mfrow = NULL,
                            Xlab = NULL, ylab = NULL,
                            Xlim = NULL, ylim=NULL,
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
    drx <- rx["max", ] - rx["min", ]

    if (is.null(axis)) {
        axis <- matrix(1:D, ncol = 1)
    } else {
        ## added by YD for the vector case
        axis <- matrix(axis, ncol = 1)
    }

    sectionview.function(
        fun = function(x) {
            x = as.data.frame(x)
            colnames(x) <- Xlab
            DiceEval::modelPredict(modelFit_model, x)
        }, vectorized=TRUE,
        dim = D, center = center,axis = axis, npoints = npoints,
        col_surf = col_surf,
        mfrow = mfrow, Xlab = Xlab, ylab = ylab,
        Xlim = rx, ylim=range(y_doe), title = title, add = add, ...)

    sectionview.matrix(X = X_doe, y = y_doe, sdy = NULL,
                       dim = D, center = center, axis = axis,
                       col_points = col_points, bg_blend = bg_blend,
                       mfrow = mfrow,
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
