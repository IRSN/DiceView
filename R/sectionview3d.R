#' @param fun a function or 'predict()'-like function that returns a simple numeric or mean and standard error: list(mean=...,se=...).
#' @param vectorized is fun vectorized?
#' @param dim input variables dimension of the model or function.
#' @param conf_lev an optional list of confidence interval values to display.
#' @param conf_blend an optional factor of alpha (color channel) blending used to plot confidence intervals.
#' @template sectionview3d-doc
#' @rdname sectionview3d
#' @method sectionview3d function
#' @aliases sectionview3d,function,function-method
#' @export
#' @seealso \code{\link{sectionview.function}} for a section plot, and \code{\link{sectionview3d.function}} for a 2D section plot.
#' \code{\link{Vectorize.function}} to wrap as vectorized a non-vectorized function.
#' @examples
#' x1 <- rnorm(15)
#' x2 <- rnorm(15)
#'
#' y <- x1 + x2 + rnorm(15)
#' DiceView:::open3d(); DiceView:::plot3d(x1,x2,y)
#'
#' model <- lm(y ~ x1 + x2)
#'
#' sectionview3d(function(x) sum(x),
#'                      dim=2, Xlim=cbind(range(x1),range(x2)), add=TRUE, col='black')
#'
#' sectionview3d(function(x) {
#'                       x = as.data.frame(x)
#'                       colnames(x) <- names(model$coefficients[-1])
#'                       p = predict.lm(model, newdata=x, se.fit=TRUE)
#'                       list(mean=p$fit, se=p$se.fit)
#'                     }, vectorized=TRUE, dim=2, Xlim=cbind(range(x1),range(x2)), add=TRUE)
#'
sectionview3d.function <- function(fun, vectorized=FALSE,
                                dim = NULL,
                             center = NULL,
                             axis = NULL,
                             npoints = 20,
                             col_surf = "blue",
                             conf_lev = c(0.95),
                             conf_blend = NULL,
                             mfrow = NULL,
                             Xlab = NULL, ylab = NULL,
                             Xlim = NULL, ylim = NULL,
                             title = NULL,
                             add = FALSE,
                             engine3d = NULL,
                             ...) {
    load3d(engine3d)

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

    if (D>2 && engine3d!="rgl") {
        if (is.null(mfrow) && (D>1)) {
            nc <- round(sqrt(D))
            nl <- ceiling(D/nc)
            mfrow <- c(nc, nl)
        }
    }

    if (!isTRUE(add)) {
        if(D>2){
            close.screen( all.screens = TRUE )
            split.screen(figs = mfrow)
        }
        assign(".split.screen.lim",matrix(NaN,ncol=6,nrow=D),envir=DiceView.env) # xmin,xmax,ymin,ymax matrix of limits, each row for one dim combination
    }

    ## Changed by YD: a vector
    ## if (is.null(dim(npoints))) { npoints <- rep(npoints,D) }
    npoints <- rep(npoints, length.out = D)

    if (is.null(conf_blend) || length(conf_blend) != length(conf_lev)) {
        conf_blend <- rep(0.5/length(conf_lev), length(conf_lev))
    }

    ## find limits: 'rx' is matrix with min in row 1 and max in row 2
    if(!is.null(Xlim))
        rx <- matrix(Xlim,nrow=2,ncol=D)
    else
        rx <- matrix(c(0,1),nrow=2,ncol=D)
    rownames(rx) <- c("min", "max")
    drx <- rx["max", ] - rx["min", ]

    zlim <- ylim

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
        if (D>2 && engine3d!="rgl") screen(id, new=!add)

        d <- axis[id, ]

        npoints_all <- npoints[d[1]]*npoints[d[2]]

        ## ind.nonfix flags the non fixed dims
        ind.nonfix <- (1:D) %in% c(d[1], d[2])
        ind.nonfix <- !ind.nonfix

        xdmin <- rx["min", d]
        xdmax <- rx["max", d]

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
        yd_mean <- matrix(y_mean,ncol=npoints[2],nrow=npoints[1])
        yd_sd <- matrix(y_sd,ncol=npoints[2],nrow=npoints[1])

        if (is.null(title)){
            if (D>2) {
                title_d <-  paste(collapse = ", ", paste(Xlab[ind.nonfix],'=', fcenter[ind.nonfix]))
            } else {
                title_d <- paste(collapse = "~", ylab, paste(collapse = ",", Xlab[d[1]], Xlab[d[2]]))
            }
        } else {
            title_d <-  title
        }

        xlim <- rx[ , d[1]]
        ylim <- rx[ , d[2]]
        if (is.null(zlim)) {
            zlim <- c(min(y_mean+3*y_sd),max(y_mean-3*y_sd))
        }

        if (isTRUE(add)) {
            .split.screen.lim = get(x=".split.screen.lim",envir=DiceView.env)
            xlim <- c(.split.screen.lim[d,1],.split.screen.lim[d,2])
            ylim <- c(.split.screen.lim[d,3],.split.screen.lim[d,4])
            zlim <- c(.split.screen.lim[d,5],.split.screen.lim[d,6])
        } else {
            eval(parse(text=paste(".split.screen.lim[",d,",] = matrix(c(",xlim[1],",",xlim[2],",",ylim[1],",",ylim[2],",",zlim[1],",",zlim[2],"),nrow=1)")),envir=DiceView.env)
            open3d()
            plot3d(x = x[ , 1], y = x[ , 2], z = y_mean,
                xlab = Xlab[d[1]], ylab = Xlab[d[2]], zlab = ylab,
                xlim = xlim, ylim = ylim, zlim = zlim, type = "n",
                main = title_d,
                col = col_surf,
                ...)
        }

        surface3d(x = xd1,y = xd2, z = yd_mean,
                col = col_surf, alpha = 0.5,
                box = FALSE)

        ## add  "confidence surfaces"
        for (p in 1:length(conf_lev)) {
            surface3d(x = xd1,
                    y = xd2,
                    z = qnorm((1+conf_lev[p])/2, y_mean, y_sd),
                    col = col_surf,
                    alpha = conf_blend[p],
                    box = FALSE)

            surface3d(x = xd1,
                    y = xd2,
                    z = qnorm((1-conf_lev[p])/2, y_mean, y_sd),
                    col = col_surf,
                    alpha = conf_blend[p],
                    box = FALSE)

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
sectionview3d.matrix <- function(X, y, sdy = NULL,
                             center = NULL,
                             axis = NULL,
                             col_points = "red",
                             conf_lev = c(0.95),
                             conf_blend = NULL,
                             bg_blend = 1,
                             mfrow = NULL,
                             Xlab = NULL, ylab = NULL,
                             Xlim = NULL, ylim = NULL,
                             title = NULL,
                             add = FALSE,
                             engine3d = NULL,
                             ...) {
    load3d(engine3d)

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

    ## define X & y labels
    if (is.null(ylab)) ylab <- names(y_doe)
    if (is.null(Xlab)) Xlab <- names(X_doe)

    if (is.null(axis)) {
        axis <- t(utils::combn(D, 2))
    } else {
        axis <- matrix(axis, ncol = 2)
    }

    if (D>2 && engine3d!="rgl") {
        if (is.null(mfrow) && (D>1)) {
            nc <- round(sqrt(D))
            nl <- ceiling(D/nc)
            mfrow <- c(nc, nl)
        }
    }

    if (!isTRUE(add)) {
        if(D>2){
            close.screen( all.screens = TRUE )
            split.screen(figs = mfrow)
        }
        assign(".split.screen.lim",matrix(NaN,ncol=6,nrow=D),envir=DiceView.env) # xmin,xmax,ymin,ymax matrix of limits, each row for one dim combination
    }

    if (is.null(conf_blend) || length(conf_blend) != length(conf_lev)) {
        conf_blend <- rep(0.5/length(conf_lev), length(conf_lev))
    }

    ## Each 'id' will produce a plot
    for (id in 1:dim(axis)[1]) {
        if (D>2 && engine3d!="rgl") screen(id, new=!add)

        d <- axis[id,]

        ## fading colors for points
        if (D>2) {
            xrel <- scale(x = as.matrix(X_doe),
                          center = center,
                          scale = drx)

            ## ind.nonfix flags the non fixed dims
            ind.nonfix <- (1:D) %in% c(d[1], d[2])
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
            points3d(x = X_doe[ , d[1]], y = X_doe[ , d[2]], z = y_doe,
                     col = col_points,
                     alpha = alpha,
                     pch = 20, size=if (is.null(sdy)) 3 else 0, box = FALSE,
                     xlim=xlim, ylim=ylim, zlim=zlim)
        } else {
            xlim = rx[,d[1]]
            ylim = rx[,d[2]]
            zlim = range(y_doe)
            eval(parse(text=paste(".split.screen.lim[",d,",] = matrix(c(",xlim[1],",",xlim[2],",",ylim[1],",",ylim[2],",",zlim[1],",",zlim[2],"),nrow=1)")),envir=DiceView.env)
            open3d()
            plot3d(x = X_doe[ , d[1]], y = X_doe[ , d[2]], z = y_doe,
                     col = col_points,
                     alpha = alpha,
                     pch = 20, size=if (is.null(sdy)) 3 else 0, box = TRUE,
                     xlab=Xlab[d], ylab=ylab,
                     xlim=xlim, ylim=ylim, zlim=zlim)
        }

        if (!is.null(sdy))
            for (p in 1:length(conf_lev)) {
                for (i in 1:n) {
                    lines3d(x = c(X_doe[i, d[1]], X_doe[i, d[1]]),
                    y = c(X_doe[i, d[2]], X_doe[i, d[2]]),
                    z = c(qnorm((1+conf_lev[p])/2, y_doe[i], sdy_doe[i]),
                          qnorm((1-conf_lev[p])/2, y_doe[i], sdy_doe[i])),
                    col = col_points,
                    alpha = alpha[i]*conf_blend[p],
                    lwd = 5, lend = 1, box = FALSE)
                }}
        # else
        #     for (p in 1:length(conf_lev)) {
        #         for (i in 1:n) {
        #             points3d(x = X_doe[i, d[1]],
        #              y = X_doe[i, d[2]],
        #              z = y_doe[i],
        #              col = col_points,
        #              alpha = alpha[i]*conf_blend[p],
        #              pch = 15, box = FALSE)
        #         }}
    }
}

#' @param km_model an object of class \code{"km"}.
#' @param type the kriging type to use for model prediction.
#' @param col_points color of points.
#' @param conf_lev an optional list of confidence interval values to display.
#' @param conf_blend an optional factor of alpha (color channel) blending used to plot confidence intervals.
#' @param bg_blend  an optional factor of alpha (color channel) blending used to plot design points outside from this section.
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
                           npoints = 20,
                           col_points = "red",
                           col_surf = "blue",
                           conf_lev = c(0.95),
                           conf_blend = NULL,
                           bg_blend = 1,
                           mfrow = NULL,
                           Xlab = NULL, ylab = NULL,
                           Xlim = NULL, ylim = NULL,
                           title = NULL,
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
        sdy_doe <- NULL
    }

    ## find limits: rx is matrix with min in row 1 and max in row 2
    rx <- apply(X_doe, 2, range)
    if(!is.null(Xlim)) rx <- matrix(Xlim,nrow=2,ncol=D)
    rownames(rx) <- c("min", "max")
    drx <- rx["max", ] - rx["min", ]

    ## define X & y labels
    if (is.null(ylab)) ylab <- names(y_doe)
    if (is.null(Xlab)) Xlab <- names(X_doe)

    if (is.null(axis)) {
        axis <- t(utils::combn(D, 2))
    } else {
        axis <- matrix(axis, ncol = 2)
    }

    if (is.null(conf_blend) || length(conf_blend) != length(conf_lev)) {
        conf_blend <- rep(0.5/length(conf_lev), length(conf_lev))
    }

    sectionview3d.function(
        fun = function(x) {
            p = DiceKriging::predict.km(km_model,type=type,newdata=x,checkNames=FALSE)
            list(mean=p$mean, se=p$sd)
        }, vectorized=TRUE,
    dim = D, center = center,axis = axis,npoints = npoints,
    col_surf = col_surf,conf_lev=conf_lev, conf_blend=conf_blend,
    mfrow = mfrow, Xlab = Xlab, ylab = ylab,
    Xlim = rx, ylim=ylim,title = title, add = add, engine3d=engine3d, ...)

    sectionview3d.matrix(X = X_doe, y = y_doe, sdy = sdy_doe,
                       dim = D, center = center, axis = axis,
                       col_points = col_points, conf_lev = conf_lev, conf_blend = conf_blend, bg_blend = bg_blend,
                       mfrow = mfrow,
                       Xlim = rx, ylim=ylim, engine3d=engine3d,
                       add=TRUE)
}


#' @param libKriging_model an object of class \code{"Kriging"}, \code{"NuggetKriging"} or \code{"NoiseKriging"}.
#' @param col_points color of points.
#' @param bg_blend  an optional factor of alpha (color channel) blending used to plot design points outside from this section.
sectionview3d_libKriging <- function(libKriging_model,
                           center = NULL,
                           axis = NULL,
                           npoints = 20,
                           col_points = "red",
                           col_surf = "blue",
                           conf_lev = c(0.95),
                           conf_blend = NULL,
                           bg_blend = 1,
                           mfrow = NULL,
                           Xlab = NULL, ylab = NULL,
                           Xlim = NULL, ylim = NULL,
                           title = NULL,
                           add = FALSE,
                           engine3d = NULL,
                           ...) {
    X_doe <- libKriging_model$X()
    y_doe <- libKriging_model$y()

    D <- ncol(X_doe)
    n <- nrow(X_doe)

    if (inherits(libKriging_model, "Kriging")) {
        sdy_doe <- NULL
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

    if (is.null(conf_blend) || length(conf_blend) != length(conf_lev)) {
        conf_blend <- rep(0.5/length(conf_lev), length(conf_lev))
    }

    sectionview3d.function(fun = function(x) {
            p = rlibkriging::predict(libKriging_model,x,stdev=TRUE)
            list(mean=p$mean, se=p$stdev)
        }, vectorized=TRUE,
        dim = D, center = center,axis = axis,npoints = npoints,
        col_surf = col_surf,conf_lev=conf_lev, conf_blend=conf_blend,
        mfrow = mfrow, Xlab = Xlab, ylab = ylab,
        Xlim = rx, ylim=ylim, title = title, add = add, engine3d=engine3d, ...)

    sectionview3d.matrix(X = X_doe, y = y_doe, sdy = sdy_doe,
                         dim = D, center = center, axis = axis,
                         col_points = col_points, conf_lev = conf_lev, conf_blend = conf_blend, bg_blend = bg_blend,
                         mfrow = mfrow,
                         Xlim = rx, ylim=ylim, engine3d=engine3d,
                         add=TRUE)
}

#' @param Kriging_model an object of class \code{"Kriging"}.
#' @param col_points color of points.
#' @param conf_lev an optional list of confidence interval values to display.
#' @param conf_blend an optional factor of alpha (color channel) blending used to plot confidence intervals.
#' @param bg_blend  an optional factor of alpha (color channel) blending used to plot design points outside from this section.
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
                                   npoints = 20,
                                   col_points = "red",
                                   col_surf = "blue",
                                   conf_lev = c(0.95),
                                   conf_blend = NULL,
                                   bg_blend = 1,
                                   mfrow = NULL,
                                   Xlab = NULL, ylab = NULL,
                                   Xlim = NULL, ylim = NULL,
                                   title = NULL,
                                   add = FALSE,
                                   engine3d = NULL,
                                   ...) {
    sectionview3d_libKriging(Kriging_model,center,axis,npoints,col_points,col_surf,conf_lev,conf_blend,bg_blend,mfrow,Xlab, ylab,Xlim,ylim,title,add,engine3d,...)
}

#' @param NuggetKriging_model an object of class \code{"Kriging"}.
#' @param col_points color of points.
#' @param conf_lev an optional list of confidence interval values to display.
#' @param conf_blend an optional factor of alpha (color channel) blending used to plot confidence intervals.
#' @param bg_blend  an optional factor of alpha (color channel) blending used to plot design points outside from this section.
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
                                npoints = 20,
                                col_points = "red",
                                col_surf = "blue",
                                conf_lev = c(0.95),
                                conf_blend = NULL,
                                bg_blend = 1,
                                mfrow = NULL,
                                Xlab = NULL, ylab = NULL,
                                Xlim = NULL, ylim = NULL,
                                title = NULL,
                                add = FALSE,
                                engine3d = NULL,
                                ...) {
    sectionview3d_libKriging(NuggetKriging_model,center,axis,npoints,col_points,col_surf,conf_lev,conf_blend,bg_blend,mfrow,Xlab, ylab,Xlim,ylim,title,add,engine3d,...)
}

#' @param NoiseKriging_model an object of class \code{"Kriging"}.
#' @param col_points color of points.
#' @param conf_lev an optional list of confidence interval values to display.
#' @param conf_blend an optional factor of alpha (color channel) blending used to plot confidence intervals.
#' @param bg_blend  an optional factor of alpha (color channel) blending used to plot design points outside from this section.
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
                                      npoints = 20,
                                      col_points = "red",
                                      col_surf = "blue",
                                      conf_lev = c(0.95),
                                      conf_blend = NULL,
                                      bg_blend = 1,
                                      mfrow = NULL,
                                      Xlab = NULL, ylab = NULL,
                                      Xlim = NULL, ylim = NULL,
                                      title = NULL,
                                      add = FALSE,
                                      engine3d = NULL,
                                      ...) {
    sectionview3d_libKriging(NoiseKriging_model,center,axis,npoints,col_points,col_surf,conf_lev,conf_blend,bg_blend,mfrow,Xlab, ylab,Xlim,ylim,title,add,engine3d,...)
}

#' @param glm_model an object of class \code{"glm"}.
#' @param col_points color of points.
#' @param conf_lev an optional list of confidence interval values to display.
#' @param conf_blend an optional factor of alpha (color channel) blending used to plot confidence intervals.
#' @param bg_blend  an optional factor of alpha (color channel) blending used to plot design points outside from this section.
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
                           npoints = 20,
                           col_points = "red",
                           col_surf = "blue",
                           conf_lev = c(0.95),
                           conf_blend = NULL,
                           bg_blend = 1,
                           mfrow = NULL,
                           Xlab = NULL, ylab = NULL,
                           Xlim = NULL, ylim = NULL,
                           title = NULL,
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

    ## find limits: rx is matrix with min in row 1 and max in row 2
    rx <- apply(X_doe, 2, range)
    if(!is.null(Xlim)) rx <- matrix(Xlim,nrow=2,ncol=D)
    rownames(rx) <- c("min", "max")
    drx <- rx["max", ] - rx["min", ]

    if (is.null(axis)) {
        axis <- t(utils::combn(D, 2))
    } else {
        axis <- matrix(axis, ncol = 2)
    }

    if (is.null(conf_blend) || length(conf_blend) != length(conf_lev)) {
        conf_blend <- rep(0.5/length(conf_lev), length(conf_lev))
    }

    sectionview3d.function(
        fun = function(x) {
            x = as.data.frame(x)
            colnames(x) <- Xlab
            p = predict.glm(glm_model, newdata=x, se.fit=TRUE)
            list(mean=p$fit, se=p$se.fit)
        }, vectorized=TRUE,
        dim = D, center = center,axis = axis, npoints = npoints,
        col_surf = col_surf,conf_lev=conf_lev, conf_blend=conf_blend,
        mfrow = mfrow, Xlab = Xlab, ylab = ylab,
        Xlim = rx, ylim=ylim, title = title, add = add, engine3d=engine3d, ...)

    sectionview3d.matrix(X = X_doe, y = y_doe, sdy = NULL,
                         dim = D, center = center, axis = axis,
                         col_points = col_points, conf_lev = conf_lev, conf_blend = conf_blend, bg_blend = bg_blend,
                         mfrow = mfrow,
                         Xlim = rx, ylim=ylim, engine3d=engine3d,
                         add=TRUE)
}


#' @param modelFit_model an object returned by DiceEval::modelFit.
#' @param col_points color of points.
#' @param bg_blend  an optional factor of alpha (color channel) blending used to plot design points outside from this section.
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
                            npoints = 20,
                            col_points = "red",
                            col_surf = "blue",
                            bg_blend = 1,
                            mfrow = NULL,
                            Xlab = NULL, ylab = NULL,
                            Xlim = NULL, ylim = NULL,
                            title = NULL,
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

    ## find limits: rx is matrix with min in row 1 and max in row 2
    rx <- apply(X_doe, 2, range)
    if(!is.null(Xlim)) rx <- matrix(Xlim,nrow=2,ncol=D)
    rownames(rx) <- c("min", "max")
    drx <- rx["max", ] - rx["min", ]

    if (is.null(axis)) {
        axis <- t(utils::combn(D, 2))
    } else {
        axis <- matrix(axis, ncol = 2)
    }

    sectionview3d.function(
        fun = function(x) {
            x = as.data.frame(x)
            colnames(x) <- Xlab
            DiceEval::modelPredict(modelFit_model, x)
        }, vectorized=TRUE,
        dim = D, center = center,axis = axis, npoints = npoints,
        col_surf = col_surf,
        mfrow = mfrow, Xlab = Xlab, ylab = ylab,
        Xlim = rx, ylim=ylim, title = title, add = add, engine3d=engine3d, ...)

    sectionview3d.matrix(X = X_doe, y = y_doe, sdy = NULL,
                         dim = D, center = center, axis = axis,
                         col_points = col_points, conf_lev = NULL, conf_blend = NULL, bg_blend = bg_blend,
                         mfrow = mfrow,
                         Xlim = rx, ylim=ylim, engine3d=engine3d,
                         add=TRUE)
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
#' sectionview3d(branin, dim=2, col='black')
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
#' sectionview3d(branin, dim=2, col='red', add=TRUE)
#' }
#'
#' if (requireNamespace("rlibkriging")) { library(rlibkriging)
#' ## model: Kriging
#' model <- rlibkriging::Kriging(X = as.matrix(design.fact), y = as.matrix(y), kernel="matern3_2")
#' sectionview3d(model)
#' sectionview3d(branin, dim=2, col='red', add=TRUE)
#' }
#'
#' ## model: glm
#' model <- glm(y ~ 1+ x1 + x2 + I(x1^2) + I(x2^2) + x1*x2, data=cbind(y,design.fact))
#' sectionview3d(model)
#' sectionview3d(branin, dim=2, col='red', add=TRUE)
#'
#' if (requireNamespace("DiceEval")) { library(DiceEval)
#' ## model: StepLinear
#' model <- modelFit(design.fact, y, type = "StepLinear")
#' sectionview3d(model)
#' sectionview3d(branin, dim=2, col='red', add=TRUE)
#' }
#' }
#'
sectionview3d <- function(...){
    UseMethod("sectionview3d")
}
