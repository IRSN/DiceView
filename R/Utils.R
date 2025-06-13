##=========================================================
## Manage the DiceView environment for screen setup
##=========================================================

DiceView.env <- new.env()
# Preliminary setup for center/Xlim/axis/mfrow regarding add=TRUE/FALSE
getView <- function(viewdim, add, center, Xlim, ylim, axis, mfrow, verbose=FALSE) {
    if (!isTRUE(add)) {
        if (is.null(Xlim)) stop("Xlim must be specified.")

        if (is.null(center) && is.matrix(Xlim)) {
            center <- colMeans(Xlim, na.rm = TRUE)
        }

        if (is.null(center)) {
            warning("center not specified, assume 1D function")
            D <- 1
        } else
            D <- length(center)

        Xlim <- matrix(unlist(Xlim),nrow=2,ncol=D) # ensure not list + dup c(0,1) if needed
        if (!isTRUE(ncol(Xlim) == D))
            stop(paste0("Xlim must be a matrix with", D," columns."))
        if (!is.null(center)) # ensure center is included in Xlim
            for (i in 1:D) {
                Xlim[1,i] <- min(Xlim[1,i],center[i])
                Xlim[2,i] <- max(Xlim[2,i],center[i])
            }

        if (is.null(ylim)) ylim=c(NA,NA) # to be updated later

        close.screen( all.screens = TRUE )

        assign(".split.screen.lim",cbind(Xlim,ylim),envir=DiceView.env) # matrix of limits, each col for one dim
        assign(".split.screen.center",center,envir=DiceView.env)

        if (is.null(axis)) {
            axis <- t(utils::combn(D, viewdim))
        } else {
            axis <- matrix(axis, ncol = viewdim)
        }
        if (is.null(mfrow)) {
            nc <- round(sqrt(nrow(axis)))
            nl <- ceiling(nrow(axis)/nc)
            mfrow <- c(nc, nl)
        }

        if (D > viewdim) {
            split.screen(figs = mfrow, erase=TRUE)
        }

        assign(".split.screen.axis",axis,envir=DiceView.env)
        assign(".split.screen.mfrow",mfrow,envir=DiceView.env)
    } else {
        if (!exists(".split.screen.lim",envir=DiceView.env) || !exists(".split.screen.center",envir=DiceView.env))
            stop("Cannot use add=TRUE when no previous sectionview() was called.")

        # re-use global settings for limits of this screen
        Xlim.ignored = !is.null(Xlim)
        Xlim = get(x=".split.screen.lim",envir=DiceView.env)
        D <- ncol(Xlim)-1
        ylim = Xlim[,(D+1)] # get ylim
        Xlim = Xlim[,-(D+1),drop=FALSE] # remove ylim
        if (Xlim.ignored) warning(paste0("Xlim ignored when add=TRUE, replaced by: ",paste0(Xlim, collapse = ",")))

        center = get(x=".split.screen.center",envir=DiceView.env)
        axis = get(x=".split.screen.axis",envir=DiceView.env)
        mfrow = get(x=".split.screen.mfrow",envir=DiceView.env)
    }

    if (verbose) {
        cat("D: ", D, "\n")
        cat("center: ", paste0(collapse=",",center), "\n")
        cat("Xlim: ", paste0(collapse=",",Xlim), "\n")
        cat("ylim: ", paste0(collapse=",",ylim), "\n")
        cat("axis: ", paste0(collapse=",",axis), "\n")
        cat("mfrow: ", paste0(collapse=",",mfrow), "\n")
    }

    return(list(D=D, center=center, Xlim=Xlim, ylim=ylim, axis=axis, mfrow=mfrow))
}

# Force setup of some DiceView environment
setView_ylim = function(ylim) {
    if (exists(".split.screen.lim",envir=DiceView.env)) {
        Xlim = get(x=".split.screen.lim",envir=DiceView.env)
        D <- ncol(Xlim)-1
        Xlim = Xlim[,-(D+1)] # remove ylim
        assign(".split.screen.lim",cbind(Xlim,ylim),envir=DiceView.env)

        # cat("cbind(Xlim,ylim): ", paste0(collapse=",",get(x=".split.screen.lim",envir=DiceView.env), "\n"))
    } else {
        stop("Cannot use setView() when no previous DieceView env setup.")
    }
}


##=========================================================
## make colors semi-transparent
##
## alpha = 0 OPAQUE
## alpha = 1 FULLY TRANSPARENT
##
##=========================================================

#' @import grDevices
translude <- function(col, alpha = 0.6) {

    alpha <- rep(alpha, length.out = length(col))
    rgb <- as.matrix(col2rgb(col)/255)
    colors2 <- rgb(red = rgb["red", ],
                   green = rgb["green", ],
                   blue = rgb["blue", ],
                   alpha = alpha)


}

# check integer
is.wholenumber <- function(x, tol = .Machine$double.eps^0.5)  abs(x - round(x)) < tol

##========================================================
## level color: one base color incremented in hsv to provide a palette
##
##========================================================

#' @import grDevices
col.levels <- function(color, levels, fill=FALSE){
    if (length(levels)!=1) # if nlevels is in fact levels
        nlevels <- length(levels)
    else nlevels <- levels
    if (fill) nlevels <- nlevels - 1

    col.rgb=col2rgb(color, alpha = TRUE)
    col.hsv=rgb2hsv(r=col.rgb[1],g=col.rgb[2],b=col.rgb[3])
    col = hsv(h=col.hsv[1],s=seq(f=0,t=col.hsv[2],l=nlevels),v=col.hsv[3],alpha=col.rgb[4]/255)
    return(col)
}

##========================================================
## level colors: two base colors incermented in hsv to provide a palette
##
##========================================================

#' @import grDevices
cols.levels <- function(color1,color2,levels, fill=FALSE) {
    if (length(levels)!=1) # if nlevels is in fact levels
        nlevels <- length(levels)
    else nlevels <- levels
    if (fill) nlevels <- nlevels - 1

    col1.rgb=col2rgb(color1, alpha = TRUE)
    col2.rgb=col2rgb(color2, alpha = TRUE)
    col1.hsv=rgb2hsv(r=col1.rgb[1],g=col1.rgb[2],b=col1.rgb[3])
    col2.hsv=rgb2hsv(r=col2.rgb[1],g=col2.rgb[2],b=col2.rgb[3])
    col = hsv(h=seq(f=col1.hsv[1],t=col2.hsv[1],l=nlevels),
              s=seq(f=col1.hsv[2],t=col2.hsv[2],l=nlevels),
              v=seq(f=col1.hsv[3],t=col2.hsv[3],l=nlevels),
              alpha=seq(f=col1.rgb[4],t=col2.rgb[4],l=nlevels)/255)
    return(col)
}

##========================================================
## fade color: one color and several fading values
## between 0 and 1
##
## alpha ~ 0 nearly equal to the input 'color'
## alpha ~ 1 nearly invisible grayed/transparent
##
## The returned value is vector. It is build
## from a matrix with four rows as
## those produced by col2rgb(x, alpha = TRUE)
##
##========================================================

# be careful that fade(.., alpha=0) means total fading, while fade(.., alpha=1) means no fading
#' @import grDevices
fade <- function(color = "red",
                 alpha =  seq(from = 0, to = 1, length.out = 5),
                 plot = FALSE) {

    if (any(alpha < 0) || any(alpha > 1)) stop("'alpha' values must be >=0 and <= 1")
    if (length(color) > 1) stop("'color' must be of length 1")

    ## a matrix with  1 col
    rgbcol <- col2rgb(color)/255
    mat <- matrix(1-alpha, nrow = 3, ncol = length(alpha), byrow = TRUE)

    mat <- mat + rgbcol %*% alpha

    colors2 <- rgb(red = mat[1, ],
                   green = mat[2, ],
                   blue = mat[3, ],
                   alpha = alpha)

    if (plot) {
        x <- seq(from = 0, to = 1, length.out = length(alpha))
        plot.new( )
        for ( i in 1:(length(alpha)) ){
            rect(xleft = x[i],
                 xright = x[i+1],
                 ybottom = 0,
                 ytop =  1,
                 border = NA,
                 col = colors2[i])
        }

    }

    colors2

}

fades <- function(colors = c('red','blue'),
                  alpha =  0.5)
    Vectorize(function(col)fade(col,alpha = alpha,plot=FALSE))(colors)

##========================================================
## try to find a good formatted value for a numeric vector
## x using a vector of diff range drx
##
## For instance, if drx is 1000, no decimal or very few
## decimals should be used.
##
##========================================================

tryFormat <- function(x, drx) {
    d <- length(x)
    ldx <- log(drx, base = 10)
    ff <- rep(1, d)
    fd <- rep(1, d)
    ff[ldx > 0] <- ceiling(ldx[ldx > 0])
    fd[ldx < 0] <- ceiling(-ldx[ldx < 0]) + 2
    ff <- ff + fd +1

        formats <- paste("%", ff, ".", fd, "f", sep = "")
    fx = format(x)
    try(fx <- sprintf(formats, x))
    fx
}

#' This is a simple copy of the Branin-Hoo 2-dimensional test function, as provided in DiceKriging package.
#' The Branin-Hoo function is defined here over [0,1] x [0,1], instead of [-5,0] x [10,15] as usual.
#' It has 3 global minima : x1 = c(0.9616520, 0.15); x2 = c(0.1238946, 0.8166644); x3 = c(0.5427730, 0.15)
#'
#' @param x a 2-dimensional vector specifying the location where the function is to be evaluated.
#'
#' @return A real number equal to the Branin-Hoo function values at x
#' @export
branin <- function(x) {
    x1 <- x[1] * 15 - 5
    x2 <- x[2] * 15
    (x2 - 5/(4 * pi^2) * (x1^2) + 5/pi * x1 - 6)^2 + 10 * (1 - 1/(8 * pi)) * cos(x1) + 10
}

maxWorkers <- function() {
    chk <- Sys.getenv("_R_CHECK_LIMIT_CORES_", "")
    if (nzchar(chk) && chk == "TRUE") {
        return(2)
    } else {
        if (Sys.info()[["sysname"]] == "Windows") {
            return(1) #parallel::detectCores() - 1)
        } else {
            return(parallel::detectCores())
        }
    }
}
