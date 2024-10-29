DiceView.env <- new.env()


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

##========================================================
## level color: one base color incermented in hsv to provide a palette
##
##========================================================

#' @import grDevices
col.levels <- function(color,nlevels){
    if (length(nlevels)!=1) # if nlevels is in fact levels
        nlevels <- length(nlevels)-1
    col.rgb=col2rgb(color)
    col.hsv=rgb2hsv(r=col.rgb[1],g=col.rgb[2],b=col.rgb[3])
    col = hsv(h=col.hsv[1],s=seq(f=0,t=col.hsv[2],l=nlevels),v=col.hsv[3])
    return(col)
}

##========================================================
## level colors: two base colors incermented in hsv to provide a palette
##
##========================================================

#' @import grDevices
cols.levels <- function(color1,color2,nlevels) {
    col1.rgb=col2rgb(color1)
    col2.rgb=col2rgb(color2)
    col1.hsv=rgb2hsv(r=col1.rgb[1],g=col1.rgb[2],b=col1.rgb[3])
    col2.hsv=rgb2hsv(r=col2.rgb[1],g=col2.rgb[2],b=col2.rgb[3])
    col = hsv(h=seq(f=col1.hsv[1],t=col2.hsv[1],l=nlevels),
              s=seq(f=col1.hsv[2],t=col2.hsv[2],l=nlevels),
              v=seq(f=col1.hsv[3],t=col2.hsv[3],l=nlevels))
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
