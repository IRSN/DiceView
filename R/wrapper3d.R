#' @importFrom utils installed.packages
load3d = function(package = NULL) {
    if (!is.null(package)) { # prefered package defined
        if (!(package %in% rownames(installed.packages()))) {
            warning(paste0("Package ",package," should be available to use any DiceView::*3d functions."))
            return(NULL)
        } else {
            env3d$package <- package
            return(package)
        }
    } else { # auto select 3d package
        if (!is.null(env3d$package) && (env3d$package %in% rownames(installed.packages()))) {
            return(env3d$package)
        } else {
            packages = c("rgl","scatterplot3d")
            for (package in packages) {
                if (package %in% rownames(installed.packages())) {
                    return(package)
                }
            }
            warning(paste0("Cannot find any installed package for 3D: ",paste0(collapse=" or ",packages)))
            return(NULL)
        }
    }
}

env3d <- new.env()

open3d = function(...) {
    package = load3d()
    if (is.null(package)) {
        stop("No 3D package available.")
    } else if (package=="rgl") {
        rgl::open3d(...)
    } else if (package=="scatterplot3d") {
        return()
    } else stop(paste0("Unsupported 3D package: ",package))
}

#' @examples plot3d(runif(10),runif(10),runif(10))
plot3d = function(x, y=NULL, z=NULL,  col='black', alpha=0.5, add = FALSE, package = load3d(),...) {
    if (is.null(y) & is.null(z)) {y = x[,2]; z = x[,3]; x=x[,1]}
    if (is.null(package)) {
        stop("No 3D package available.")
    } else if (package=="rgl") {
        rgl::plot3d(x=x, y=y, z=z, col=col,...)
    } else if (package=="scatterplot3d") {
        if (!add) {
            p3d = scatterplot3d::scatterplot3d(x=x, y=y, z=z, color=translude(col,alpha),...)
            assign(".p3d",value=p3d,envir=env3d)
        } else {
            p3d = get(".p3d",envir = env3d)
            p3d$points3d(x=x, y=y, z=z, color=translude(col,alpha),...)
        }
        return(p3d)
    } else stop(paste0("Unsupported 3D package: ",package))
}

#' @examples plot3d(runif(10),runif(10),runif(10)); points3d(runif(10),runif(10),runif(10),col='red')
points3d = function(x, y=NULL, z=NULL,  col='black', alpha=0.5, box=FALSE, package = load3d(), ...) {
    if (is.null(y) & is.null(z)) {y = x[,2]; z = x[,3]; x=x[,1]}
    if (is.null(package)) {
        stop("No 3D package available.")
    } else if (package=="rgl") {
        rgl::points3d(x, y , z ,  color=col, alpha,...)
    } else if (package=="scatterplot3d") {
        p3d = get(".p3d",envir = env3d)
        p3d$points3d(x=x, y=y, z=z, col=translude(col,alpha),...)
    } else stop(paste0("Unsupported 3D package: ",package))
}

#' @examples plot3d(runif(10),runif(10),runif(10)); lines3d(runif(10),runif(10),runif(10),col='red')
lines3d = function(x, y=NULL, z=NULL,  col='black' , alpha=0.5, box=FALSE, package = load3d(),...) {
    if (is.null(package)) {
        stop("No 3D package available.")
    } else if (package=="rgl") {
        rgl::lines3d(x, y , z ,  color=col , alpha,...)
    } else if (package=="scatterplot3d") {
        p3d = get(".p3d",envir = env3d)
        p3d$points3d(x=x, y=y, z=z, col=translude(col,alpha),type='l',...)
    } else stop(paste0("Unsupported 3D package: ",package))
}

#' @examples plot3d(runif(10),runif(10),runif(10)); triangles3d(runif(3),runif(3),runif(3),col='red')
triangles3d = function(x, y=NULL, z=NULL, col='black', alpha=0.5, box=FALSE, package = load3d(), ...) {
    if (is.null(y) & is.null(z)) {
        if (is.matrix(x)) {
            y = x[,2]; z = x[,3]; x=x[,1]
        } else {
            y = x[2]; z = x[3]; x=x[1]
        }
    }
    if (is.null(package)) {
        stop("No 3D package available.")
    } else if (package=="rgl") {
        rgl::triangles3d(x ,y , z ,  color=col , alpha,... )
    } else if (package=="scatterplot3d") {
        p3d = get(".p3d",envir = env3d)
        polygon(p3d$xyz.convert(x,y,z),col = translude(col,alpha), border=translude(col,alpha),...)
    } else stop(paste0("Unsupported 3D package: ",package))
}

#' @examples plot3d(runif(10),runif(10),runif(10)); x=runif(4); y=runif(4); z=runif(4); quads3d(x,y,z,col='red'); p=c(4,3,2,1); quads3d(x[p],y[p],z[p],col='red')
quads3d = function(x, y=NULL, z=NULL, col='black', alpha=0.5, box=FALSE, package = load3d(), ...) {
    if (is.null(y) & is.null(z)) {
        if (is.matrix(x)) {
            y = x[,2]; z = x[,3]; x=x[,1]
        } else {
            y = x[2]; z = x[3]; x=x[1]
        }
    }
    if (is.null(package)) {
        stop("No 3D package available.")
    } else if (package=="rgl") {
        rgl::quads3d(x ,y , z ,  color=col , alpha,... )
    } else if (package=="scatterplot3d") {
        p3d = get(".p3d",envir = env3d)
        polygon(p3d$xyz.convert(x,y,z),col = translude(col,alpha), border=translude(col,alpha),...)
    } else stop(paste0("Unsupported 3D package: ",package))
}

surface3d = function(x, y, z, col='black', alpha = 0.5, box=FALSE, package = load3d(), ...) {
    if (is.null(package)) {
        stop("No 3D package available.")
    } else if (package=="rgl") {
        rgl::surface3d(x ,y , z ,  color=col , alpha,... )
    } else if (package=="scatterplot3d") {
        p3d = get(".p3d",envir = env3d)
        # if (nrow(z)!=length(y)) z = t(z)
        # if (ncol(z)!=length(x)) z = matrix(z,nrow=length(y),ncol=length(x))
        # stop(paste0("z should be ",length(y),"x",length(x)," or ",length(x),"x",length(y)))
        for (i in 1:length(x)) {
            p3d$points3d(x=rep(x[i],length(y)), y=y, z=z[ (1:length(y)) * length(x) + (i-length(x))], col=translude(col,alpha),type='l',...)
        }
        for (j in 1:length(y)) {
            p3d$points3d(x=x,y=rep(y[j],length(x)), z=z[ ((j-1)*length(x)+1):(j*length(x)) ], col=translude(col,alpha),type='l',...)
        }
    } else stop(paste0("Unsupported 3D package: ",package))
}

##### Tests #####
# X = matrix(runif(40),ncol=2)
# p = 2
# f <- function(X) apply(X, 1,
#                        function(x)
#                            prod(
#                                sin(2*pi*
#                                        ( x * (seq(0,1,l=1+length(x))[-1])^p )
#                                )))
# Y = f(X)
#
# plot3d(x=X[,1],y=X[,2],z=Y)
#
# X2 = matrix(runif(40),ncol=2)
# Y2 = f(X2)
# points3d(x=X2[,1],y=X2[,2],z=Y2,col='red')
# lines3d(x=X2[,1],y=X2[,2],z=Y2,col='red')
#
# X3 = matrix(runif(6),ncol=2)
# Y3 = f(X3)
# triangles3d(x=X3[,1],y=X3[,2],z=Y3,col='red')
#
# X4_1 = seq(0,1,,21)
# X4_2 = seq(0,1,,31)
# Y4 = f(expand.grid(X4_1,X4_2))
# surface3d(x=X4_1,y=X4_2,z=Y4,col='red')
