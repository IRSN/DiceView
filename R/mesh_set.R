#### Excursion sets ####

#' @title Search excursion set of nD function, sampled by a mesh
#' @param f Function to inverse at 'threshold'
#' @param threshold target value to inverse
#' @param sign focus at conservative for above (sign=1) or below (sign=-1) the threshold
#' @param intervals bounds to inverse in, each column contains min and max of each dimension
#' @param mesh function or "unif" or "seq" (default) to preform interval partition
#' @param mesh.sizes number of parts for mesh (duplicate for each dimension if using "seq")
#' @param maxerror_f maximal tolerance on f precision
#' @param ex_filter.tri boolean function to validate a geometry::tri as considered in excursion : 'any' or 'all'
#' @param ... parameters to forward to mesh_roots(...) call
#' @param vectorized boolean: is f already vectorized ? (default: FALSE) or if function: vectorized version of f.
#' @param tol the desired accuracy (convergence tolerance on f arg).
#' @importFrom geometry delaunayn
#' @export
#' @examples
#' # mesh_exsets(function(x) x, threshold=.51, sign=1, intervals=rbind(0,1),
#' #   maxerror_f=1E-2,tol=1E-2) # for faster testing
#' # mesh_exsets(function(x) x, threshold=.50000001, sign=1, intervals=rbind(0,1),
#' #   maxerror_f=1E-2,tol=1E-2) # for faster testing
#' # mesh_exsets(function(x) sum(x), threshold=.51,sign=1, intervals=cbind(rbind(0,1),rbind(0,1)),
#' #   maxerror_f=1E-2,tol=1E-2) # for faster testing
#' # mesh_exsets(sin,threshold=0,sign="sup",interval=c(pi/2,5*pi/2),
#' #   maxerror_f=1E-2,tol=1E-2) # for faster testing
#'
#' if (identical(Sys.getenv("NOT_CRAN"), "true")) { # too long for CRAN on Windows
#'
#'   e = mesh_exsets(function(x) (0.25+x[1])^2+(0.5+x[2])^2 ,
#'                 threshold =0.25,sign=-1, intervals=matrix(c(-1,1,-1,1),nrow=2),
#'                 maxerror_f=1E-2,tol=1E-2) # for faster testing
#'
#'   plot(e$p,xlim=c(-1,1),ylim=c(-1,1));
#'   apply(e$tri,1,function(tri) polygon(e$p[tri,],col=rgb(.4,.4,.4,.4)))
#'
#'   if (requireNamespace("rgl")) {
#'     e = mesh_exsets(function(x) (0.5+x[1])^2+(-0.5+x[2])^2+(0.+x[3])^2,
#'                   threshold = .25,sign=-1, mesh="unif",
#'                   intervals=matrix(c(-1,1,-1,1,-1,1),nrow=2),
#'                   maxerror_f=1E-2,tol=1E-2) # for faster testing
#'
#'     rgl::plot3d(e$p,xlim=c(-1,1),ylim=c(-1,1),zlim=c(-1,1));
#'     apply(e$tri,1,function(tri)rgl::lines3d(e$p[tri,]))
#'   }
#' }
mesh_exsets = function (f, vectorized = FALSE, threshold, sign, intervals,
          mesh = "seq", mesh.sizes = 11, maxerror_f = 1e-09, tol = .Machine$double.eps^0.25,
          ex_filter.tri = all, ...) {
    if (sign == "lower" || sign == -1 || sign == "inf" || sign == "<" || isFALSE(sign))
        return(
            mesh_exsets(f = function(...) { -f(...) }, vectorized = vectorized, threshold = -threshold, sign = 1,
            intervals = intervals, mesh = mesh, mesh.sizes = mesh.sizes,
            maxerror_f = maxerror_f, tol = tol, ...))
    if (sign != "upper" && sign != 1 && sign != "sup" && sign != ">" && !isTRUE(sign))
        stop("unknown sign: '", sign, "'")

    if (is.matrix(mesh))
        d = ncol(mesh)
    else if (is.matrix(intervals))
        d = ncol(intervals)
    else if (is.array(intervals))
        d = 1
    else
        stop("cannot identify ncol of mesh")

    if (isTRUE(vectorized) && is.function(f))
        f_vec = function(x, ...) f(x, ...)
    else if (is.function(vectorized)) {
        f_vec = function(x, ...) vectorized(x, ...)
        if (is.null(f)) f = f_vec
    } else if (isFALSE(vectorized) && !is.null(f))
        f_vec = Vectorize.function(f, dim = d)
    else
        stop("Cannot decide how to vectorize f")

    f_0 <- function(...) return(f(...) - threshold)
    f_vec_0 <- function(...) return(f_vec(...) - threshold)
    r <- mesh_roots(f = f_0, vectorized = f_vec_0, intervals = intervals,
                    mesh = mesh, mesh.sizes = mesh.sizes, maxerror_f = maxerror_f,
                    tol = tol, ...)

    if (all(is.na(r)))
        all_points = attr(r, "mesh")$p
    else
        all_points = rbind(attr(r, "mesh")$p, r)

    if (maxerror_f != 0) { # try add points on each side of the frontier
        f_inf <- function(...) return(f(...) - (threshold - maxerror_f))
        f_vec_inf <- function(...) return(f_vec(...) - (threshold - maxerror_f))
        r_inf <- mesh_roots(f = f_inf, vectorized = f_vec_inf, intervals = intervals,
                            mesh = mesh, mesh.sizes = mesh.sizes, maxerror_f = maxerror_f,
                            tol = tol, ...)
        if (!all(is.na(r_inf)))
            all_points = rbind(all_points, r_inf)

        f_sup <- function(...) return(f(...) - (threshold + maxerror_f))
        f_vec_sup <- function(...) return(f_vec(...) - (threshold + maxerror_f))
        r_sup <- mesh_roots(f = f_sup, vectorized = f_vec_sup, intervals = intervals,
                            mesh = mesh, mesh.sizes = mesh.sizes, maxerror_f = maxerror_f,
                            tol = tol, ...)
        if (!all(is.na(r_sup)))
            all_points = rbind(all_points, r_sup)
    }

    new_mesh <- geometry::delaunayn(all_points, output.options = TRUE)
    new_mesh$y = f_vec(new_mesh$p, ...)
    I = which(apply(new_mesh$tri, 1,
              function(i) ex_filter.tri(new_mesh$y[i] >= (threshold - 2 * maxerror_f))))
    colnames(new_mesh$p) <- colnames(intervals)
    return(list(p = new_mesh$p, tri = new_mesh$tri[I, ], areas = new_mesh$areas[I],
                neighbours = new_mesh$neighbours[I]))
}

#### Plot meshes ####

#' @title Plot a one dimensional mesh
#' @param mesh 1-dimensional mesh to draw
#' @param y ordinate value where to draw the mesh
#' @param color color of the mesh
#' @param ... optional arguments passed to plot function
#' @import graphics
#' @export
#' @examples
#' plot_mesh(mesh_exsets(function(x) x, threshold=.51, sign=1, intervals=rbind(0,1)))
#' plot_mesh(mesh_exsets(function(x) (x-.5)^2, threshold=.1, sign=-1, intervals=rbind(0,1)))
plot_mesh = function(mesh,y=0,color='black',...){
    col.rgb=col2rgb(color)/255
    plot(x=mesh$p,y=rep(y,length(mesh$p)),ylab="",col=rgb(col.rgb[1,],col.rgb[2,],col.rgb[3,],0.4),...)
    apply(mesh$tri,1,function(tri) lines(mesh$p[tri,],y=rep(y,length(tri)),col=color))
}

#' @title Plot a two dimensional mesh
#' @param mesh 2-dimensional mesh to draw
#' @param color color of the mesh
#' @param ... optional arguments passed to plot function
#' @import graphics
#' @export
#' @examples
#' plot2d_mesh(mesh_exsets(f = function(x) sin(pi*x[1])*sin(pi*x[2]),
#'                         threshold=0,sign=1, mesh="unif",mesh.size=11,
#'                         intervals = matrix(c(1/2,5/2,1/2,5/2),nrow=2)))
plot2d_mesh = function(mesh,color='black',...){
    col.rgb=col2rgb(color)/255
    plot(mesh$p,col=rgb(col.rgb[1,],col.rgb[2,],col.rgb[3,],0.4),...)
    apply(mesh$tri,1,function(tri) polygon(mesh$p[c(tri,tri[1]),],border = color, col = rgb(0,0,0,.2)))
    # sapply(1:nrow(mesh$tri),function(i) {xy=colMeans(mesh$p[mesh$tri[i,],]);text(x = xy[1],y=xy[2],paste0(i))})
    # apply(mesh$tri,1,function(tri) points(mesh$p[tri,], col = rgb(0,0,0,1),pch=20))
}

#' @title Plot a three dimensional mesh
#' @param mesh 3-dimensional mesh to draw
#' @param engine3d 3d framework to use: 'rgl' if installed or 'scatterplot3d' (default)
#' @param color color of the mesh
#' @param ... optional arguments passed to plot function
#' @export
#' @examples
#' if (identical(Sys.getenv("NOT_CRAN"), "true")) { # too long for CRAN on Windows
#'
#'   plot3d_mesh(mesh_exsets(function(x) (0.5+x[1])^2+(-0.5+x[2])^2+(0.+x[3])^2,
#'                           threshold = .25,sign=-1, mesh="unif",
#'                           maxerror_f=1E-2,tol=1E-2, # faster display
#'                           intervals=matrix(c(-1,1,-1,1,-1,1),nrow=2)),
#'                           engine3d='scatterplot3d')
#'
#'   if (requireNamespace("rgl")) {
#'     plot3d_mesh(mesh_exsets(function(x) (0.5+x[1])^2+(-0.5+x[2])^2+(0.+x[3])^2,
#'                             threshold = .25,sign=-1, mesh="unif",
#'                             maxerror_f=1E-2,tol=1E-2, # faster display
#'                             intervals=matrix(c(-1,1,-1,1,-1,1),nrow=2)),engine3d='rgl')
#'   }
#' }
plot3d_mesh = function(mesh,engine3d=NULL,color='black',...){
    col.rgb=col2rgb(color)/255
    package = load3d(engine3d)
    if (is.null(package)) return()
    p3d = plot3d(mesh$p,col=rgb(col.rgb[1,],col.rgb[2,],col.rgb[3,],0.4), package=package,...)
    apply(mesh$tri,1,function(tri) {
        quads3d(mesh$p[tri,],col=color,alpha=0.05, package=package)
        quads3d(mesh$p[tri,][c(4,3,2,1),],col=color,alpha=0.05, package=package)
        # triangles3d(mesh$p[tri,][-1,],col=color,alpha=0.05, package=package)
        # triangles3d(mesh$p[tri,][-2,],col=color,alpha=0.05, package=package)
        # triangles3d(mesh$p[tri,][-3,],col=color,alpha=0.05, package=package)
        # triangles3d(mesh$p[tri,][-4,],col=color,alpha=0.05, package=package)
    }) #rgl::lines3d(mesh$p[t(combn(tri,2)),],col=color))
    invisible(p3d)
}


#### Utility functions ####

#' @title Generalize expand.grid() for multi-columns data. Build all combinations of lines from X1 and X2. Each line may hold multiple columns.
#' @param X1 variable values, possibly with many columns
#' @param X2 variable values, possibly with many columns
#' combn.design(matrix(c(10,20),ncol=1),matrix(c(1,2,3,4,5,6),ncol=2))
#' combn.design(matrix(c(10,20,30,40),ncol=2),matrix(c(1,2,3,4,5,6),ncol=2))
combn.design <- function(X1,X2) {
    n1 = nrow(X1)
    n2 = nrow(X2)
    n = expand.grid(1:n1,1:n2)
    C = matrix(NA,ncol=ncol(X1)+ncol(X2),nrow=n1*n2)
    for (i in 1:(n1*n2)) {
        C[i,1:ncol(X1)] = X1[n[i,1],]
        C[i,ncol(X1)+(1:ncol(X2))] = X2[n[i,2],]
    }
    C
}

#' @title Test if points are in a hull
#' @param x points to test
#' @param p points defining the hull
#' @param h hull itself (built from p if given as NULL (default))
#' @export
#' @importFrom geometry convhulln
#' @importFrom geometry inhulln
#' @examples
#' is_in.p(x=-0.5,p=matrix(c(0,1),ncol=1))
#' is_in.p(x=0.5,p=matrix(c(0,1),ncol=1))
#' is_in.p(x=matrix(-.5,ncol=2,nrow=1),p=matrix(c(0,0,1,1,0,0),ncol=2))
#' is_in.p(x=matrix(.25,ncol=2,nrow=1),p=matrix(c(0,0,1,1,0,0),ncol=2))
#' is_in.p(x=matrix(-.5,ncol=3,nrow=1),p=matrix(c(0,0,0,1,0,0,0,1,0,0,0,1),ncol=3,byrow = TRUE))
#' is_in.p(x=matrix(.25,ncol=3,nrow=1),p=matrix(c(0,0,0,1,0,0,0,1,0,0,0,1),ncol=3,byrow = TRUE))
is_in.p = function(x,p,h=NULL) {
    if (is.null(h)) { # use p points, build h hull
        if (ncol(p)==1) return(x>min(p) && x<max(p))
        if (!is.matrix(x)) x = matrix(x,ncol=ncol(p))
        h = geometry::convhulln(p)
    }
    geometry::inhulln(ch = h,p=x)
}

#' Checks if some point belongs to a given mesh
#' @param x point to check
#' @param mesh mesh identifying the set which X may belong
#' @import stats
#' @export
#' @examples
#' is_in.mesh(-0.5,mesh=geometry::delaunayn(matrix(c(0,1),ncol=1),output.options =TRUE))
#' is_in.mesh(0.5,mesh=geometry::delaunayn(matrix(c(0,1),ncol=1),output.options =TRUE))
#'
#' x =matrix(-.5,ncol=2,nrow=1)
#' is_in.mesh(x,mesh=geometry::delaunayn(matrix(c(0,0,1,1,0,0),ncol=2),output.options =TRUE))
#'
#' x =matrix(.5,ncol=2,nrow=1)
#' is_in.mesh(x,mesh=geometry::delaunayn(matrix(c(0,0,1,1,0,0),ncol=2),output.options =TRUE))
is_in.mesh = function(x,mesh) {
    for (i in 1:nrow(mesh$tri)) {
        # if (mesh$areas[i]>.Machine$double.eps) # otherwise, flat triangle NOT RELIABLE
            if (all(dist(mesh$p[mesh$tri[i,],, drop = FALSE])>.Machine$double.eps)) {# not too close points
                #if (qr(mesh$p[mesh$tri[i,],, drop = FALSE])$rank == ncol(mesh$p)) # check this is not a degenerated (flat) triangle
                isin = FALSE
                try(isin <- is_in.p(x, mesh$p[mesh$tri[i,],, drop = FALSE]),silent = T) # to avoid coplanar errors in qhull
                if (isin) return(TRUE)
            } # else warning("Too flat element (",i,"): ",capture.output(print(mesh$p[mesh$tri[i,],, drop = FALSE])))
        # else warning("Too small element (",i,"): area=",mesh$areas[i])
        }
    return(FALSE)
}

#' @title Checks if some points belong to a given mesh
#' @param X points to check
#' @param mesh mesh identifying the set which X may belong
#' @export
#' @examples
#' X = matrix(runif(100),ncol=2);
#' inside = are_in.mesh(X,mesh=geometry::delaunayn(matrix(c(0,0,1,1,0,0),ncol=2),output.options =TRUE))
#' print(inside)
#' plot(X,col=rgb(1-inside,0,0+inside))
are_in.mesh = function(X,mesh) {
    #apply(X,1,is_in.mesh,mesh)
    #unlist(foreach(i = 1:nrow(X)) %dopar% {is_in.mesh(X[i,],mesh)})
    X.list = lapply(seq_len(nrow(X)), function(i) X[i,])
    array(unlist(parallel::mclapply(X.list,is_in.mesh,mesh)))
}

#' @title Extract points of mesh which belong to the mesh triangulation (may not contain all points)
#' @param mesh mesh (list(p,tri,...) from geometry)
#' @return points coordinates inside the mesh triangulation
#' @export
points_in.mesh = function(mesh) {
    if (is.null(mesh)) return(NULL)
    mesh$p[unique(array(mesh$tri)),, drop = FALSE]
}
#' @title Extract points of mesh which do not belong to the mesh triangulation (may not contain all points)
#' @param mesh (list(p,tri,...) from geometry)
#' @return points coordinates outside the mesh triangulation
#' @export
points_out.mesh = function(mesh) {
    if (is.null(mesh)) return(NULL)
    mesh$p[-unique(array(mesh$tri)),, drop = FALSE]
}
