#### Excursion sets ####

#' @title Search excursion set of nD function, sampled by a mesh
#' @param f Function to inverse at 'threshold'
#' @param threshold target value to inverse
#' @param sign focus at conservative for above (sign=1) or below (sign=-1) the threshold
#' @param intervals bounds to inverse in, each column contains min and max of each dimension
#' @param mesh.type "unif" or "seq" (default) or "LHS" to preform interval partition
#' @param mesh.sizes number of parts for mesh (duplicate for each dimension if using "seq")
#' @param maxerror_f maximal tolerance on f precision
#' @param ex_filter.tri boolean function to validate a geometry::tri as considered in excursion : 'any' or 'all'
#' @param ... parameters to forward to roots_mesh(...) call
#' @param vectorized boolean: is f already vectorized ? (default: FALSE) or if function: vectorized version of f.
#' @param tol the desired accuracy (convergence tolerance on f arg).
#' @param num_workers number of cores to use for parallelization
#' @importFrom geometry delaunayn
#' @importFrom utils combn
#' @export
#' @examples
#' # mesh_exsets(function(x) x, threshold=.51, sign=1, intervals=rbind(0,1),
#' #   maxerror_f=1E-2,tol=1E-2, num_workers=1) # for faster testing
#' # mesh_exsets(function(x) x, threshold=.50000001, sign=1, intervals=rbind(0,1),
#' #   maxerror_f=1E-2,tol=1E-2, num_workers=1) # for faster testing
#' # mesh_exsets(function(x) sum(x), threshold=.51,sign=1, intervals=cbind(rbind(0,1),rbind(0,1)),
#' #   maxerror_f=1E-2,tol=1E-2, num_workers=1) # for faster testing
#' # mesh_exsets(sin,threshold=0,sign="sup",interval=c(pi/2,5*pi/2),
#' #   maxerror_f=1E-2,tol=1E-2, num_workers=1) # for faster testing
#'
#' if (identical(Sys.getenv("NOT_CRAN"), "true")) { # too long for CRAN on Windows
#'
#'   e = mesh_exsets(function(x) (0.25+x[1])^2+(0.5+x[2])^2 ,
#'                 threshold =0.25,sign=-1, intervals=matrix(c(-1,1,-1,1),nrow=2),
#'                 maxerror_f=1E-2,tol=1E-2, # for faster testing
#'                 num_workers=1)
#'
#'   plot(e$p,xlim=c(-1,1),ylim=c(-1,1));
#'   apply(e$tri,1,function(tri) polygon(e$p[tri,],col=rgb(.4,.4,.4,.4)))
#'   apply(e$frontiers,1,function(front) lines(e$p[front,],col='red'))
#'
#'   if (requireNamespace("rgl")) {
#'     e = mesh_exsets(function(x) (0.5+x[1])^2+(-0.5+x[2])^2+(0.+x[3])^2,
#'                   threshold = .25,sign=-1, mesh.type="unif",
#'                   intervals=matrix(c(-1,1,-1,1,-1,1),nrow=2),
#'                   maxerror_f=1E-2,tol=1E-2, # for faster testing
#'                   num_workers=1)
#'
#'     rgl::plot3d(e$p,xlim=c(-1,1),ylim=c(-1,1),zlim=c(-1,1));
#'     apply(e$tri,1,function(tri)rgl::lines3d(e$p[tri,]))
#'   }
#' }
mesh_exsets = function (f, vectorized = FALSE, threshold, sign, intervals,
      mesh.type = "seq", mesh.sizes = 11, maxerror_f = 1e-09, tol = .Machine$double.eps^0.25,
      ex_filter.tri = all, num_workers=maxWorkers(), ...) {
    # .t0 <<- Sys.time()

    if (sign == "lower" || sign == -1 || sign == "inf" || sign == "<" || isFALSE(sign))
        return(
            mesh_exsets(f = function(...) { -f(...) },
                        vectorized = vectorized, threshold = -threshold, sign = 1,
                        intervals = intervals, mesh.type = mesh.type, mesh.sizes = mesh.sizes,
                        maxerror_f = maxerror_f, tol = tol, ex_filter.tri=ex_filter.tri,
                        num_workers=num_workers, ...))

    if (sign != "upper" && sign != 1 && sign != "sup" && sign != ">" && !isTRUE(sign))
        stop("unknown sign: '", sign, "'")

    if (is.matrix(mesh.type))
        d = ncol(mesh.type)
    else if (is.matrix(intervals))
        d = ncol(intervals)
    else if (is.array(intervals))
        d = 1
    else
        stop("cannot identify dim of mesh")

    if (isTRUE(vectorized) && is.function(f)) {
        f_vec = function(x, ...) f(x, ...)
    } else if (is.function(vectorized)) {
        f_vec = function(x, ...) vectorized(x, ...)
        if (is.null(f)) f = f_vec
    } else if (isFALSE(vectorized) && !is.null(f)) {
        f_vec = Vectorize.function(f, dim = d)
    } else if (is.character(vectorized) && !is.null(f)) { # arbitrary lapply-like function
        f_vec = Vectorize.function(f, dim = d, .lapply = match.fun(vectorized))
    } else
        stop("Cannot decide how to vectorize f")

    # warning(paste0("[mesh_exsets] init: ",Sys.time() - .t0))
    # .t0 <<- Sys.time()

    k0 = 0#-1
    f_0 <- function(...) return(f(...) - (threshold + k0*maxerror_f))
    f_vec_0 <- function(...) return(f_vec(...) - (threshold + k0*maxerror_f))
    r <- roots_mesh(f = f_0, vectorized = f_vec_0, intervals = intervals,
                     mesh.type = mesh.type, mesh.sizes = mesh.sizes, maxerror_f = maxerror_f,
                     tol = tol, num_workers=num_workers, ...)

    # warning(paste0("[mesh_exsets] roots_mesh: ",Sys.time() - .t0))
    # .t0 <<- Sys.time()

    all_points = attr(r, "mesh")$p
    if (!all(is.na(r)))
        all_points = rbind(all_points, r)

    new_mesh <- mesh(intervals, mesh.type = all_points)

    # warning(paste0("[mesh_exsets] mesh: ",Sys.time() - .t0))
    # .t0 <<- Sys.time()

    # identify points belonging to the excursion set
    new_mesh$y = f_vec(new_mesh$p, ...) # re-evaluate f to get exact values (ie. close to frontier are 0.0 +/- maxrror_f)

    # warning(paste0("[mesh_exsets] f_vec: ",Sys.time() - .t0))
    # .t0 <<- Sys.time()

    I = which(apply(new_mesh$tri, 1,
                    function(i) ex_filter.tri(new_mesh$y[i] >= (threshold - 2*maxerror_f))))

    # warning(paste0("[mesh_exsets] I: ",Sys.time() - .t0))
    # .t0 <<- Sys.time()

    # identify edges at the frontier: one point of the tri is outside, the other inside
    d = ncol(new_mesh$p)
    frontiers = matrix(NA, nrow = 0, ncol = d)
    for (i in 1:nrow(new_mesh$tri)) {
        tri = new_mesh$tri[i,]
        outs = tri[ which(new_mesh$y[tri] <  (threshold - 2*maxerror_f))]
        ins =  tri[ which(new_mesh$y[tri] >= (threshold - 2*maxerror_f))]
        if (length(outs) == 1) {
            if (d==1)
                frontiers = rbind(frontiers, ins)
            else
                frontiers = rbind(frontiers, t(combn(ins, d)))
        }
    }
    frontiers = unique(frontiers)

    colnames(new_mesh$p) <- colnames(intervals)

    return(list(p = new_mesh$p, y = new_mesh$y,
                tri = new_mesh$tri[I, ],
                all_tri = new_mesh$tri,
                areas = new_mesh$areas[I],
                all_areas = new_mesh$areas,
                neighbours = new_mesh$neighbours[I],
                frontiers = frontiers))
}

#### Plot meshes ####

#' @title Plot a one dimensional mesh
#' @param mesh 1-dimensional mesh to draw
#' @param y ordinate value where to draw the mesh
#' @param color.nodes color of the mesh nodes
#' @param color.mesh color of the mesh elements
#' @param alpha transparency of the mesh elements & nodes
#' @param ... optional arguments passed to plot function
#' @import graphics
#' @export
#' @examples
#' plot_mesh(mesh_exsets(function(x) x, threshold=.51, sign=1,
#' intervals=rbind(0,1), num_workers=1))
#' plot_mesh(mesh_exsets(function(x) (x-.5)^2, threshold=.1, sign=-1,
#' intervals=rbind(0,1), num_workers=1))
plot_mesh = function(mesh,y=0,color.nodes='black',color.mesh='darkgray',alpha=0.4, ...){
    nodes.rgb=col2rgb(color.nodes)/255
    p = plot(x=mesh$p,y=rep(y,length(mesh$p)),ylab="",col=rgb(nodes.rgb[1,],nodes.rgb[2,],nodes.rgb[3,],alpha),...)
    mesh.rgb=col2rgb(color.mesh)/255
    apply(mesh$tri,1,
          function(tri) {
              lines(mesh$p[tri,],y=rep(0,length(tri)),col=rgb(mesh.rgb[1,],mesh.rgb[2,],mesh.rgb[3,],alpha))
              polygon(x=c(mesh$p[tri,],rev(mesh$p[tri,])),y=c(-1,-1,1,1),border = NA,col=rgb(mesh.rgb[1,],mesh.rgb[2,],mesh.rgb[3,],alpha))
          })
    invisible(p)
}

#' @title Plot a two dimensional mesh
#' @param mesh 2-dimensional mesh to draw
#' @param color.nodes color of the mesh nodes
#' @param color.mesh color of the mesh elements
#' @param alpha transparency of the mesh elements & nodes
#' @param ... optional arguments passed to plot function
#' @import graphics
#' @export
#' @examples
#' plot2d_mesh(mesh_exsets(f = function(x) sin(pi*x[1])*sin(pi*x[2]),
#'                         threshold=0,sign=1, mesh.type="unif",mesh.size=11,
#'                         intervals = matrix(c(1/2,5/2,1/2,5/2),nrow=2),
#'                         num_workers=1))
plot2d_mesh = function(mesh,color.nodes='black',color.mesh='darkgray',alpha=0.4,...){
    nodes.rgb=col2rgb(color.nodes)/255
    p = plot(mesh$p,col=rgb(nodes.rgb[1,],nodes.rgb[2,],nodes.rgb[3,],alpha),...)
    mesh.rgb=col2rgb(color.mesh)/255
    apply(mesh$tri,1,
          function(tri)
              polygon(mesh$p[c(tri,tri[1]),],border = rgb(mesh.rgb[1,],mesh.rgb[2,],mesh.rgb[3,],alpha), col = rgb(mesh.rgb[1,],mesh.rgb[2,],mesh.rgb[3,],alpha)))
    # sapply(1:nrow(mesh$tri),function(i) {xy=colMeans(mesh$p[mesh$tri[i,],]);text(x = xy[1],y=xy[2],paste0(i))})
    # apply(mesh$tri,1,function(tri) points(mesh$p[tri,], col = rgb(0,0,0,1),pch=20))
    invisible(p)
}

#' @title Plot a three dimensional mesh
#' @param mesh 3-dimensional mesh to draw
#' @param engine3d 3d framework to use: 'rgl' if installed or 'scatterplot3d' (default)
#' @param color.nodes color of the mesh nodes
#' @param color.mesh color of the mesh elements
#' @param alpha transparency of the mesh elements & nodes
#' @param ... optional arguments passed to plot function
#' @export
#' @examples
#' if (identical(Sys.getenv("NOT_CRAN"), "true")) { # too long for CRAN on Windows
#'
#'   plot3d_mesh(mesh_exsets(function(x) (0.5+x[1])^2+(-0.5+x[2])^2+(0.+x[3])^2,
#'                           threshold = .25,sign=-1, mesh.type="unif",
#'                           maxerror_f=1E-2,tol=1E-2, # faster display
#'                           intervals=matrix(c(-1,1,-1,1,-1,1),nrow=2),
#'                           num_workers=1),
#'                           engine3d='scatterplot3d')
#'
#'   if (requireNamespace("rgl")) {
#'     plot3d_mesh(mesh_exsets(function(x) (0.5+x[1])^2+(-0.5+x[2])^2+(0.+x[3])^2,
#'                             threshold = .25,sign=-1, mesh.type="unif",
#'                             maxerror_f=1E-2,tol=1E-2, # faster display
#'                             intervals=matrix(c(-1,1,-1,1,-1,1),nrow=2),
#'                             num_workers=1),engine3d='rgl')
#'   }
#' }
plot3d_mesh = function(mesh,engine3d=NULL,color.nodes='black',color.mesh='darkgray',alpha=0.4,...){
    nodes.rgb=col2rgb(color.nodes)/255
    package = .load3d(engine3d)
    if (is.null(package)) return()
    p3d = .plot3d(mesh$p,col=rgb(nodes.rgb[1,],nodes.rgb[2,],nodes.rgb[3,],alpha), package=package,...)
    mesh.rgb=col2rgb(color.mesh)/255
    apply(mesh$tri,1,function(tri) {
        .quads3d(mesh$p[tri,],col=rgb(mesh.rgb[1,],mesh.rgb[2,],mesh.rgb[3,]),alpha=alpha/10, package=package)
        .quads3d(mesh$p[tri,][c(4,3,2,1),],col=rgb(mesh.rgb[1,],mesh.rgb[2,],mesh.rgb[3,]),alpha=alpha/10, package=package)
        # .triangles3d(mesh$p[tri,][-1,],col=color,alpha=0.05, package=package)
        # .triangles3d(mesh$p[tri,][-2,],col=color,alpha=0.05, package=package)
        # .triangles3d(mesh$p[tri,][-3,],col=color,alpha=0.05, package=package)
        # .triangles3d(mesh$p[tri,][-4,],col=color,alpha=0.05, package=package)
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
            } # else warning("Too flat element (",i,"): ",utils::capture.output(print(mesh$p[mesh$tri[i,],, drop = FALSE])))
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

#' @title Checks if a mesh is valid
#' @param x mesh to check
#' @export
#' @return TRUE if mesh is valid
is.mesh = function(x) {
    is.list(x) && all(c("p","tri") %in% names(x))
}


#' @title Compute distance between a point and a mesh
#' @param p point to compute distance from
#' @param mesh mesh to compute distance to
#' @param norm vector of weights for each dimension (default: 1)
#' @return distance between x and mesh
#' @export
#' @examples
#'  x = matrix(0,ncol=2)
#'  m = list(p = matrix(c(0,1,1,0,1,1),ncol=2,byrow=TRUE), tri = matrix(c(1,2,3),nrow=1))
#'  plot2d_mesh(m)
#'  points(x)
#'  min = min_dist.mesh(x,m)
#'  lines(rbind(x,attr(min,"proj")),col='red')
#'
#'  m = mesh_exsets(function(x) (0.25+x[1])^2+(0.5+x[2]/2)^2, vec=FALSE,
#'                  1 ,1, intervals=rbind(cbind(0,0),cbind(1,1)), num_workers=1)
#'  plot2d_mesh(m)
#'  x = matrix(c(0.25,0.25),ncol=2)
#'  points(x)
#'  min = min_dist.mesh(x,m)
#'  lines(rbind(x,attr(min,"proj")),col='red')
min_dist.mesh = function(p,mesh, norm=rep(1,ncol(mesh$p))) {
    dist2_to_p = function(x) sum(((p-x)/norm)^2)
    dists = NULL
    projs = NULL
    dim = ncol(mesh$p)
    for (i in 1:nrow(mesh$tri)) {
        trip = mesh$p[mesh$tri[i,],,drop=F]
        # find min dist bw p and tri, as a weighted barycenter of all points in the tri
        o = optim(rep(1,dim+1), # not optimal: 1 df too much
                  function(x) dist2_to_p((x/sum(x)) %*% trip),
                  method="L-BFGS-B",
                  lower=rep(1e-5,dim+1),upper=rep(1,dim+1)) # 1e-5: avoid 0 for sum(x)!=0, but that also excludes extreme points
        # ...so also compare dist with the extreme points of the tri:
        o$par = o$par/sum(o$par)
        best_points = rbind((o$par/sum(o$par)) %*% trip, trip)
        best_dists = sqrt(apply(best_points,1,dist2_to_p))
        best_i = which.min(best_dists)

        dists = c(dists, best_dists[best_i])
        projs = rbind(projs, best_points[best_i,])
    }
    im = which.min(dists)
    m = dists[im]
    attr(m,"proj") <- projs[im,]
    return(m)
}


#### Build mesh (on hypercube) ####

#' @title Builds a mesh from a design aor set of points
#' @param intervals bounds to inverse in, each column contains min and max (or values) of each dimension
#' @param mesh.type function or "unif" or "seq" (default) or "LHS" to preform interval partition
#' @param mesh.sizes number of parts for mesh (duplicate for each dimension if using "seq")
#' @return delaunay mesh (list(p,tri,...) from geometry)
#' @export
#' @examples
#' mesh = mesh(intervals=matrix(c(0,1,0,1),ncol=2),mesh.type="unif",mesh.sizes=10)
#' plot2d_mesh(mesh)
mesh = function(intervals, mesh.type = "seq", mesh.sizes = 11) {
    # setup bounds & dim
    if (is.matrix(intervals)) {
        if (nrow(intervals)!=2 && ncol(intervals)==2)
            intervals = t(intervals)
        lowers = apply(intervals, 2, min)
        uppers = apply(intervals, 2, max)
        d = ncol(intervals)
    } else if (is.null(intervals) && is.matrix(mesh.type)) {
        lowers = apply(mesh.type, 2, min)
        uppers = apply(mesh.type, 2, max)
        d = ncol(mesh.type)
    } else
        d = 1

    if (length(mesh.sizes) != d)
        mesh.size = rep(mesh.sizes, d)

    if (is.matrix(mesh.type)) {
        ridge.points = mesh.type
    } else if (is.function(mesh.type)) {
        ridge.points = mesh.type(prod(mesh.size), lowers, uppers)
    } else if (mesh.type == "seq") {
        ridge.points = matrix(seq(f = lowers[1], to = uppers[1],
                                  length.out = mesh.size[1]), ncol = 1)
        for (i in 2:d) {
            ridge.points = combn.design(ridge.points, matrix(seq(f = lowers[i], to = uppers[i], length.out = mesh.size[i]), ncol = 1))
        }
    } else if (mesh.type == "unif") {
        ridge.points = matrix(runif(n = d * prod(mesh.size),
                                    min = 0, max = 1), ncol = d)
        ridge.points = matrix(lowers, ncol = d, nrow = nrow(ridge.points),
                              byrow = TRUE) +
            ridge.points * (matrix(uppers, ncol = d, nrow = nrow(ridge.points), byrow = TRUE) -
                                matrix(lowers, ncol = d, nrow = nrow(ridge.points), byrow = TRUE))

    } else if (mesh.type == "LHS") {
        ridge.points = DiceDesign::lhsDesign(prod(mesh.size), dimension = d)$design
        ridge.points = matrix(lowers, ncol = d, nrow = nrow(ridge.points), byrow = TRUE) +
            ridge.points * (matrix(uppers, ncol = d, nrow = nrow(ridge.points), byrow = TRUE) -
                                matrix(lowers, ncol = d, nrow = nrow(ridge.points), byrow = TRUE))

    } else stop("unsupported mesh setup : ", mesh.type)

    # add bounds
    b = rbind(lowers[1], uppers[1])
    if (d>1) for (id in 1:(d - 1)) { # efficient way for factorial design
        b = rbind(cbind(b, lowers[id + 1]),cbind(b, uppers[id + 1]))
    }
    for (i in 1:nrow(b)) if (min_dist(b[i, ,drop=FALSE], ridge.points) > .Machine$double.eps)
        ridge.points = rbind(ridge.points, b[i, ,drop=FALSE])

    return( geometry::delaunayn(ridge.points, output.options = "Fn Fa", options= "Qt Qc Qz QbB Qcc") )
}
