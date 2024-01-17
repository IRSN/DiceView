#### Generalization of root finding ####

#' @title One Dimensional Root (Zero) Finding
#' @description Search one root with given precision (on y). Iterate over uniroot as long as necessary.
#' @param f the function for which the root is sought.
#' @param lower the lower end point of the interval to be searched.
#' @param upper the upper end point of the interval to be searched.
#' @param maxerror_f the maximum error on f evaluation (iterates over uniroot to converge).
#' @param f_lower the same as f(lower).
#' @param f_upper the same as f(upper).
#' @param tol the desired accuracy (convergence tolerance on f arg).
#' @param convexity the learned convexity factor of the function, used to reduce the boundaries for uniroot.
#' @param rec counter of recursive level.
#' @param max.rec maximal number of recursive level before failure (stop).
#' @param ... additional named or unnamed arguments to be passed to f.
#' @import stats
#' @export
#' @author Yann Richet, IRSN
#' @examples
#' f=function(x) {cat("f");1-exp(x)}; f(root(f,lower=-1,upper=2))
#' f=function(x) {cat("f");exp(x)-1}; f(root(f,lower=-1,upper=2))
#'
#' .f = function(x) 1-exp(1*x)
#' f=function(x) {cat("f");y=.f(x);points(x,y,pch=20,col=rgb(0,0,0,.2));y}
#' plot(.f,xlim=c(-1,2)); f(root(f,lower=-1,upper=2))
#'
#' .f = function(x) exp(10*x)-1
#' f=function(x) {cat("f");y=.f(x);points(x,y,pch=20);y}
#' plot(.f,xlim=c(-1,2)); f(root(f,lower=-1,upper=2))
#'
#' .f = function(x) exp(100*x)-1
#' f=function(x) {cat("f");y=.f(x);points(x,y,pch=20);y}
#' plot(.f,xlim=c(-1,2)); f(root(f,lower=-1,upper=2))
#'
#' f=function(x) {cat("f");exp(100*x)-1}; f(root(f,lower=-1,upper=2))
#'
#' \dontrun{
#'
#'   # Quite hard functions to find roots
#'
#'   ## Increasing function
#'   ## convex
#'   n.f=0
#'   .f = function(x) exp(10*x)-1
#'   f=function(x) {n.f<<-n.f+1;y=.f(x);points(x,y,pch=20);y}
#'   plot(.f,xlim=c(-.1,.2)); f(root(f,lower=-1,upper=2))
#'   print(n.f)
#'   ## non-convex
#'   n.f=0
#'   .f = function(x) 1-exp(-10*x)
#'   f=function(x) {n.f<<-n.f+1;y=.f(x);points(x,y,pch=20);y}
#'   plot(.f,xlim=c(-.1,.2)); f(root(f,lower=-1,upper=2))
#'   print(n.f)
#'
#'   # ## Decreasing function
#'   # ## non-convex
#'   n.f=0
#'   .f = function(x) 1-exp(10*x)
#'   f=function(x) {n.f<<-n.f+1;y=.f(x);points(x,y,pch=20,col=rgb(0,0,0,.2));y}
#'   plot(.f,xlim=c(-.1,.2)); f(root(f,lower=-1,upper=2))
#'   print(n.f)
#'   # ## convex
#'   n.f=0
#'   .f = function(x) exp(-10*x)-1
#'   f=function(x) {n.f<<-n.f+1;y=.f(x);points(x,y,pch=20,col=rgb(0,0,0,.2));y}
#'   plot(.f,xlim=c(-.1,.2)); f(root(f,lower=-1,upper=2))
#'   print(n.f)
#' }
root <- function(f, lower, upper, maxerror_f = 1e-07,
                 f_lower = f(lower,  ...), f_upper = f(upper, ...),
                 tol = .Machine$double.eps^0.25,
                 convexity = FALSE, rec=0, max.rec=NA, ...) {
    # if (rec>50) warning(paste0("Many recursions:\n"," * lower=",lower,"\n"," * upper=",upper,"\n"," * f_lower=",f_lower,"\n"," * f_upper=",f_upper,"\n"," * convexity=",convexity,"\n"," * maxerror_f=",maxerror_f,"\n"," * tol=",tol,"\n"," * rec=",rec,"\n"))
    if (isTRUE(rec>max.rec)) {
      # x=seq(lower,upper,,11)
      # y=Vectorize(f)(x)
      # cat(paste0(apply(cbind(x,y),1,function(x) paste0(x,collapse=" -> ")),collapse="\n"))
      stop(paste0("Too many recursions:\n"," * lower=",lower,"\n"," * upper=",upper,"\n"," * f_lower=",f_lower,"\n"," * f_upper=",f_upper,"\n"," * convexity=",convexity,"\n"," * maxerror_f=",maxerror_f,"\n"," * tol=",tol,"\n"," * rec=",rec,"\n"))
    }

    tol = max(tol,.Machine$double.eps^0.25) # ensure suitable tol for later uniroot
    if (upper < lower) {
      lower.old = lower
      upper.old = upper
      f_lower.old = f_lower
      f_upper.old = f_upper
    return(root(f = f, lower = upper.old, upper = lower.old, maxerror_f = maxerror_f,
                #f_lower = f_upper.old, f_upper = f_lower.old,
                tol = tol, convexity = convexity, rec=rec+1, ...))
    }
    r = NULL
    try(r <- uniroot(f = f, lower = lower, upper = upper,
                     f.lower = f_lower, f.upper = f_upper,
                     tol = tol, ...), silent = FALSE)
    if (is.null(r)) {
        warning(paste0("No root found in [", lower, ",", upper, "] -> [", f_lower, ",", f_upper, "]"))
        return(NULL)
    }

    r_root = r$root
    f_root = f(r_root, ...)
    err = abs(f_root)/maxerror_f

    # print(paste0("[",lower,",",r_root,",",upper,"] -> ","[",f_lower,",",f_root,",",f_upper,"]"))

    if (!is.numeric(err)) stop(paste0("Error in root at ",r_root,": not numeric abs(", f_root,")/", maxerror_f, " = ", err))
    if (err > 1) {
      if (f_lower * f_root < 0) {

        x0 = r_root - (r_root - lower) * min(1-tol,max(tol,(f_root/(f_root - f_lower)) ^exp(convexity)))
        f_x0 = f(x0, ...)

        if (isTRUE(f_x0 * f_root < 0)) {
          return(root(f, lower = x0, upper = r_root, maxerror_f = maxerror_f,
                      f_lower = f_x0, f_upper = f_root,
                      convexity = if (!is.numeric(convexity)) convexity else convexity - log((f_x0 - f_lower)/(f_root - f_lower)),
                      tol = tol, rec=rec+1, max.rec=max.rec, ...))
        } else if (isFALSE(f_x0 * f_root < 0)) {
          return(root(f, lower = lower, upper = x0, maxerror_f = maxerror_f,
                      f_lower = f_lower, f_upper = f_x0,
                      convexity = if (!is.numeric(convexity)) convexity else convexity + log((f_x0 - f_lower)/(f_root - f_lower)),
                      tol = tol, rec=rec+1, max.rec=max.rec, ...))
        } else stop(paste0("Error in root at x0=",x0,", f(x0)=",f_x0," root=",r_root," f(root)=",f_root))

      } else if (f_upper * f_root < 0) {

        x0 = r_root + (upper - r_root) * min(1-tol,max(tol,(-f_root/(f_upper - f_root)) ^exp(convexity)))
        f_x0 = f(x0, ...)

        if (isTRUE(f_x0 * f_root < 0)) {
          return(root(f, lower = r_root, upper = x0, maxerror_f = maxerror_f,
                      f_lower = f_root, f_upper = f_x0,
                      convexity = if (!is.numeric(convexity)) convexity else convexity + log((f_upper - f_x0)/(f_upper - f_root)),
                      tol = tol, rec=rec+1, max.rec=max.rec, ...))
        } else if (isFALSE(f_x0 * f_root < 0)) {
          return(root(f, lower = x0, upper = upper, maxerror_f = maxerror_f,
                      f_lower = f_x0, f_upper = f_upper,
                      convexity = if (!is.numeric(convexity)) convexity else convexity - log((f_upper - f_x0)/(f_upper - f_root)),
                      tol = tol, rec=rec+1, max.rec=max.rec, ...))
        } else stop(paste0("Error in root at x0=",x0,", f(x0)=",f_x0," root=",r_root," f(root)=",f_root))

    } else stop(paste0("Error in root with lower=",lower," r_root=",r_root,", upper=",upper,", f_lower=",f_lower," f_root=",f_root,", f_upper=",f_upper))
  } else r$root
}

#' @title One Dimensional Multiple Roots (Zero) Finding
#' @description Search multiple roots of 1D function, sampled/splitted by a (1D) mesh
#' @param f Function to find roots
#' @param vectorized boolean: is f already vectorized ? (default: FALSE) or if function: vectorized version of f.
#' @param interval bounds to inverse in
#' @param split.size number of parts to perform uniroot inside
#' @param split function or "unif" or "seq" (default) to preform interval partition
#' @param maxerror_f the maximum error on f evaluation (iterates over uniroot to converge).
#' @param tol the desired accuracy (convergence tolerance on f arg).
#' @param .lapply control the loop/vectorization over different roots (defaults to multicore apply).
#' @param ... additional named or unnamed arguments to be passed to f.
#' @return array of x, so f(x)=target
#' @import stats
#' @export
#' @examples
#' roots(sin,interval=c(pi/2,5*pi/2))
#' roots(sin,interval=c(pi/2,1.5*pi/2))
#'
#' f=function(x)exp(x)-1;
#' f(roots(f,interval=c(-1,2)))
#'
#' f=function(x)exp(1000*x)-1;
#' f(roots(f,interval=c(-1,2)))
roots = function (f, vectorized=FALSE, interval, maxerror_f = 1e-07, split = "seq", split.size = 11,
          tol = .Machine$double.eps^0.25, .lapply = parallel::mclapply, ...) {
    lower = min(interval)
    upper = max(interval)

    if (is.function(split))
        intervals = split(min = lower, max = upper, n = split.size)
    else if (is.numeric(split))
        intervals = split
    else if (split == "seq")
        intervals = seq(from = lower, to = upper, length.out = split.size)
    else if (split == "unif")
        intervals = runif(min = lower, max = upper, n = split.size)
    else stop("unsupported mesh: function(min,max,n)/array/'seq'/'fun': type is ",
              typeof(split), " : ", paste0(split, collapse = ";", sep = ","))

    intervals <- sort(unique(intervals))

    if (isTRUE(vectorized))
        f_vec = function(x, ...) f(x, ...)
    else if (is.function(vectorized))
        f_vec = function(x, ...) vectorized(x, ...)
    else f_vec = Vectorize.function(f, dim = 1)

    f_intervals = f_vec(intervals)

    I.list <- lapply(seq_len(length(intervals) - 1),
                     function(i) if (f_intervals[i] * f_intervals[i + 1] < 0)
                                 c(intervals[i], intervals[i + 1], f_intervals[i] , f_intervals[i + 1])
                                 else NULL)

    I.list <- Filter(Negate(is.null), I.list)
    I.roots = .lapply(I.list, function(I) {
        r = NULL
        try({
            r = root(f, lower = I[1], upper = I[2], f_lower = I[3], f_upper = I[4],
                     maxerror_f = maxerror_f, tol = tol, ...)
        }, silent = F)
        r
    })
    I.roots = unlist(Filter(Negate(is.null), I.roots))

    if (length(I.roots) > 0)
        r = matrix(unlist(I.roots), ncol = 1)
    else
        r = NA

    attr(r, "mesh") <- list(p = matrix(intervals, ncol = 1))
    return(r)
}

#' @title Minimal distance between one point to many points
#' @param x one point
#' @param X matrix of points (same number of columns than x)
#' @param norm normalization vecor of distance (same number of columns than x)
#' @return minimal distance
#' @export
#' @examples
#' min_dist(runif(3),matrix(runif(30),ncol=3))
min_dist <- function (x, X, norm=rep(1,ncol(X))){
    return(min(sqrt(rowSums(((X - matrix(x, nrow = nrow(X), ncol = ncol(X), byrow = TRUE))/norm)^2))))
}

#' @title Multi Dimensional Multiple Roots (Zero) Finding, sampled by a mesh
#' @param f Function (one or more dimensions) to find roots of
#' @param vectorized boolean: is f already vectorized ? (default: FALSE) or if function: vectorized version of f.
#' @param intervals bounds to inverse in, each column contains min and max of each dimension
#' @param mesh.type function or "unif" or "seq" (default) to preform interval partition
#' @param mesh.sizes number of parts for mesh (duplicate for each dimension if using "seq")
#' @param vectorized is f already vectorized ? (default: no)
#' @param maxerror_f the maximum error on f evaluation (iterates over uniroot to converge).
#' @param tol the desired accuracy (convergence tolerance on f arg).
#' @param ... Other args for f
#' @import stats
#' @importFrom DiceDesign lhsDesign
#' @importFrom parallel mclapply
#' @export
#' @return matrix of x, so f(x)=0
#' @examples
#' roots_mesh(function(x) x-.51, intervals=rbind(0,1))
#' roots_mesh(function(x) sum(x)-.51, intervals=cbind(rbind(0,1),rbind(0,1)))
#' roots_mesh(sin,intervals=c(pi/2,5*pi/2))
#' roots_mesh(f = function(x) sin(pi*x[1])*sin(pi*x[2]),
#'            intervals = matrix(c(1/2,5/2,1/2,5/2),nrow=2))
#'
#' r = roots_mesh(f = function(x) (0.25+x[1])^2+(0.5+x[2])^2 - .25,
#' intervals=matrix(c(-1,1,-1,1),nrow=2), mesh.size=5)
#' plot(r,xlim=c(-1,1),ylim=c(-1,1))
#'
#' r = roots_mesh(function(x) (0.5+x[1])^2+(-0.5+x[2])^2+(0.+x[3])^2 - .5,
#'                mesh.sizes = 11,
#'                intervals=matrix(c(-1,1,-1,1,-1,1),nrow=2))
#' scatterplot3d::scatterplot3d(r,xlim=c(-1,1),ylim=c(-1,1),zlim=c(-1,1))
#'
#' roots_mesh(function(x)exp(x)-1,intervals=c(-1,2))
#' roots_mesh(function(x)exp(1000*x)-1,intervals=c(-1,2))
roots_mesh = function (f, vectorized = FALSE, intervals, mesh.type = "seq", mesh.sizes = 11,
                       maxerror_f = 1e-07, tol = .Machine$double.eps^0.25, ...) {
  # if (is.matrix(intervals)) {
  #   lowers = apply(intervals, 2, min)
  #   uppers = apply(intervals, 2, max)
  #   d = ncol(intervals)
  # } else if (is.null(intervals) && is.matrix(mesh)) {
  #   lowers = apply(mesh, 2, min)
  #   uppers = apply(mesh, 2, max)
  #   d = ncol(mesh)
  # } else
  #   d = 1

  if (is.matrix(intervals))
    d = ncol(intervals)
  else if (is.null(intervals) && is.matrix(mesh.type))
    d = ncol(mesh)
  else
    d = 1

  if (d == 1)
    return(roots(f, vectorized = vectorized, interval = intervals, split = mesh.type, split.size = mesh.sizes,
                 maxerror_f = maxerror_f, tol = tol, ...))

  # if (length(mesh.sizes) != d)
  #   mesh.size = rep(mesh.sizes, d)
  # if (is.matrix(mesh)) {
  #   ridge.points = mesh
  # } else if (is.function(mesh)) {
  #   ridge.points = mesh(prod(mesh.size), lowers, uppers)
  # } else if (mesh == "seq") {
  #   ridge.points = matrix(seq(f = lowers[1], to = uppers[1],
  #                             length.out = mesh.size[1]), ncol = 1)
  #   for (i in 2:d) {
  #     ridge.points = combn.design(ridge.points, matrix(seq(f = lowers[i], to = uppers[i], length.out = mesh.size[i]), ncol = 1))
  #   }
  # } else if (mesh == "unif") {
  #   ridge.points = matrix(runif(n = d * prod(mesh.size),
  #                               min = 0, max = 1), ncol = d)
  #   ridge.points = matrix(lowers, ncol = d, nrow = nrow(ridge.points),
  #                         byrow = TRUE) +
  #     ridge.points * (matrix(uppers, ncol = d, nrow = nrow(ridge.points), byrow = TRUE) -
  #                       matrix(lowers, ncol = d, nrow = nrow(ridge.points), byrow = TRUE))
  #
  # } else if (mesh == "LHS") {
  #   ridge.points = DiceDesign::lhsDesign(prod(mesh.size), dimension = d)$design
  #   ridge.points = matrix(lowers, ncol = d, nrow = nrow(ridge.points), byrow = TRUE) +
  #     ridge.points * (matrix(uppers, ncol = d, nrow = nrow(ridge.points), byrow = TRUE) -
  #                       matrix(lowers, ncol = d, nrow = nrow(ridge.points), byrow = TRUE))
  #
  # } else stop("unsupported mesh : ", mesh)
  #
  # b = c(lowers[1], uppers[1])
  # for (id in 1:(d - 1)) {
  #   b = rbind(cbind(b, lowers[id + 1]), cbind(b, uppers[id + 1]))
  # }
  # for (i in 1:nrow(b)) if (min_dist(b[i, ], ridge.points) > .Machine$double.eps)
  #   ridge.points = rbind(ridge.points, b[i, ])
  #
  # simplexes <- geometry::delaunayn(ridge.points, output.options = TRUE)

  simplexes = mesh(intervals, mesh.type, mesh.sizes)
  ridge.points = simplexes$p

  if (isTRUE(vectorized) && is.function(f))
    f_vec = function(x, ...) f(x, ...)
  else if (is.function(vectorized)) {
    f_vec = function(x, ...) vectorized(x, ...)
    if (is.null(f)) f = f_vec
  } else if (isFALSE(vectorized) && !is.null(f))
    f_vec = Vectorize.function(f, dim = ncol(ridge.points))
  else
    stop("Cannot decide how to vectorize f")

  ridge.y = f_vec(ridge.points, ...)

  if (requireNamespace("arrangements"))
    tcombn2 = function(x) arrangements::combinations(x, 2)
  else tcombn2 = function(x) t(utils::combn(x, 2))

  ridges = NULL
  for (i in 1:nrow(simplexes$tri)) {
    more_ridges = tcombn2(simplexes$tri[i, ])
    for (j in 1:nrow(more_ridges))
      if (ridge.y[more_ridges[j,1]] * ridge.y[more_ridges[j, 2]] < 0)
        ridges = rbind(ridges, more_ridges[j, ])
  }

  if (is.null(ridges)) {
    r = NA
    if (!is.null(ridge.points)) {
      attr(r, "mesh") <- simplexes
    }
    return(r)
  }

  ridges = unique(as.matrix(ridges))

  # r = Apply.function(.lapply = lapply,X = 1:length(ridges), MARGIN = 1, FUN = function(ridge_i) {
  #     X = ridge.points[ridge_i, , drop = FALSE]
  #     y = ridge.y[ridge_i]
  #     if (isFALSE((all(y > 0) || all(y < 0)))) {
  #         f.r = function(alpha, ...) {
  #           fa=as.numeric(f(alpha * X[1,] + (1 - alpha) * X[2, ], ...));
  #           # print(paste0("fa=",fa));
  #           fa}
  #         alpha_i = root(f.r, lower = 0, upper = 1, maxerror_f = maxerror_f,
  #                        tol = tol, f_lower = y[2], f_upper = y[1], ...)
  #         # print(paste0("a=",alpha_i))
  #         return(alpha_i * X[1, ,drop=FALSE] + (1 - alpha_i) * X[2, ,drop=FALSE])
  #     }
  #     else return(NULL)
  # }, .combine = rbind)

  r = do.call(rbind,parallel::mclapply(X = 1:nrow(ridges), FUN = function(ridge_i,...) {
    X = ridge.points[ridges[ridge_i,], , drop = FALSE]
    y = ridge.y[ridges[ridge_i,]]
    if (isFALSE((all(y > 0) || all(y < 0)))) {
      f_r = function(alpha, ...) {
        as.numeric(f(alpha * X[1, ] + (1 - alpha) * X[2, ], ...))} # assumes one only root (inside current mesh element)
      alpha_i = 0 # default value if root finding fails after:
      try({alpha_i <- root(f=f_r, lower = 0, upper = 1, maxerror_f = maxerror_f,
                           tol = tol, f_lower = y[2], f_upper = y[1], max.rec=10, ...)})
      return(alpha_i * X[1, ,drop=FALSE] + (1 - alpha_i) * X[2, ,drop=FALSE])
    } else return(NULL)
  }))

  if (is.null(r) || length(r) == 0)
    r = NA
  else {
    colnames(r) <- colnames(intervals)
    rownames(r) <- NULL
  }
  if (!is.null(ridge.points)) {
    attr(r, "mesh") <- simplexes
  }

  return(r)
}

#' @title Mesh level set of function
#' @param f function to be evaluated on the mesh
#' @param vectorized logical or function. If TRUE, f is assumed to be vectorized.
#' @param level level/threshold value
#' @param intervals matrix of intervals
#' @param mesh mesh object or type
#' @param ... additional arguments passed to f
#' @export
mesh_level = function(f, vectorized=FALSE, level=0, intervals, mesh, ...) {
  if (!is.mesh(mesh))
    mesh = mesh(intervals, mesh.type=mesh)

  if (is.array(f) && length(f)==nrow(mesh$p))
    mesh$y = f
  else {
    if (isTRUE(vectorized))
      f_vec=function(x,...) f(x,...)
    else if (is.function(vectorized))
      f_vec=function(x,...) vectorized(x,...)
    else
      f_vec=Vectorize.function(f,dim=ncol(mesh$p))
    mesh$y = f_vec(mesh$p,...)
  }
  ## Benchmarking
  # a = runif(5)-0.5
  # microbenchmark::microbenchmark(
  #   any(a>=0) && any(a<=0),
  #   any(diff(sign(a))!=0),
  #   times=100
  # )
  I = which(apply(mesh$tri,1,function(i) {yi=mesh$y[i]-level; any(yi>=0) && any(yi<=0)}))

  colnames(mesh$p) <- colnames(intervals)

  return(list(p=mesh$p,
              y=mesh$y,
              tri=mesh$tri[I,],
              areas=mesh$areas[I],
              neighbours=mesh$neighbours[I]))
}
