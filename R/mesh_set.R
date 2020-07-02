#### Generalization of root finding ####

#' One Dimensional Root (Zero) Finding
#' @description Search one root with given precision (on y). Iterate over uniroot as long as necessary.
#' @param f the function for which the root is sought.
#' @param lower the lower end point of the interval to be searched.
#' @param upper the upper end point of the interval to be searched.
#' @param maxerror_f the maximum error on f evaluation (iterates over uniroot to converge).
#' @param f_lower the same as f(lower).
#' @param f_upper the same as f(upper).
#' @param tol the desired accuracy (convergence tolerance on f arg).
#' @param convexity the learned convexity factor of the function, used to reduce the boundaries for uniroot.
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
root = function(f,lower,upper,maxerror_f=1E-7, f_lower = f(lower, ...),f_upper = f(upper, ...), tol = .Machine$double.eps^0.25, convexity=0, ...) {
    if (upper < lower)
        return(root(f,lower=upper,upper=lower,maxerror_f=maxerror_f, f_lower = f(upper, ...), f_upper=f(lower, ...), tol = tol, convexity=convexity, ...))

    r = NULL
    try(
        r <- uniroot(f,lower=lower,upper=upper,f.lower = f_lower,f.upper = f_upper,tol=tol,...)
        ,silent=T)
    if (is.null(r)) {
        warning(paste0("No root found in [",lower,",",upper,"] -> [",f_lower,",",f_upper,"]"))
        return(NULL)
    }

    r_root = r$root
    f_root = f(r_root, ...)
    err = abs(f_root)/maxerror_f

    # cat(paste0("[ ",lower," | ",r_root," | ",upper," ] -> ^",exp(convexity)," -> [ ",f_lower," | ",f_root," | ",f_upper," ]\n"))

    if (err>1) {
        if (f_lower*f_root<0) {
            # cat("<")
            x0 = r_root - (r_root-lower) * max(1E-3,(f_root/(f_root-f_lower))^exp(convexity))
            f_x0 = f(x0, ...)
            # cat(paste0(" x0:",x0," -> ",f_x0))
            if (f_x0*f_root<0){
                # cat("+")
                return(root(f, lower=x0,upper=r_root,maxerror_f=maxerror_f,f_lower=f_x0,f_upper=f_root, convexity=convexity-f_x0/(f_root-f_lower) ,tol=tol,...))
            } else {
                # cat("-")
                return(root(f, lower=lower,upper=x0,maxerror_f=maxerror_f,f_lower=f_lower,f_upper=f_x0, convexity=convexity-f_x0/(f_root-f_lower) ,tol=tol,...))
            }
        } else { #(f_upper*f_root<0)
            # cat(">")
            x0 = r_root + (upper-r_root) * max(1E-3,(f_root/(f_root-f_upper))^exp(convexity))
            f_x0 = f(x0, ...)
            # cat(paste0(" x0:",x0," -> ",f_x0))
            if (f_x0*f_root<0){
                # cat("-")
                return(root(f, lower=r_root,upper=x0,maxerror_f=maxerror_f,f_lower=f_root,f_upper=f_x0, convexity=convexity-f_x0/(f_upper-f_root) ,tol=tol,...))
            }else{
                # cat("+")
                return(root(f, lower=x0,upper=upper,maxerror_f=maxerror_f,f_lower=f_x0,f_upper=f_upper, convexity=convexity-f_x0/(f_upper-f_root) ,tol=tol,...))
            }
        }
    } else
        r$root
}

#' One Dimensional Multiple Roots (Zero) Finding
#' @description Search multiple roots of 1D function, sampled/splitted by a (1D) mesh
#' @param f Function to find roots
#' @param interval bounds to inverse in
#' @param split.size number of parts to perform uniroot inside
#' @param split function or "unif" or "seq" (default) to preform interval partition
#' @param maxerror_f the maximum error on f evaluation (iterates over uniroot to converge).
#' @param tol the desired accuracy (convergence tolerance on f arg).
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
roots = function(f,interval, maxerror_f=1E-7,split="seq",split.size=11,tol=.Machine$double.eps^0.25,...) {
    lower = min(interval)
    upper = max(interval)

    if (is.function(split))
        intervals = split(min=lower,max=upper,n=split.size)
    else if (is.numeric(split))
        intervals = split
    else if (split=="seq")
        intervals = seq(from=lower,to=upper,length.out=split.size)
    else if (split=="unif")
        intervals = runif(min=lower,max=upper,n=split.size)
    else stop("unsupported mesh: function(min,max,n)/array/'seq'/'fun': type is ",typeof(split)," : ",paste0(split,collapse=";",sep=","))

    # roots = foreach (i = 1:(length(intervals)-1)) %dopar% {
    #     lower.i = intervals[i]
    #     upper.i = intervals[i+1]
    #     r = NULL
    #     try({r = root(f,lower=lower.i,upper=upper.i,maxerror_f = maxerror_f,...)},silent = T)
    #     r
    # }
    I.list = lapply(seq_len(length(intervals)-1), function(i) c(intervals[i],intervals[i+1]))
    roots = parallel::mclapply(I.list,
                               function(I) {
                                   r = NULL;
                                   try({r = root(f,lower=I[1],upper=I[2],maxerror_f = maxerror_f,tol=tol,...)},silent = F);
                                   r
                                })
    roots = unlist(Filter(Negate(is.null), roots))

    if (length(roots)>0)
        r = matrix(unlist(roots),ncol=1)
    else
        r = NA
    attr(r,"mesh") <- list(p=matrix(intervals,ncol=1))
    return(r)
}

#' Minimal distance between one point to many points
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

#' Multi Dimensional Multiple Roots (Zero) Finding, sampled by a mesh
#' @param f Function (one or more dimensions) to find roots of
#' @param intervals bounds to inverse in, each column contains min and max of each dimension
#' @param mesh function or "unif" or "seq" (default) to preform interval partition
#' @param mesh.sizes number of parts for mesh (duplicate for each dimension if using "seq")
#' @param f.vectorized is f already vectorized ? (default: no)
#' @param maxerror_f the maximum error on f evaluation (iterates over uniroot to converge).
#' @param tol the desired accuracy (convergence tolerance on f arg).
#' @param ... Other args for f
#' @import stats
#' @importFrom DiceDesign lhsDesign
#' @export
#' @return matrix of x, so f(x)=0
#' @examples
#' mesh_roots(function(x) x-.51, intervals=rbind(0,1))
#' mesh_roots(function(x) sum(x)-.51, intervals=cbind(rbind(0,1),rbind(0,1)))
#' mesh_roots(sin,intervals=c(pi/2,5*pi/2))
#' mesh_roots(f = function(x) sin(pi*x[1])*sin(pi*x[2]),
#'            intervals = matrix(c(1/2,5/2,1/2,5/2),nrow=2))
#'
#' r = mesh_roots(function(x) (0.25+x[1])^2+(0.5+x[2])^2-.25,
#'                intervals=matrix(c(-1,1,-1,1),nrow=2))
#' plot(r,xlim=c(-1,1),ylim=c(-1,1))
#'
#' r = mesh_roots(function(x) (0.5+x[1])^2+(-0.5+x[2])^2+(0.+x[3])^2- .25,
#'                mesh.sizes = 11,
#'                intervals=matrix(c(-1,1,-1,1,-1,1),nrow=2))
#' scatterplot3d::scatterplot3d(r,xlim=c(-1,1),ylim=c(-1,1),zlim=c(-1,1))
#'
#' mesh_roots(function(x)exp(x)-1,intervals=c(-1,2))
#' mesh_roots(function(x)exp(1000*x)-1,intervals=c(-1,2))
mesh_roots = function(f,f.vectorized=FALSE,intervals, mesh="seq",mesh.sizes=11,maxerror_f=1E-7,tol=.Machine$double.eps^0.25,...) {
    if (is.matrix(intervals)) {
        lowers = apply(intervals,2,min)
        uppers = apply(intervals,2,max)
        d = ncol(intervals)
    } else if (is.null(intervals) && is.matrix(mesh)) {
        lowers = apply(mesh,2,min)
        uppers = apply(mesh,2,max)
        d = ncol(mesh)
    } else { d=1 }

    if (d==1)
        return(roots(f,interval = intervals,split = mesh,split.size = mesh.sizes,maxerror_f=maxerror_f,tol=tol,...))

    if (length(mesh.sizes)!=d)
        mesh.size = rep(mesh.sizes,d)

    # Create mesh points matrix
    if (is.matrix(mesh)) {
        ridge.points = mesh
    } else if (is.function(mesh)) {
        ridge.points = mesh(prod(mesh.size), lowers, uppers)
    } else if (mesh == "seq") {
        ridge.points = matrix(seq(f=lowers[1],to=uppers[1],length.out = mesh.size[1]),ncol=1)
        for (i in 2:d) {
            ridge.points = combn.design(ridge.points, matrix(seq(f=lowers[i],to=uppers[i],length.out = mesh.size[i]),ncol=1))
        }
    } else if (mesh == "unif") {
        ridge.points = matrix(runif(n = prod(mesh.size),min=0,max=1),ncol=d)
        ridge.points = matrix(lowers,ncol=d,nrow=nrow(ridge.points),byrow = TRUE) + ridge.points * (matrix(uppers,ncol=d,nrow=nrow(ridge.points),byrow = TRUE) - matrix(lowers,ncol=d,nrow=nrow(ridge.points),byrow = TRUE))
    } else if (mesh == "LHS") {
        ridge.points = DiceDesign::lhsDesign(prod(mesh.size),dimension=d)$design
        ridge.points = matrix(lowers,ncol=d,nrow=nrow(ridge.points),byrow = TRUE) + ridge.points * (matrix(uppers,ncol=d,nrow=nrow(ridge.points),byrow = TRUE) - matrix(lowers,ncol=d,nrow=nrow(ridge.points),byrow = TRUE))
    } else stop("unsupported mesh : ",mesh)

    # Add bounds if needed
    b = c(lowers[1], uppers[1])
    for (id in 1:(d-1)) {
        b = rbind(cbind(b, lowers[id+1]), cbind(b, uppers[id+1]))
    }
    for (i in 1:nrow(b))
        if (min_dist(b[i,], ridge.points)>.Machine$double.eps)
            ridge.points = rbind(ridge.points,b[i,])

    # y = apply(ridge.points,1,f,...)
    # plot(ridge.points,col=rgb(y>0,0,y<0))

    simplexes <- geometry::delaunayn(ridge.points,output.options = TRUE)

    # Now find ridges where target is bounded, and then root it
    if (isTRUE(f.vectorized))
        f_vec=function(x,...) f(x,...)
    else if (is.function(f.vectorized))
        f_vec=function(x,...) f.vectorized(x,...)
    else
        f_vec=Vectorize.funD(f,d=ncol(ridge.points))
    ridge.y = f_vec(ridge.points,...)

    # Get all ridges where we will do uniroot later
    # ridges = t(combn(simplexes$tri[1,],2))
    # for (i in 1:nrow(simplexes$tri)) {
    #     more_ridges = t(combn(simplexes$tri[i,],2))
    #     for (j in 1:nrow(more_ridges))
    #         if (min_dist(more_ridges[j,], ridges)>.Machine$double.eps)
    #             ridges = rbind(ridges,more_ridges[j,])
    # }
    ridges = Apply.fun(X=simplexes$tri,MARGIN=1,FUN=function(tri_i) {
        more_ridges=NULL
        all_ridges = t(combn(tri_i,2))
        for (j in 1:nrow(all_ridges))
            if (ridge.y[all_ridges[j,1]]*ridge.y[all_ridges[j,2]]<0) # not same sign
                more_ridges = rbind(more_ridges,all_ridges[j,])
        return(more_ridges)
    },.combine=rbind,.lapply=base::lapply)

    ridges = unique(as.matrix(ridges))

    # for (i in 1:nrow(ridges))
    #     lines(x=rbind(ridge.points[ridges[i,1],],ridge.points[ridges[i,2],]))


    # r = foreach(i.r = 1:nrow(ridges),.combine = rbind,.verbose = FALSE,.errorhandling='stop') %dopar% { # No error expected, so stop if occurs
    #     X = ridge.points[ridges[i.r,],,drop=FALSE]
    #     #y=f_vec(X,...)
    #     y = ridge.y[ridges[i.r,]]
    #     if (isFALSE((all(y>0) || all(y<0)))) {
    #         f.r = function(alpha,...)
    #             as.numeric(f(alpha*X[1,] + (1-alpha)*X[2,],...))
    #         alpha_i = root(f.r,lower=0,upper=1, maxerror_f=maxerror_f,...)
    #         return(alpha_i*X[1,] + (1-alpha_i)*X[2,])
    #     } else return(NULL)
    # }

    # r.list = lapply(seq_len(nrow(ridges)), function(i.r) ridges[i.r,])
    # r = parallel::mclapply(r.list,
    #                        function(ridge_i) {
    #                             X = ridge.points[ridge_i,,drop=FALSE]
    #                             y = ridge.y[ridge_i]
    #                             if (isFALSE((all(y>0) || all(y<0)))) {
    #                                 f.r = function(alpha,...)
    #                                     as.numeric(f(alpha*X[1,] + (1-alpha)*X[2,],...))
    #                                 alpha_i = root(f.r,lower=0,upper=1, maxerror_f=maxerror_f,tol=tol,...)
    #                                 return(alpha_i*X[1,] + (1-alpha_i)*X[2,])
    #                             } else return(NULL)
    #                         })
    # r = do.call("rbind",r)

    r = Apply.fun(X=ridges,MARGIN=1,FUN=function(ridge_i) {
        X = ridge.points[ridge_i,,drop=FALSE]
        y = ridge.y[ridge_i]
        if (isFALSE((all(y>0) || all(y<0)))) {
            f.r = function(alpha,...)
                as.numeric(f(alpha*X[1,] + (1-alpha)*X[2,],...))
            alpha_i = root(f.r,lower=0,upper=1, maxerror_f=maxerror_f,tol=tol,...)
            return(alpha_i*X[1,] + (1-alpha_i)*X[2,])
        } else return(NULL)
    },.combine = rbind)

    if (is.null(r) || length(r)==0)
        r = NA
    else {
        colnames(r) <- colnames(intervals)
        rownames(r) <- NULL
    }
    if (!is.null(ridge.points)) {
        attr(r,"mesh") <- simplexes
    }
    # TODO attr(r,"values") <- y

    # points(r,pch=20)
    return(r)
}


#### Excursion sets ####

#' Search excursion set of nD function, sampled by a mesh
#' @param f Function to inverse at 'threshold'
#' @param threshold target value to inverse
#' @param sign focus at conservative for above (sign=1) or below (sign=-1) the threshold
#' @param intervals bounds to inverse in, each column contains min and max of each dimension
#' @param mesh function or "unif" or "seq" (default) to preform interval partition
#' @param mesh.sizes number of parts for mesh (duplicate for each dimension if using "seq")
#' @param maxerror_f maximal tolerance on f precision
#' @param ex_filter.tri boolean function to validate a geometry::tri as considered in excursion : 'any' or 'all'
#' @param ... parameters to forward to mesh_roots(...) call
#' @param f.vectorized is f already vectorized ? (default: no)
#' @param tol the desired accuracy (convergence tolerance on f arg).
#' @importFrom geometry delaunayn
#' @export
#' @examples
#' mesh_exsets(function(x) x, threshold=.51, sign=1, intervals=rbind(0,1))
#' mesh_exsets(function(x) x, threshold=.50000001, sign=1, intervals=rbind(0,1))
#' mesh_exsets(function(x) sum(x), threshold=.51,sign=1, intervals=cbind(rbind(0,1),rbind(0,1)))
#' mesh_exsets(sin,threshold=0,sign="sup",interval=c(pi/2,5*pi/2))
#' mesh_exsets(f = function(x) sin(pi*x[1])*sin(pi*x[2]),
#'             threshold=0,sign=1, intervals = matrix(c(1/2,5/2,1/2,5/2),nrow=2))
#'
#' e = mesh_exsets(function(x) (0.25+x[1])^2+(0.5+x[2])^2 ,
#'               threshold =0.25,sign=-1, intervals=matrix(c(-1,1,-1,1),nrow=2))
#' plot(e$p,xlim=c(-1,1),ylim=c(-1,1));
#' apply(e$tri,1,function(tri) polygon(e$p[tri,],col=rgb(.4,.4,.4,.4)))
#'
#' \dontrun{
#' e = mesh_exsets(function(x) (0.5+x[1])^2+(-0.5+x[2])^2+(0.+x[3])^2,
#'               threshold = .25,sign=-1, mesh="unif", mesh.sizes = 10,
#'               intervals=matrix(c(-1,1,-1,1,-1,1),nrow=2))
#' rgl::plot3d(e$p,xlim=c(-1,1),ylim=c(-1,1),zlim=c(-1,1));
#' apply(e$tri,1,function(tri)rgl::lines3d(e$p[tri,]))
#' }
mesh_exsets = function(f, f.vectorized=FALSE, threshold, sign, intervals, mesh="seq", mesh.sizes=11, maxerror_f=1E-9,tol=.Machine$double.eps^0.25,ex_filter.tri=all,...) {
    if (sign=="lower" || sign==-1 || sign=="inf" || sign=="<" || isFALSE(sign))
        return(mesh_exsets(f=function(...){-f(...)},
                           f.vectorized=f.vectorized,
                           threshold=-threshold,
                           sign=1,
                           intervals=intervals, mesh=mesh, mesh.sizes=mesh.sizes, maxerror_f=maxerror_f,tol=tol,...))
    if (sign!="upper" && sign!=1 && sign!="sup" && sign!=">" && !isTRUE(sign))
        stop("unknown sign: '",sign,"'")

    f_0 <- function(...) return(f(...)-threshold)
    r <- mesh_roots(f=f_0, f.vectorized=f.vectorized, intervals=intervals,mesh=mesh,mesh.sizes=mesh.sizes,maxerror_f=maxerror_f, tol=tol,...)
    if (all(is.na(r)))
        all_points = attr(r,"mesh")$p
    else
        all_points = rbind(attr(r,"mesh")$p,r)
    new_mesh <- geometry::delaunayn(all_points,output.options=TRUE)

    if (isTRUE(f.vectorized))
        f_vec=function(x,...) f(x,...)
    else if (is.function(f.vectorized))
        f_vec=function(x,...) f.vectorized(x,...)
    else
        f_vec=Vectorize.funD(f,d=ncol(new_mesh$p))
    new_mesh$y = f_vec(new_mesh$p,...)
    # I = foreach (i.I = 1:nrow(new_mesh$tri)) %do% {
    #     X = new_mesh$p[new_mesh$tri[i.I,],,drop=FALSE] # NEVER USE "drop=F" => if "F" object exists, it will not crash but do a mess !!!
    #     y = f_vec(X,...) #apply(X,1,f, ...)
    #     if (all(y>=(threshold-2*maxerror_f))) {
    #         return(i.I)
    #     } else {
    #         return(NULL)
    #     }
    # }
    # I = unlist(Filter(Negate(is.null), I))
    I = which(apply(new_mesh$tri,1,function(i) ex_filter.tri(new_mesh$y[i]>=(threshold-10*maxerror_f))))

    colnames(new_mesh$p) <- colnames(intervals)

    return(list(p=new_mesh$p,tri=new_mesh$tri[I,],areas=new_mesh$areas[I],neighbours=new_mesh$neighbours[I]))
}

#### Plot meshes ####

#' Plot a one dimensional mesh
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

#' Plot a two dimensional mesh
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

#' Plot a three dimensional mesh
#' @param mesh 3-dimensional mesh to draw
#' @param view 3d framework to use: 'rgl' or 'scatterplot3d' (default)
#' @param color color of the mesh
#' @param ... optional arguments passed to plot function
#' @importFrom scatterplot3d scatterplot3d
#' @importFrom rgl plot3d
#' @export
#' @examples
#' plot2d_mesh(mesh_exsets(f = function(x) sin(pi*x[1])*sin(pi*x[2]),
#'                         threshold=0,sign=1, mesh="unif",mesh.size=11,
#'                         intervals = matrix(c(1/2,5/2,1/2,5/2),nrow=2)))
#'
#' plot3d_mesh(mesh_exsets(function(x) (0.5+x[1])^2+(-0.5+x[2])^2+(0.+x[3])^2,
#'                         threshold = .25,sign=-1, mesh="unif", mesh.sizes = 10,
#'                         intervals=matrix(c(-1,1,-1,1,-1,1),nrow=2)))
#'
#' plot3d_mesh(mesh_exsets(function(x) (0.5+x[1])^2+(-0.5+x[2])^2+(0.+x[3])^2,
#'                         threshold = .25,sign=-1, mesh="unif", mesh.sizes = 10,
#'                         intervals=matrix(c(-1,1,-1,1,-1,1),nrow=2)),mode='rgl')
plot3d_mesh = function(mesh,view='scatterplot3d',color='black',...){
    col.rgb=col2rgb(color)/255
    if (view=='scatterplot3d') {
        p3d = scatterplot3d::scatterplot3d(mesh$p,color = rgb(col.rgb[1,],col.rgb[2,],col.rgb[3,],0.4),...)
        apply(mesh$tri,1,function(tri) polygon(p3d$xyz.convert(mesh$p[c(tri,tri[1]),]),border=color))
        return(p3d)
    } else if (view=='rgl') {
        rgl::plot3d(mesh$p,col=rgb(col.rgb[1,],col.rgb[2,],col.rgb[3,],0.4),...)
        apply(mesh$tri,1,function(tri) rgl::quads3d(mesh$p[tri,],col=color,alpha=0.2)) #rgl::lines3d(mesh$p[t(combn(tri,2)),],col=color))
    } else stop("unsupported view: ",view)
}


#### Utility functions ####

#' Generalize expand.grid() for multi-columns data. Build all combinations of lines from X1 and X2. Each line may hold multiple columns.
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

#' Test if points are in a hull
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

#' Checks if some points belong to a given mesh
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

#' Extract points of mesh which belong to the mesh triangulation (may not contain all points)
#' @param mesh mesh (list(p,tri,...) from geometry)
#' @return points coordinates inside the mesh triangulation
#' @export
points_in.mesh = function(mesh) {
    if (is.null(mesh)) return(NULL)
    mesh$p[unique(array(mesh$tri)),, drop = FALSE]
}
#' Extract points of mesh which do not belong to the mesh triangulation (may not contain all points)
#' @param mesh (list(p,tri,...) from geometry)
#' @return points coordinates outside the mesh triangulation
#' @export
points_out.mesh = function(mesh) {
    if (is.null(mesh)) return(NULL)
    mesh$p[-unique(array(mesh$tri)),, drop = FALSE]
}
