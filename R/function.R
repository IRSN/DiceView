#' @title Apply Functions Over Array Margins, using custom vectorization (possibly using parallel)
#' @description Emulate parallel apply on a function, from mclapply. Returns a vector or array or list of values obtained by applying a function to margins of an array or matrix.
#' @param FUN function to apply on X
#' @param X array of input values for FUN
#' @param MARGIN 1 indicates to apply on rows (default), 2 on columns
#' @param .combine how to combine results (default using c(.))
#' @param .lapply how to vectorize FUN call (default is parallel::mclapply)
#' @param ... optional arguments to FUN.
#' @return array of values taken by FUN on each row/column of X
#' @export
#' @examples
#' X = matrix(runif(10),ncol=2);
#'   rowSums(X) == apply(X,1,sum)
#'   apply(X,1,sum) == Apply.function(sum,X)
#'
#' X = matrix(runif(10),ncol=1)
#'   rowSums(X) == apply(X,1,sum)
#'   apply(X,1,sum) == Apply.function(sum,X)
#'
#' X = matrix(runif(10),ncol=2)
#' f = function(X) X[1]/X[2]
#' apply(X,1,f) == Apply.function(f,X)
Apply.function <- function(FUN, X, MARGIN=1, .combine=c, .lapply=parallel::mclapply, ...) {
    if (MARGIN==2) return(FUN, X=t(Apply.function(t(X), MARGIN=1, .combine=.combine, .lapply=.lapply,...)))
    if (MARGIN != 1) stop("Do not (yet) support MARGIN != 1")

    X.list = lapply(seq_len(nrow(X)), function(i) X[i,])
    l = .lapply(X.list,FUN,...)

    if (is.null(.combine)) return(l)
    do.call(.combine,l)
}

#' @title Vectorize a multidimensional Function
#' @description Vectorize a d-dimensional (input) function, in the same way that base::Vectorize for 1-dimensional functions.
#' @param fun 'dim'-dimensional function to Vectorize
#' @param dim dimension of input arguments of fun
#' @param .combine how to combine results (default using c(.))
#' @param .lapply how to vectorize FUN call (default is parallel::mclapply)
#' @param ... optional args to pass to 'Apply.function()', including .combine, .lapply, or optional args passed to 'fun'.
#' @return a vectorized function (to be called on matrix argument, on each row)
#' @export
#' @examples
#' f = function(x)x[1]+1; f(1:10); F = Vectorize.function(f,1);
#' F(1:10); #F = Vectorize(f); F(1:10);
#'
#' f2 = function(x)x[1]+x[2]; f2(1:10); F2 = Vectorize.function(f2,2);
#' F2(cbind(1:10,11:20));
#'
#' f3 = function(x)list(mean=x[1]+x[2],se=x[1]*x[2]); f3(1:10); F3 = Vectorize.function(f3,2);
#' F3(cbind(1:10,11:20));
Vectorize.function = function(fun, dim, .combine=rbind, .lapply=parallel::mclapply, ...) {
    function(X,...) {
        if (!is.matrix(X)) X = matrix(X,ncol=dim)
        Apply.function(fun,X, .combine=.combine, .lapply=.lapply,...)
    }
}


#' @title Memoize a function
#' @description Before each call of a function, check that the cache holds the results and returns it if available. Otherwise, compute f and cache the result for next evluations.
#' @param fun function to memoize
#' @param suffix suffix to use for cache files (default ".RcacheDiceView")
#' @return a function with same behavior than argument one, but using cache.
#' @importFrom R.cache loadCache
#' @importFrom R.cache saveCache
#' @export
#' @examples
#' f=function(n) rnorm(n);
#' F=Memoize.function(f);
#' F(5); F(6); F(5)
Memoize.function <- function(fun, suffix=".RcacheDiceView") {
    function(...) {
        arg = list(...)
        res <- loadCache(arg, suffix=suffix)
        if (!is.null(res)) {
            # cat("Loaded cached result\n")
            #cat(".")
            return(res)
        }
        # 2. If not available, generate it.
        # cat("Generating result from scratch...")
        #cat("o")
        res <- fun(...)
        # Emulate slow algorithm
        # cat("ok\n")
        saveCache(res, key=arg, comment="DiceView", suffix=suffix)
        res
    }
}

#' Create a Data Frame from all combinations of factor variables
#' @description Generalization of base::expand.grid to more than 2 variables.
#' @param d number of variables (taken in following arguments with modulo)
#' @param ... variables to combine, as arrays of values
#' @return data frame of all possible combinations of variables values
#' @export
#' @examples
#' expand.grids(d=1)
#' expand.grids(d=1,seq(f=0,t=1,l=11))
#' expand.grids(d=2)
#' expand.grids(d=2,seq(f=0,t=1,l=11))
#' expand.grids(d=2,seq(f=0,t=1,l=11),seq(0,1,l=3))
#' expand.grids(d=3,seq(f=0,t=1,l=5))
#' expand.grids(d=NULL,seq(f=0,t=1,l=5),seq(f=0,t=1,l=5),seq(f=0,t=1,l=5))
#' expand.grids(seq(f=0,t=1,l=5),seq(f=0,t=1,l=5),seq(f=0,t=1,l=5))
#' expand.grids(d=4,seq(f=0,t=1,l=5))
expand.grids <- function(d=length(list(...)),...) {
    if (isTRUE(is.na(d)) || isTRUE(is.null(d))) d = length(list(...)) # when d is explicitely NA, compute it.
    s = list(...)
    if (length(d)>0) { # When d is not set, it effectively contains first sample.
        s = c("d",s)
        s[[1]] = d
        d = length(s)
    }
    if (length(s)==0) s=list(c(0,1)) # when no ... provided, uses {0,1}
    if (length(s)==1 & is.list(s[[1]])) s = s[[1]] # when ... is already a list of samples
    l=list(s[[1]])
    if (d>1) for (i in 2:d) {
        l=c(l,list(s[[ (i-1) %% length(s) + 1 ]])) # weird formula when table index starts from 1...
    }
    return(do.call("expand.grid",l))
}

#' eval function and cast result to a list of y, y_low, y_up (possibly NA)
#' @param fun function to evaluate
#' @param X matrix of input values for fun
#' @param vectorized whether fun is vectorized or not
#' @param dim dimension of input values for fun if
#' @return list of y, y_low, y_up
EvalInterval.function = function(fun,X, vectorized=FALSE, dim=ncol(X)) {
        if (!vectorized)
            Fun = Vectorize.function(fun, dim)
        else
            Fun = fun

        if (is.null(X)) {
            if (is.null(dim)) dim = 1
            X = matrix(c(0,1), ncol=dim, nrow=2)
        }

        f_x <- Fun(as.matrix(X))

        # Now cast that eval :
        y <- NA #array(NA, npoints)
        y_low <- NA #array(NA, npoints)
        y_up <- NA #array(NA, npoints)
        if (is.list(f_x)) {
            f_x = lapply(as.list(as.data.frame(f_x)),unlist)
            if (!("mean" %in% names(f_x)) || !("se" %in% names(f_x)))
                stop(paste0("If function returns a list, it must have 'mean' and 'se', while was ",paste0(collapse="\n",utils::capture.output(print(f_x)))))
            y <- as.numeric(f_x$mean)
            if (!is.numeric(y))
                stop("If function returns a list, 'mean' must be (as) numeric:",paste0(y,collapse="\n"))

            if (!is.numeric(as.numeric(f_x$se)))
                stop("If function returns a list, 'se' must be (as) numeric:",paste0(f_x$se,collapse="\n"))
            y_low <- y - as.numeric(f_x$se)
            y_up <- y + as.numeric(f_x$se)

        } else if (is.matrix(f_x) && ncol(f_x)==2) {
            y_low <- as.numeric(f_x[,1])
            y_up <- as.numeric(f_x[,2])
            if (!is.numeric(y_low))
                stop("If function returns a matrix, first column must be (as) numeric.")
            if (!is.numeric(y_up))
                stop("If function returns a matrix, second column must be (as) numeric.")
        } else { # simple function, not a list nor a matrix
            y <- as.numeric(f_x)
            if (!is.numeric(y))
                stop("If function does not returns a list, it must be (as) numeric.")
        }

        return(list(y=y,y_low=y_low,y_up=y_up))
}
