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
#' @param ... optional args to pass to 'Apply.function()', including .combine, .lapply, or optional args passed to 'fun'.
#' @return a vectorized function (to be called on matrix argument, on each row)
#' @export
#' @examples
#' f = function(x)x[1]+1; f(1:10); F = Vectorize.function(f,1);
#' F(1:10); #F = Vectorize(f); F(1:10);
#'
#' f2 = function(x)x[1]+x[2]; f2(1:10); F2 = Vectorize.function(f2,2);
#' F2(cbind(1:10,11:20));
Vectorize.function <- function(fun, dim, ...) {
    function(X,...) {
        if (!is.matrix(X)) X = matrix(X,ncol=dim)
        Apply.function(fun,X,...)
    }
}

#' @title Memoize a function
#' @description Before each call of a function, check that the cache holds the results and returns it if available. Otherwise, compute f and cache the result for next evluations.
#' @param fun function to memoize
#' @return a function with same behavior than argument one, but using cache.
#' @importFrom R.cache loadCache
#' @importFrom R.cache saveCache
#' @export
#' @examples
#' f=function(n) rnorm(n);
#' F=Memoize.function(f);
#' F(5); F(6); F(5)
Memoize.function <- function(fun) {
    function(...) {
        arg = list(...)
        res <- loadCache(arg)
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
        saveCache(res, key=arg, comment="fun()")
        res
    }
}
