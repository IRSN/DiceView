#### Generalization of optim (multistart) ####

#' Title optim wrapper for early stopping criterion
#' @param par starting point for optim
#' @param fn objective function, like in optim().
#' @param gr gradient function, like in optim().
#' @param fn.stop early stopping criterion
#' @param fn.NaN replacement value of fn when returns NaN
#' @param control control parameters for optim()
#' @param ... additional arguments passed to optim()
#' @return list with best solution and all solutions
#' @export
#' @author Yann Richet, IRSN
#' @examples
#' fn = function(x) x^6
#' o = optim( par=15, fn,lower=-20,upper=20,method='L-BFGS-B')
#' o.s = optim.stop( par=15, fn,lower=-20,upper=20,method='L-BFGS-B',fn.stop=0.1)
#' #check o.s$value == 0.1 && o.s$counts < o$counts
optim.stop <- function(par, fn, gr=NULL,
                       fn.stop=NA, fn.NaN=NaN,
                       control=list(), ...) {
    if (is.na(fn.stop)) {
        fn.o = function(...) {
            y = fn(...)
            y[which(is.nan(y))] <- fn.NaN
            return(y)
        }
        gr.o = gr
    } else {
        fn.o = function(...) {
            y = fn(...)
            if ((isTRUE(fn.stop > y) && !isFALSE(control$fnscale > 0)) || (isTRUE(fn.stop < y) && isTRUE(control$fnscale < 0))) {# reach early stopping criterion
                warning(paste("Early stopping criterion reached at", paste0(round(y, 3), collapse = ","), "for", paste0(round(par, 3), collapse = ",")))
                return(fn.stop)
            }
            y[which(is.nan(y))] <- fn.NaN
            return(y)
        }
        gr.o = if (is.null(gr)) NULL else function(...) {
            dy = gr(...)
            y = fn(...)
            if ((isTRUE(fn.stop > y) && !isFALSE(control$fnscale > 0)) || (isTRUE(fn.stop < y) && isTRUE(control$fnscale < 0))) # reach early stopping criterion
                return(rep(0,length(dy)))
            return(dy)
        }

    }
    optim(par=par, fn=fn.o, gr=gr.o, control=control,...)
}

#' Title Multi-local optimization wrapper for optim, using (possibly parallel) multistart.
#'
#' @param pars starting points for optim
#' @param fn objective function, like in optim().
#' @param fn.NaN replacement value of fn when returns NaN
#' @param fn.stop early stopping criterion
#' @param .apply loop/parallelization backend for multistart ("mclapply", "lapply" or "foreach")
#' @param pars.eps minimal distance between two solutions to be considered different
#' @param control control parameters for optim()
#' @param ... additional arguments passed to optim()
#'
#' @return list with best solution and all solutions
#' @importFrom foreach foreach
#' @importFrom foreach %dopar%
#' @export
#' @author Yann Richet, IRSN
#' @examples
#' fn = function(x) ifelse(x==0,1,sin(x)/x)
#' # plot(fn, xlim=c(-20,20))
#' optim( par=5,                     fn,lower=-20,upper=20,method='L-BFGS-B')
#' optims(pars=t(t(seq(-20,20,,20))),fn,lower=-20,upper=20,method='L-BFGS-B')
#'
#' # Branin function (3 local minimas)
#' f = function (x) {
#'   x1 <- x[1] * 15 - 5
#'   x2 <- x[2] * 15
#'   (x2 - 5/(4 * pi^2) * (x1^2) + 5/pi * x1 - 6)^2 + 10 * (1 - 1/(8 * pi)) * cos(x1) + 10
#' }
#' # expect to find 3 local minimas
#' optims(pars=matrix(runif(100),ncol=2),f,method="L-BFGS-B",lower=c(0,0),upper=c(1,1))
optims <- function(pars,fn,fn.NaN=NaN,fn.stop=NA,.apply="mclapply",pars.eps=1E-5,control=list(),...) {
    if (is.character(.apply)) {
        if (.apply=="lapply")
            O = lapply(X=as.list(as.data.frame(t(pars))),
                       FUN=function(x0){
                           try(optim.stop(par=x0,fn=fn, fn.stop=fn.stop,fn.NaN=fn.NaN, control=control,...))})
        else if (.apply=="mclapply")
            O = parallel::mclapply(X=as.list(as.data.frame(t(pars))),
                                   FUN=function(x0){
                                       try(optim.stop(par=x0,fn=fn, fn.stop=fn.stop,fn.NaN=fn.NaN, control=control,...))})
        else if (.apply=="foreach") {
            i.o = NULL
            O = foreach::foreach(i.o = 1:nrow(pars),.errorhandling = "remove") %dopar% { return(
                try(optim.stop(par=pars[i.o,],fn=fn, fn.stop=fn.stop,fn.NaN=fn.NaN, control=control,...))) }
        }
    } else
        O = .apply(X=as.list(as.data.frame(t(pars))),
                   FUN=function(x0){return(
                       try(optim.stop(par=x0,fn=fn, fn.stop=fn.stop,fn.NaN=fn.NaN, control=control,...)))})

    best = list(par=NA,value=NA)
    all = NULL
    is_best = function(best_value,o_value) {
        if (is.numeric(best_value) && is.numeric(o_value)) {
            if (!is.null(control$fnscale) && control$fnscale < 0)
                return(!isTRUE(best_value > o_value))
            else
                return(!isTRUE(best_value < o_value))
        } else return(FALSE)
    }
    for (o in O) {if ("value" %in% names(o)) {
        if (is.numeric(o$value) && !is.numeric(best$value)) # first value
            best = o
        if (is.numeric(o$value) && is_best(best$value, o$value)) # best value
            best = o
        if (is.null(all))
            all = list(pars=matrix(o$par,nrow=1),values=matrix(o$value,nrow=1))
        else if (min_dist(o$par,all$pars)>pars.eps) {
            all$pars=rbind(all$pars,o$par)
            all$values=rbind(all$values,o$value)
        }
    }}
    c(best,all)
}
