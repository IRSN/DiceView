#### Wrapper for view ####

if(!isGeneric("view")) {
    setGeneric(name = "view",
               def = function(model, ...) standardGeneric("view")
               )
}

setMethod("view", "km",
          function(model, type = "auto",model.type = "UK",
                   center = NULL, axis = NULL,
                   npoints = NULL,
                   col_points = "red",
                   col_surf = "blue",
                   col_needles = NA,
                   conf_lev = NULL,
                   conf_blend = NULL,
                   bg_blend = NULL,
                   mfrow = NULL,
                   nlevels = 10,
                   Xname = NULL,
                   yname = NULL,
                   Xscale = 1,
                   yscale = 1,
                   xlim = NULL,
                   ylim = NULL,
                   title = NULL,
                   ...){

              if(is.null(type)) type="auto"
              if(type=="auto"){
                  if(model@d==1) type="xy"
                  if(model@d==2) type="3d"
                  if(model@d>2) {
                      type="section"
                      if(is.null(center)) center=colMeans(model@X)
                  }
              }

              if (type == "section" || type == "xy")
                  sectionview.km(model = model, type = model.type,
                                 center = center, axis = axis,
                                 npoints = ifelse(is.null(npoints),100,npoints),
                                 col_points = col_points,
                                 col_surf = col_surf,
                                 conf_lev = ifelse(is.null(conf_lev),c(0.5,0.8,0.9,0.95,0.99),conf_lev),
                                 conf_blend = conf_blend,
                                 bg_blend = ifelse(is.null(bg_blend),5,bg_blend),
                                 mfrow = mfrow,
                                 Xname = Xname,
                                 yname=yname,
                                 Xscale = Xscale,
                                 yscale = yscale,
                                 xlim = xlim,
                                 ylim = ylim,
                                 title = title,
                                 ...)

              if (type == "section3d" || type == "3d")
                  sectionview3d.km(model = model, type = model.type,
                                   center = center, axis = axis,
                                   npoints = ifelse(is.null(npoints),20,npoints),
                                   col_points = col_points,
                                   col_surf = col_surf,
                                   col_needles = col_needles,
                                   conf_lev = ifelse(is.null(conf_lev),c(0.95),conf_lev),
                                   conf_blend = conf_blend,
                                   bg_blend = ifelse(is.null(bg_blend),5,bg_blend),
                                   Xname = Xname,
                                   yname = yname,
                                   Xscale = Xscale,
                                   yscale = yscale,
                                   xlim = xlim,
                                   ylim = ylim,
                                   title = title,
                                   ...)

              if (type == "contour")
                  contourview.km(model = model, type = model.type,
                                 center = center, axis = axis,
                                 npoints = ifelse(is.null(npoints),20,npoints),
                                 col_points = col_points,
                                 col_surf = col_surf,
                                 bg_blend = ifelse(is.null(bg_blend),1,bg_blend),
                                 mfrow = mfrow,
                                 nlevels = nlevels,
                                 Xname = Xname,
                                 yname = yname,
                                 Xscale = Xscale,
                                 yscale = yscale,
                                 xlim = xlim,
                                 ylim = ylim,
                                 title = title,
                                 ...)
          }
          )

setMethod("view", "list",
          function(model,
                   type = "auto",
                   center = NULL, axis = NULL,
                   npoints = NULL,
                   col_points = "red",
                   col_surf = "blue",
                   col_needles = NA,
                   bg_blend = NULL,
                   mfrow = NULL,
                   nlevels = 10,
                   Xname = NULL,
                   yname = NULL,
                   Xscale = 1,
                   yscale = 1,
                   xlim = NULL,
                   ylim = NULL,
                   title = NULL,
                   ...){

              if(is.null(type)) type="auto"
              if(type=="auto"){
                  if(ncol(model$data$X)==1) type="xy"
                  if(ncol(model$data$X)==2) type="3d"
                  if(ncol(model$data$X)>2) {
                      type="section"
                      if(is.null(center)) center=colMeans(model$data$X)
                  }
              }

              if (type == "section" || type == "xy")
                  sectionview.list(model = model,
                                   center = center, axis = axis,
                                   npoints = ifelse(is.null(npoints),100,npoints),
                                   col_points = col_points,
                                   col_surf = col_surf,
                                   bg_blend = ifelse(is.null(bg_blend),5,bg_blend),
                                   mfrow = mfrow,
                                   Xname = Xname,
                                   yname=yname,
                                   Xscale = Xscale,
                                   yscale = yscale,
                                   xlim = xlim,
                                   ylim = ylim,
                                   title = title,
                                   ...)

              if (type == "section3d" || type == "3d")
                  sectionview3d.list(model = model,
                                     center = center, axis = axis,
                                     npoints = ifelse(is.null(npoints),20,npoints),
                                     col_points = col_points,
                                     col_surf = col_surf,
                                     col_needles = col_needles,
                                     bg_blend = ifelse(is.null(bg_blend),5,bg_blend),
                                     Xname = Xname,
                                     yname = yname,
                                     Xscale = Xscale,
                                     yscale = yscale,
                                     xlim = xlim,
                                     ylim = ylim,
                                     title = title,
                                     ...)

              if (type == "contour")
                  contourview.list(model = model,
                                   center = center, axis = axis,
                                   npoints = ifelse(is.null(npoints),20,npoints),
                                   col_points = col_points,
                                   col_surf = col_surf,
                                   bg_blend = ifelse(is.null(bg_blend),1,bg_blend),
                                   mfrow = mfrow,
                                   nlevels = nlevels,
                                   Xname = Xname,
                                   yname = yname,
                                   Xscale = Xscale,
                                   yscale = yscale,
                                   xlim = xlim,
                                   ylim = ylim,
                                   title = title,
                                   ...)
          }
          )

setMethod("view", "function",
          function(model ,type = "auto", dim,
                   center = NULL, axis = NULL,
                   npoints = NULL,
                   col = "blue",
                   mfrow = NULL,
                   nlevels = 10,
                   Xname = NULL,
                   yname = NULL,
                   Xscale = 1,
                   yscale = 1,
                   xlim = c(0,1),
                   ylim = NULL,
                   title = NULL,
                   ...){

              if(is.null(type)) type="auto"
              if(type=="auto"){
                  if(dim==1) type="xy"
                  if(dim==2) type="3d"
                  if(dim>2) {
                      type="section"
                      if(is.null(center)) if(!is.null(xlim)) center=colMeans(xlim) else center=rep(0.5,dim)
                  }
              }

              if (type == "section" || type == "xy")
                  sectionview.function(model, dim = dim,
                                  center = center, axis = axis,
                                  npoints = ifelse(is.null(npoints),100,npoints),
                                  col_surf = col,
                                  mfrow = mfrow,
                                  Xname = Xname,
                                  yname=yname,
                                  Xscale = Xscale,
                                  yscale = yscale,
                                  xlim = xlim,
                                  ylim = ylim,
                                  title = title,
                                  ...)

              if (type == "section3d" || type == "3d")
                  sectionview3d.function(model, dim = dim,
                                    center = center, axis = axis,
                                    npoints = ifelse(is.null(npoints),20,npoints),
                                    col = col,
                                    Xname = Xname,
                                    yname = yname,
                                    Xscale = Xscale,
                                    yscale = yscale,
                                    xlim = xlim,
                                    ylim = ylim,
                                    title = title,
                                    ...)

              if (type == "contour")
                  contourview.function(model, dim = dim,
                                  center = center, axis = axis,
                                  npoints = ifelse(is.null(npoints),20,npoints),
                                  col = col,
                                  mfrow = mfrow,
                                  nlevels = nlevels,
                                  Xname = Xname,
                                  yname = yname,
                                  Xscale = Xscale,
                                  yscale = yscale,
                                  xlim = xlim,
                                  ylim = ylim,
                                  title = title,
                                  ...)
          }
          )

## #' Plot a view of a kriging, modelPredict model or function.
## #' @description Standard entry point function to plot a  view of a kriging, modelPredict model or function. It is useful for a better understanding of a model behaviour. This function is just a wrapping of all other ploting functions (section, contour, section3d), for all supported types (km, list, function).
## #' @param model an object of class \code{"km"}, a list that can be used in a \code{"modelPredict"} call, or a function.
## #' @param type a string to describe the type of view to display: "auto", "section", "xy", "section3d", "3d", "contour".
## #' @param ... other arguments of the \code{sectionview}, \code{sectionview3d} or \code{contourview} function
## #' @export
## #' @examples
## #' ## A 2D example - Branin-Hoo function
## #' ## a 16-points factorial design, and the corresponding response
## #' d <- 2; n <- 16
## #' design.fact <- expand.grid(seq(0, 1, length = 4), seq(0, 1, length = 4))
## #' design.fact <- data.frame(design.fact); names(design.fact) <- c("x1", "x2")
## #' y <- branin(design.fact)
## #'
## #' ## kriging model 1 : matern5_2 covariance structure, no trend, no nugget effect
## #' m1 <- km(design = design.fact, response = y)
## #'
## #' view(m1,"3d")
## #'
## #' view(branin,"3d", dim = 2, col='red', add = TRUE)
## view <- function(model,...){
##     UseMethod("view")
## }


#### Wrapper for sectionview ####

if(!isGeneric("sectionview")) {
    setGeneric(name = "sectionview",
               def = function(model, ...) standardGeneric("sectionview")
               )
}

#' View the model in a section view.
#' @param km kriging model
#' @importFrom methods setMethod
#' @method sectionview km
#' @rdname km-methods
#' @aliases sectionview,km,km-method
setMethod("sectionview", "km",
          function(model,
                   type = "UK",
                   center = NULL,
                   npoints = 100,
                   col_points = "red",
                   col_surf = "blue",
                   conf_lev = c(0.5,0.8,0.9,0.95,0.99),
                   conf_blend = NULL,
                   bg_blend = 5,
                   mfrow = NULL,
                   Xname = NULL,
                   yname = NULL,
                   Xscale = 1,
                   yscale = 1,
                   xlim = NULL,
                   ylim = NULL,
                   title = NULL,
                   ...){

              sectionview.km(model = model,
                             type = type,
                             center = center,
                             npoints = npoints,
                             col_points = col_points,
                             col_surf = col_surf,
                             conf_lev = conf_lev,
                             conf_blend = conf_blend,
                             bg_blend = bg_blend,
                             mfrow = mfrow,
                             Xname = Xname,
                             yname=yname,
                             Xscale = Xscale,
                             yscale = yscale,
                             xlim = xlim,
                             ylim = ylim,
                             title = title,
                             ...)
          }
          )

#' View the model in a section view.
#' @param list DiceEval model
#' @importFrom methods setMethod
#' @method sectionview list
#' @rdname list-methods
#' @aliases sectionview,list,list-method
setMethod("sectionview", "list",
          function(model,
                   center = NULL,
                   npoints = 100,
                   col_points = "red",
                   col_surf = "blue",
                   bg_blend = 5,
                   mfrow = NULL,
                   Xname = NULL,
                   yname = NULL,
                   Xscale = 1,
                   yscale = 1,
                   xlim = NULL,
                   ylim = NULL,
                   title = NULL,
                   ...){

              sectionview.list(model = model,
                               center = center,
                               npoints = npoints,
                               col_points = col_points,
                               col_surf = col_surf,
                               bg_blend = bg_blend,
                               mfrow = mfrow,
                               Xname = Xname,
                               yname = yname,
                               Xscale = Xscale,
                               yscale = yscale,
                               xlim = xlim,
                               ylim = ylim,
                               title = title,
                               ...)
          }
          )

#' View the model in a section view.
#' @param function function, taken as model
#' @importFrom methods setMethod
#' @method sectionview function
#' @rdname function-methods
#' @aliases sectionview,function,function-method
setMethod("sectionview", "function",
          function(model,dim,
                   center = NULL, axis = NULL,
                   npoints = 100,
                   col = "blue",
                   mfrow = NULL,
                   Xname = NULL,
                   yname = NULL,
                   Xscale = 1,
                   yscale = 1,
                   xlim = c(0,1),
                   ylim = NULL,
                   title = NULL,
                   ...){

              sectionview.function(model = model,
                              dim = dim,
                              center = center, axis = axis,
                              npoints = npoints,
                              col_surf = col,
                              mfrow = mfrow,
                              Xname = Xname,
                              yname = yname,
                              Xscale = Xscale,
                              yscale = yscale,
                              xlim = xlim,
                              ylim = ylim,
                              title = title,
                              ...)
          }
          )


#' Plot a section view of a kriging or modelPredict model including design points, or a function.
#' @description Plot one section view per dimension of a kriging, \code{modelPredict} model or function. It is useful for a better understanding of a model behaviour (including uncertainty).
#' @param model an object of class \code{"km"}, a list that can be used in a \code{"modelPredict"} call, or a function.
#' @param ... other arguments of the \code{contourview.km}, \code{contourview.list} or \code{contourview.function} function
#' @importFrom DiceKriging branin
#' @export
#' @examples
#' ## A 2D example - Branin-Hoo function
#' ## a 16-points factorial design, and the corresponding response
#' d <- 2; n <- 16
#' design.fact <- expand.grid(seq(0, 1, length = 4), seq(0, 1, length = 4))
#' design.fact <- data.frame(design.fact); names(design.fact) <- c("x1", "x2")
#' y <- branin(design.fact)
#'
#' ## kriging model 1 : matern5_2 covariance structure, no trend, no nugget effect
#' m1 <- km(design = design.fact, response = y)
#'
#' sectionview(m1, center = c(.333, .333))
#'
#' sectionview(branin, dim = 2, center = c(.333, .333), add=TRUE)
sectionview <- function(model, ...){
    UseMethod("sectionview")
}


#### Wrapper for sectionview3d ####
if(!isGeneric("sectionview3d")) {
    setGeneric(name = "sectionview3d",
               def = function(model, ...) standardGeneric("sectionview3d")
               )
}

#' View the model in a section (3D) view.
#' @param km kriging model
#' @importFrom methods setMethod
#' @method sectionview3d km
#' @rdname km-methods
#' @aliases sectionview3d,km,km-method
setMethod("sectionview3d", "km",
          function(model, type = "UK",
                   center = NULL, axis = NULL,
                   npoints = 20,
                   col_points = "red",
                   col_surf = "blue",
                   col_needles = NA,
                   conf_lev = c(0.95),
                   conf_blend = NULL,
                   bg_blend = 5,
                   Xname = NULL,
                   yname = NULL,
                   Xscale = 1,
                   yscale = 1,
                   xlim = NULL,
                   ylim = NULL,
                   title = NULL,
                   ...){

              sectionview3d.km(model = model, type = type,
                               center = center, axis = axis,
                               npoints = npoints,
                               col_points = col_points,
                               col_surf = col_surf,
                               col_needles = col_needles ,
                               conf_lev = conf_lev,
                               conf_blend = conf_blend,
                               bg_blend = bg_blend,
                               Xname = Xname,
                               yname = yname,
                               Xscale = Xscale,
                               yscale = yscale,
                               xlim = xlim,
                               ylim = ylim,
                               title = title,
                               ...)
          }
          )

#' View the model in a section (3D) view.
#' @param list DiceEval model
#' @importFrom methods setMethod
#' @method sectionview3d list
#' @rdname list-methods
#' @aliases sectionview3d,list,list-method
setMethod("sectionview3d", "list",
          function(model,
                   center = NULL, axis = NULL,
                   npoints = 20,
                   col_points = "red",
                   col_surf = "blue",
                   bg_blend = 5,
                   Xname = NULL,
                   yname = NULL,
                   Xscale = 1,
                   yscale = 1,
                   xlim = NULL,
                   ylim = NULL,
                   title = NULL,
                   ...){

              sectionview3d.list(model = model,
                                 center = center, axis = axis,
                                 npoints = npoints,
                                 col_points = col_points,
                                 col_surf = col_surf,
                                 bg_blend = bg_blend,
                                 Xname = Xname,
                                 yname = yname,
                                 Xscale = Xscale,
                                 yscale = yscale,
                                 xlim = xlim,
                                 ylim = ylim,
                                 title = title,
                                 ...)
          }
          )

#' View the model in a section (3D) view.
#' @param function function, taken as model
#' @importFrom methods setMethod
#' @method sectionview3d function
#' @rdname function-methods
#' @aliases sectionview3d,function,function-method
setMethod("sectionview3d", "function",
          function(model,dim,
                   center = NULL, axis = NULL,
                   npoints = 20,
                   col = "blue",
                   Xname = NULL,
                   yname = NULL,
                   Xscale = 1,
                   yscale = 1,
                   xlim = c(0,1),
                   ylim = NULL,
                   title = NULL,
                   ...){

              sectionview3d.function(model, dim = dim,
                                center = center, axis = axis,
                                npoints = npoints,
                                col = col,
                                Xname = Xname,
                                yname = yname,
                                Xscale = Xscale,
                                yscale = yscale,
                                xlim = xlim,
                                ylim = ylim,
                                title = title,
                                ...)
          }
          )


#' Plot a 3-D (using RGL) view of a kriging or modelPredict model, including design points
#' @description Plot a 3-D view of a kriging or modelPredict model. It is useful for a better understanding of a model behaviour.
#' @param model an object of class \code{"km"}, a list that can be used in a \code{"modelPredict"} call, or a function.
#' @param ... other arguments of the \code{contourview.km}, \code{contourview.list} or \code{contourview.function} function
#' @importFrom methods setMethod
#' @importFrom DiceKriging branin
#' @export
#' @examples
#' ## A 2D example - Branin-Hoo function
#' ## a 16-points factorial design, and the corresponding response
#' d <- 2; n <- 16
#' design.fact <- expand.grid(seq(0, 1, length = 4), seq(0, 1, length = 4))
#' design.fact <- data.frame(design.fact); names(design.fact) <- c("x1", "x2")
#' y <- branin(design.fact)
#'
#' ## kriging model 1 : matern5_2 covariance structure, no trend, no nugget effect
#' m1 <- km(design = design.fact, response = y)
#'
#' sectionview3d(m1)
#'
#' sectionview3d(branin, dim = 2, add=TRUE)
sectionview3d <- function(model, ...){
    UseMethod("sectionview3d")
}

#### Wrapper for contourview ####
if(!isGeneric("contourview")) {
    setGeneric(name = "contourview",
               def = function(model, ...) standardGeneric("contourview")
           )
}

#' View the model in a contour (2D) view.
#' @param km kriging model
#' @importFrom methods setMethod
#' @method contourview km
#' @rdname km-methods
#' @aliases contourview,km,km-method
setMethod("contourview", "km",
          function(model, type = "UK",
                   center = NULL, axis = NULL,
                   npoints = 20,
                   col_points = "red",
                   col_surf = "blue",
                   bg_blend = 1,
                   nlevels = 10,
                   Xname = NULL,
                   yname = NULL,
                   Xscale = 1,
                   yscale = 1,
                   xlim = NULL,
                   ylim = NULL,
                   title = NULL,
                   ...){

              contourview.km(model = model, type = type,
                             center = center, axis = axis,
                             npoints = npoints,
                             col_points = col_points,
                             col_surf = col_surf,
                             bg_blend = bg_blend,
                             nlevels = nlevels,
                             Xname = Xname,
                             yname = yname,
                             Xscale = Xscale,
                             yscale = yscale,
                             xlim = xlim,
                             ylim = ylim,
                             title = title,
                             ...)
          }
          )

#' View the model in a contour (2D) view.
#' @param list DiceEval model
#' @importFrom methods setMethod
#' @method contourview list
#' @rdname list-methods
#' @aliases contourview,list,list-method
setMethod("contourview", "list",
          function(model,
                   center = NULL, axis = NULL,
                   npoints = 20,
                   col_points = "red",
                   col_surf = "blue",
                   bg_blend = 1,
                   nlevels = 10,
                   Xname = NULL,
                   yname = NULL,
                   Xscale = 1,
                   yscale = 1,
                   xlim = NULL,
                   ylim = NULL,
                   title = NULL,
                   ...){

              contourview.list(model = model,
                               center = center, axis = axis,
                               npoints = npoints,
                               col_points = col_points,
                               col_surf = col_surf,
                               bg_blend = bg_blend,
                               nlevels = nlevels,
                               Xname = Xname,
                               yname = yname,
                               Xscale = Xscale,
                               yscale = yscale,
                               xlim = xlim,
                               ylim = ylim,
                               title = title,
                               ...)
          }
          )

#' View the model in a contour (2D) view.
#' @param function function, taken as model
#' @importFrom methods setMethod
#' @method contourview function
#' @rdname function-methods
#' @aliases contourview,function,function-method
setMethod("contourview", "function",
          function(model,dim,
                   center = NULL, axis = NULL,
                   npoints = 20,
                   col = "blue",
                   nlevels = 10,
                   Xname = NULL,
                   yname = NULL,
                   Xscale = 1,
                   yscale = 1,
                   xlim = c(0,1),
                   ylim = NULL,
                   title = NULL,
                   ...){

              contourview.function(model = model, dim = dim,
                              center = center, axis = axis,
                              npoints = npoints,
                              col = col,
                              nlevels = nlevels,
                              Xname = Xname,
                              yname = yname,
                              Xscale = Xscale,
                              yscale = yscale,
                              xlim = xlim,
                              ylim = ylim,
                              title = title,
                              ...)
          }
          )

#' Plot a contour view of a kriging or modelPredict model including design points, or a function.
#' @description Plot a contour view of a kriging or modelPredict model. It is useful for a better understanding of a model behaviour.
#' @param model an object of class \code{"km"}, a list that can be used in a \code{"modelPredict"} call, or a function.
#' @param ... other arguments of the \code{contourview.km}, \code{contourview.list} or \code{contourview.function} function
#' @importFrom DiceKriging branin
#' @importFrom DiceKriging km
#' @export
#' @examples
#' ## A 2D example - Branin-Hoo function
#' ## a 16-points factorial design, and the corresponding response
#' d <- 2; n <- 16
#' design.fact <- expand.grid(seq(0, 1, length = 4), seq(0, 1, length = 4))
#' design.fact <- data.frame(design.fact); names(design.fact) <- c("x1", "x2")
#' y <- branin(design.fact)
#'
#' ## kriging model 1 : matern5_2 covariance structure, no trend, no nugget effect
#' m1 <- km(design = design.fact, response = y)
#'
#' contourview(m1)
#'
#' contourview(branin, dim = 2, add=TRUE)
contourview <- function(model, ...){
    UseMethod("contourview")
}
