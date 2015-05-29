#' ModelObject RefClass Object
#' 
#' @field name Character string
#' @field expr R expression that defines the model equation
#' @field P a list of named numeric vectors defining parameters:
#'      \code{list(p0=<initial guess>, lb=<lower bounds>, ub=<upper bounds>)}
#' 
#' @export ModelObject
#' @exportClass ModelObject
ModelObject = setRefClass('ModelObject',
                          fields = list(
                            name = 'character',
                            expr = 'expression',
                            P    = 'list',   # parameter initial guess and bounds
                            q    = 'numeric' # parameter fitted values
                          ),
                          methods = list(
                            initialize = function(...) {
                              'initialize object'
                              
                              initFields(...)
                              
                              # initally set the fitted parameter values to the initial guess
                              .self$q = .self$P$p0
                            },
                            value = function(p=NULL, data) {
                              'Evaluates the model'
                              
                              if (is.null(p)) {
                                p = .self$q
                              }
                              return(eval(.self$expr, c(as.list(p), as.list(data))))
                            },
                            jacobian = function(p=NULL, data) {
                              'Evaluates the model jacobian'
                              
                              if (is.null(p)) {
                                p = .self$q
                              }
                              
                              
                              J = sapply(all.vars(.self$expr), function(v, env) {
                                return(eval(D(.self$expr, v), env))
                              }, env = c(as.list(p), as.list(data)), simplify=F)
                              
                              # interesting edge problem:
                              # if D() returns a constant, it will be of length 1 causing sapply()
                              # to return a list-matrix
                              #
                              # solution:
                              # use rbind and replication rules instead of assuming sapply() will
                              # return a numeric data.matrix
                              J = do.call(rbind, J)
                              
                              return(J[names(p),,drop=F])
                            },
                            setP = function(p0=NULL, lb=NULL, ub=NULL) {
                              'Sets initial parameter values, and optionally upper and lower bounds.
                              Arguments p0, lb, and ub should be named numeric vectors'
                              
                              if (!is.null(p0)) {
                                if (is.null(names(p0))) {
                                  message('argument `p0` must be a named vector')
                                }
                                else {
                                  .self$P$p0[names(p0)] = p0
                                }  
                              }
                              
                              if (!is.null(lb)) {
                                if (is.null(names(lb))) {
                                  message('argument `lb` must be a named vector')
                                }
                                else {
                                  .self$P$lb[names(lb)] = lb
                                }  
                              }
                              
                              if (!is.null(ub)) {
                                if (is.null(names(ub))) {
                                  message('argument `ub` must be a named vector')
                                }
                                else {
                                  .self$P$ub[names(ub)] = ub
                                }
                              }
                              
                              invisible(.self)
                            }
                          )
)