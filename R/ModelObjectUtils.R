#' Generalized non-linear model fitting
#' 
#' Fits a non-linear model using minimization of squared residuals
#' @param data input data.frame or list to fit
#' @param MODEL an object of class \link{ModelObject} defining the model to fit
#' 
#' @param optimizer Name of the optimizer to use for minimization. Default 'nlminb'.
#'   Optimizer 'trust' requires installing the 'trust' package.
#' @param control control list to pass to \link{nlminb}. Ignored if using 'trust'.
#' 
#' @details
#' MODEL is expected to be of the form:
#' \deqn{y = f(p, x)}
#' 
#' Thus, data to be fitted must have a named element \code{x} indicating the 
#' independent variable and a named element \code{y} indicating the dependent
#' (observed) variable.
#' 
#' @return
#' A \code{fit} list-object as returned by \link{nlminb} with additional elements:
#' \describe{
#'    \item{residuals}{A numeric vector of residuals}
#'    \item{DATA}{
#'      A copy of the data used to fit model parameters
#'    }
#'    \item{MODEL}{
#'      A copy of \code{MODEL} provided to the optimization.
#'      Used for evaulating the model prediction using the fitted parameters
#'    }
#' }
#' 
#' @seealso \link{nlminb}
#' 
#' @import trust
#' @export
ModelObjectFit = function(data, MODEL, weightFun = NULL, optimizer=c('nlminb', 'trust', 'hybrid'), control = NULL, trust.bounded=F) {
  # get model
  MODEL = deepcopy(MODEL)
  
  if (!inherits(MODEL, 'ModelObject')) {
    stop('MODEL is not a ModelObject')
  }
  
  if (is.null(MODEL$P)) stop('Missing initial parameter estimate and bounds')
  
  modelFun = MODEL$value
  jacFun = MODEL$jacobian
  P = MODEL$P
  
  if (is.null(weightFun)) {
    # default to no weighting
    weightFun = function(data) {
      return(1)
    }
  }
  
  if (!is.function(weightFun)) {
    stop('weightFun must be a function that returns a numeric value')
  }
  
  # internal optimization functions
  resFun = function(p, data) {
    'computes residual'
    
    p = as.list(p)
    res = data$y - modelFun(p, data)
    return(res)
  }
  objFun = function(p, data) {
    'computes objective - sum of squared residuals'
    
    p = as.list(p)
    res = resFun(p, data)*weightFun(data)
    return((res %*% res)[1])
  }
  hessFun = function(p, data) {
    'computes (reasonably approximate) hessian matrix'
    
    # this is actually an approximation of the objective hessian
    
    # the true objective hessian is
    # del^2 f  = J'J + h (del^2 h)
    # where f is the objective and h is the residual
    # in most cases the term J'J dominates
    
    p = as.list(p)
    jac = jacFun(p, data)
    return(jac %*% t(jac))
  }
  gradFun = function(p, data) {
    'computes objective gradient'
    
    p = as.list(p)
    grad = (-jacFun(p, data) %*% (resFun(p, data)*weightFun(data)^2))[,1]
    names(grad) = names(p)
    return(grad)
  }
  
  # if jacFun is missing (e.g. NULL) set gradFun and hessFun to NULL
  if (is.null(jacFun) || !is.function(jacFun)) {
    gradFun = hessFun = NULL
  }
  
  # wrap different optimizers
  # notably trust()
  fit = switch(match.arg(optimizer),
               nlminb = {
                 # merge control parameters
                 control = c(control, list(eval.max=1e4, iter.max=1e4))
                 
                 fit = with(P, {
                   p0 = as.list(p0)
                   nlminb(p0, objective = objFun, gradient = gradFun, hessian = hessFun,
                          data = data, lower=lb, upper=ub,
                          control = control)
                 })
                 
                 fit$message = paste(sprintf('%s', fit$message),
                                     sprintf('iter: %d', fit$iterations),
                                     sprintf('value: %g', fit$objective),
                                     sep = '; ')
                 
                 # set fitted parameters on input model
                 MODEL$q = fit$par
                 
                 fit
               },
               trust = {
                 
                 # the objective function for trust() needs to return a list of
                 # .. $value, $gradient, and $hessian
                 # wrap existing objFun, gradFun, and hessFun
                 trustObjFun = function(p) {
                   
                   # crudely implement bounds
                   inbounds = apply(cbind(p=p, lb=P$lb, ub=P$ub), 1, function(rng) {
                     rng['p'] >= rng['lb'] & rng['p'] <= rng['ub']
                   })
                   
                   value = objFun(p, data)
                   if (!all(inbounds) & trust.bounded) return(list(value = Inf))
                   
                   list(value    = value,
                        gradient = gradFun(p, data),
                        hessian  = hessFun(p, data))
                 }
                 
                 fit = with(P, {
                   # estimate the initial trust radius
                   rinit = sqrt(p0 %*% (hessFun(p0, data) %*% p0))
                   trust(trustObjFun, 
                         parinit = p0, rinit = rinit, rmax = 1000*rinit, 
                         parscale = rep(1, length(p0)), iterlim = 1e4,
                         minimize = TRUE, blather = TRUE)
                 })
                 
                 # rename argument -> par to be compatible with other optimizer
                 # outputs (alternatively, create optimizer aware downstream)
                 fit = rename(fit, c(argument = 'par'))
                 
                 # construct a result message
                 msg = paste(sprintf('converged: %s', fit$converged), 
                             sprintf('bounded: %s', trust.bounded),
                             sprintf('iter: %d', fit$iterations),
                             sprintf('value: %g', fit$value),
                             sep = '; ')
                 fit$message = msg
                 
                 # set fitted parameters on input model
                 MODEL$q = fit$par
                 
                 fit
               },
               hybrid = {
                 # perform an nlminb optimization to seed initial values for
                 # bounded trust optimization
                 
                 ## WARNING: THIS USES RECURSION
                 
                 # copy the input model for fits
                 M0 = deepcopy(MODEL)
                 M1 = deepcopy(MODEL)
                 
                 f0 = ModelObjectFit(data, M0, weightFun = weightFun, optimizer='nlminb', control = control)
                 
                 # propagate fitted nlminb parameters as initial guess for trust
                 M1$setP(p0 = f0$par)
                 
                 f1 = try(ModelObjectFit(data, M1, weightFun = weightFun, optimizer='trust', trust.bounded = trust.bounded), silent=T)
                 # if bounded trust fails, do an unbounded trust
                 if (inherits(f1, 'try-error')) {
                   f1 = ModelObjectFit(data, M1, weightFun = weightFun, optimizer='trust', trust.bounded = F)
                 }
                 
                 fit = f1
                 fit$message = c(nlminb=f0$message, trust=f1$message)
                 
                 # set fitted parameters on input model
                 MODEL$q = fit$par
                 
                 fit
               },
               constrOptim = {
                 # not implemented yet
               })
  
  # append fitted data to the fit object
  fit$DATA = data
  
  # append residuals to the fit object
  fit$residuals = resFun(fit$par, data)
  
  # append model definition to fit object
  fit$MODEL = MODEL
  
  # append optimizer name to fit object
  fit$optimizer = optimizer
  
  return(fit)
}

#' Generic model fit predictor
#' 
#' Predicts a fitted model over the x-values provided in the data.frame or list \code{data}
#' 
#' @param fit A \code{fit} object as returned by \code{ModelFit} which is expected
#'    to have an element of \code{MODEL$modelFun}
#' 
#' @return
#' An \code{xy.coords} compatible list object of the fitted model
#' 
#' @export
ModelObjectPredict = function(fit, data=NULL) {
  'predicts model from fit'
  
  if (is.null(data)) {
    data = fit$DATA
  }
  
  return(list(x=data$x, y=fit$MODEL$value(as.list(fit$par), data)))
}