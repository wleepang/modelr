#' Mix (combine) ModelObjects
#' 
#' @param ... list of ModelObjects
#' @param op operator by which the models are mixed. Default is \code{`+`}.
#' 
#' @return
#' A new ModelObject instance that is the combination of the inputs
#' 
#' @export
mix = function(..., op='+') {
  mol = list(...)
  
  if (length(mol) < 2) {
    return(mol[[1]])
    
  } else {
    mo = Reduce(function(x, y) {
      expr = expression()
      expr[[1]] = parse(text=paste(as.character(x$expr), as.character(y$expr), sep = op))[[1]]
      
      ModelObject(
        name = paste(x$name, y$name, sep = op),
        expr = expr,
        P = list(p0=c(x$P$p0, y$P$p0),
                 lb=c(x$P$lb, y$P$lb),
                 ub=c(x$P$ub, y$P$ub))
      )
    }, mol)
    
    return(mo)
  }
}

#' Wrap ModelObjects with transformation functions (NSE)
#' 
#' @param ... list of ModelObjects
#' @param fun function with which models are wrapped. Default is \code{identity}
#' 
#' @return
#' A list of ModelObjects with names and expressions wrapped with the function specified in \code{fun}.
#' 
#' @export
wrap = function(..., fun=identity) {
  mol = list(...)
  fun = deparse(substitute(fun))
  
  add_wrap = function(x) {
    expr = expression()
    expr[[1]] = parse(text=paste0(fun, '(', as.character(x$expr), ')'))[[1]]
    
    ModelObject(
      name = paste0(fun, '(', x$name, ')'),
      expr = expr,
      P = x$P
    )
  }
  
  mol = lapply(mol, add_wrap)
  
  if (length(mol) == 1) {
    return(mol[[1]])
  }
  
  return(mol)
}

#' Wrap ModelObjects with transformation functions
#' 
#' @param ... list of ModelObjects
#' @param fun function name as a character string with which models are wrapped. Default is \code{identity}
#' 
#' @return
#' A list of ModelObjects with names and expressions wrapped with the function specified in \code{fun}.
#' 
#' @export
wrap_ = function(..., fun='identity') {
  mol = list(...)
  
  add_wrap = function(x) {
    expr = expression()
    expr[[1]] = parse(text=paste0(fun, '(', as.character(x$expr), ')'))[[1]]
    
    ModelObject(
      name = paste0(fun, '(', x$name, ')'),
      expr = expr,
      P = x$P
    )
  }
  
  mol = lapply(mol, add_wrap)
  
  if (length(mol) == 1) {
    return(mol[[1]])
  }
  
  return(mol)
}

#' Deep Copy a ModelObject
#' 
#' @param mo a ModelObject
#' 
#' @return
#' A copy of the ModelObject \code{mo} that *should* be a separate object from
#' the original
#' 
#' @export
deepcopy = function(mo) {
  cp = ModelObject(
    name = mo$name,
    expr = mo$expr,
    P = mo$P
  )
  return(cp)
}