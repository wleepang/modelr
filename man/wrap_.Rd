% Generated by roxygen2 (4.1.0): do not edit by hand
% Please edit documentation in R/ops.R
\name{wrap_}
\alias{wrap_}
\title{Wrap ModelObjects with transformation functions}
\usage{
wrap_(..., fun = "identity")
}
\arguments{
\item{...}{list of ModelObjects}

\item{fun}{function name as a character string with which models are wrapped. Default is \code{identity}}
}
\value{
A list of ModelObjects with names and expressions wrapped with the function specified in \code{fun}.
}
\description{
Wrap ModelObjects with transformation functions
}

