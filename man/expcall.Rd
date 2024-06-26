% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{expcall}
\alias{expcall}
\title{Create list for experimental data}
\usage{
expcall(
  response.formula,
  treat.var,
  compl.var,
  exp.data,
  weights = NULL,
  cluster = NULL,
  ID = NULL
)
}
\arguments{
\item{response.formula}{formula for response equation of binary outcome
variable and covariates}

\item{treat.var}{string for binary treatment variable}

\item{compl.var}{string for complier variable}

\item{exp.data}{\code{data.frame} of experimental variable}

\item{weights}{observation weights}

\item{cluster}{clustering variable}

\item{ID}{identifier variable}
}
\value{
list of processed dataset
}
\description{
create list object of experimental data for easy data processing
}
\keyword{internal}
