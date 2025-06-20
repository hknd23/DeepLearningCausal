% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plots.R
\name{plot.metalearner_deepneural}
\alias{plot.metalearner_deepneural}
\title{plot.metalearner_deepneural}
\usage{
\method{plot}{metalearner_deepneural}(x, ..., conf_level = 0.95, type = "CATEs")
}
\arguments{
\item{x}{\code{metalearner_deepneural} model object.}

\item{...}{Additional arguments}

\item{conf_level}{numeric value for confidence level. Defaults to 0.95.}

\item{type}{"CATEs" or "predict".}
}
\value{
\code{ggplot} object.
}
\description{
Uses \code{plot()} to generate histogram of ditribution of CATEs or predicted
outcomes from  \code{metalearner_deepneural}
}
