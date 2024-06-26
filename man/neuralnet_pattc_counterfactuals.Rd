% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/patt_neural.R
\name{neuralnet_pattc_counterfactuals}
\alias{neuralnet_pattc_counterfactuals}
\title{Assess Population Data counterfactuals}
\usage{
neuralnet_pattc_counterfactuals(
  pop.data,
  neuralnet.response.mod,
  ID = NULL,
  cluster = NULL,
  binary.outcome = FALSE
)
}
\arguments{
\item{pop.data}{population data.}

\item{neuralnet.response.mod}{trained model from.
\code{neuralnet_response_model}.}

\item{ID}{string for identifier variable.}

\item{cluster}{string for clustering variable (currently unused).}

\item{binary.outcome}{logical specifying predicted outcome variable will take
binary values or proportions.}
}
\value{
\code{data.frame} of predicted outcomes of response variable from
counterfactuals.
}
\description{
Create counterfactual datasets in the population for compliers and
noncompliers. Then predict potential outcomes using trained model from
\code{neuralnet_response_model}.
}
