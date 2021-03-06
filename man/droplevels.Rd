% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/AllGenerics.R, R/droplevels-methods.R
\name{droplevels}
\alias{droplevels}
\alias{droplevels,SummarizedExperiment-method}
\title{Drop unused levels from factors}
\usage{
droplevels(x, ...)

\S4method{droplevels}{SummarizedExperiment}(x)
}
\arguments{
\item{x}{Object.}

\item{...}{Additional arguments.}
}
\value{
Modified object.
}
\description{
Drop unused levels from factors
}
\note{
Updated 2021-06-04.
}
\examples{
data(RangedSummarizedExperiment, package = "AcidTest")

## SummarizedExperiment ====
object <- RangedSummarizedExperiment
droplevels(object)
}
