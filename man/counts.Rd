% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/AllGenerics.R, R/counts-methods.R
\name{counts}
\alias{counts}
\alias{counts<-}
\alias{counts,SummarizedExperiment-method}
\alias{counts<-,SummarizedExperiment,matrix-method}
\alias{counts<-,SummarizedExperiment,Matrix-method}
\title{Counts}
\usage{
counts(object, ...)

counts(object, ...) <- value

\S4method{counts}{SummarizedExperiment}(object)

\S4method{counts}{SummarizedExperiment,matrix}(object) <- value

\S4method{counts}{SummarizedExperiment,Matrix}(object) <- value
}
\arguments{
\item{object}{Object.}

\item{value}{Value to assign.}

\item{...}{Additional arguments.}
}
\value{
Matrix.
Typically \code{matrix} or \code{Matrix} class.
}
\description{
Count matrix.
}
\note{
For a \code{SummarizedExperiment} object, \code{"counts"} must be explicitly
defined in \code{assayNames}.

Updated 2019-12-04.
}
\examples{
data(RangedSummarizedExperiment, package = "AcidTest")

## SummarizedExperiment ====
object <- RangedSummarizedExperiment
x <- counts(object)
summary(x)
}
