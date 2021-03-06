% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/AllGenerics.R, R/geneNames-methods.R
\name{geneNames}
\alias{geneNames}
\alias{geneNames,GRanges-method}
\alias{geneNames,SummarizedExperiment-method}
\title{Gene names}
\usage{
geneNames(object, ...)

\S4method{geneNames}{GRanges}(object)

\S4method{geneNames}{SummarizedExperiment}(object)
}
\arguments{
\item{object}{Object.}

\item{...}{Additional arguments.}
}
\value{
\code{character}.
}
\description{
Convenience function for quickly returning gene names
(a.k.a gene symbols) that map to the stable gene identifiers.
}
\note{
Updated 2019-07-28.
}
\examples{
data(RangedSummarizedExperiment, package = "AcidTest")
rse <- RangedSummarizedExperiment

## SummarizedExperiment ====
head(geneNames(rse))
}
