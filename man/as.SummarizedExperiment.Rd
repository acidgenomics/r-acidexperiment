% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/AllGenerics.R,
%   R/as.SummarizedExperiment-methods.R
\name{as.SummarizedExperiment}
\alias{as.SummarizedExperiment}
\alias{as.SummarizedExperiment,RangedSummarizedExperiment-method}
\alias{as.SummarizedExperiment,SummarizedExperiment-method}
\title{Coerce to \code{SummarizedExperiment}.}
\usage{
as.SummarizedExperiment(x, ...)

\S4method{as.SummarizedExperiment}{RangedSummarizedExperiment}(x)

\S4method{as.SummarizedExperiment}{SummarizedExperiment}(x)
}
\arguments{
\item{x}{Object.}

\item{...}{Additional arguments.}
}
\value{
\code{SummarizedExperiment}.
}
\description{
Improved S4 methods for reliably coercing objects that extend
\code{RangedSummarizedExperiment} to a standard \code{SummarizedExperiment}, that
doesn't drop \code{rowData()}.
}
\note{
Updated 2022-05-04.
}
\examples{
suppressPackageStartupMessages(library(SummarizedExperiment))
data(RangedSummarizedExperiment, package = "AcidTest")

## RangedSummarizedExperiment ===
## Current S4 coercion drops row metadata.
names(metadata(rowRanges(RangedSummarizedExperiment)))
se <- as(RangedSummarizedExperiment, "SummarizedExperiment")
names(metadata(rowData(se)))
## Our alternate S3 method preserves the metadata.
se <- as.SummarizedExperiment(RangedSummarizedExperiment)
names(metadata(rowData(se)))
}
