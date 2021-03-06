% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/AllGenerics.R, R/estimateSizeFactors-methods.R
\name{estimateSizeFactors}
\alias{estimateSizeFactors}
\alias{estimateSizeFactors,SummarizedExperiment-method}
\title{Estimate size factors}
\usage{
estimateSizeFactors(object, ...)

\S4method{estimateSizeFactors}{SummarizedExperiment}(
  object,
  type = c("mean-ratio", "geometric-mean-ratio", "log-geometric-mean-ratio"),
  center = 1L
)
}
\arguments{
\item{object}{Object.}

\item{type}{\code{character(1)}.
Method for estimation:\preformatted{libSize <- colSums(counts(object))
}
\itemize{
\item \code{mean-ratio}:\preformatted{libSize / mean(libSize)
}
\item \code{geometric-mean-ratio}:\preformatted{libSize / geometricMean(libSize)
}
\item \code{mean-geometric-mean-log-total}:\preformatted{log(libSize) / geometricMean(log(libSize))
}
}}

\item{center}{\code{numeric(1)}.
If non-zero, scales all size factors so that the average size factor across
cells is equal to the value defined. Set to \code{0} to disable centering.}

\item{...}{Additional arguments.}
}
\value{
Modified object.
Use \code{sizeFactors()} to access the computed size factor numeric.
}
\description{
Define size factors from the library sizes, and then apply centering at
unity. This ensures that the library size adjustment yields values comparable
to those generated after normalization with other sets of size factors.
}
\details{
Centering of size factors at unity ensures that division by size factors
yields values on the same scale as the raw counts. This is important for the
interpretation of the normalized values, as well as comaprisons between
features normalized with different size factors (e.g., spike-ins).

The estimated size factors computed by this function can be accessed using
the accessor function \code{sizeFactors()}. Alternative library size estimators
can also be supplied using the assignment function \verb{sizeFactors<-()}.
}
\note{
Updated 2021-02-22.
}
\examples{
data(RangedSummarizedExperiment, package = "AcidTest")

## SummarizedExperiment ====
object <- RangedSummarizedExperiment
object <- estimateSizeFactors(object)
sizeFactors(object)
}
\seealso{
DESeq2:
\itemize{
\item \code{DESeq2::estimateSizeFactors()}.
\item \verb{DESeq2::estimateSizeFactorsForMatrix().}
}

scater:
\itemize{
\item \code{scater::librarySizeFactors()}.
\item \code{scater::centreSizeFactors()}.
\item \code{scater::normalizeSCE()}.
}

monocle3:
\itemize{
\item \code{monocle3::estimate_size_factors()}.
\item \code{monocle3:::estimate_sf_sparse()}.
}
}
