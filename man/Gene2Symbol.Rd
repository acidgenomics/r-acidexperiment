% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/AllGenerics.R, R/Gene2Symbol-methods.R
\name{Gene2Symbol}
\alias{Gene2Symbol}
\alias{Gene2Symbol,SummarizedExperiment-method}
\title{Gene-to-symbol mappings}
\usage{
Gene2Symbol(object, ...)

\S4method{Gene2Symbol}{SummarizedExperiment}(object, format = c("makeUnique", "unmodified", "1:1"))
}
\arguments{
\item{object}{Object.}

\item{format}{\code{character(1)}.
Formatting method to apply:
\itemize{
\item \code{"makeUnique"}: \emph{Recommended.} Apply \code{\link[base:make.unique]{make.unique()}}
to the \code{geneName} column. Gene symbols are made unique, while the gene
identifiers remain unmodified.
\item \code{"unmodified"}: Return \code{geneId} and \code{geneName} columns unmodified, in
long format.
\item \code{"1:1"}: For gene symbols that map to multiple gene identifiers, select
}}

\item{...}{Additional arguments.}
}
\value{
\code{Gene2Symbol}.
}
\description{
Gene-to-symbol mappings
}
\note{
Updated 2021-02-01.
}
\examples{
data(RangedSummarizedExperiment, package = "AcidTest")

## SummarizedExperiment ====
object <- RangedSummarizedExperiment
x <- Gene2Symbol(object)
print(x)
}
\seealso{
\code{\link[AcidGenomes:makeGene2Symbol]{makeGene2Symbol()}}.
}
