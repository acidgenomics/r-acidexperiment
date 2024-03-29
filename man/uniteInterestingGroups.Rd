% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/AllGenerics.R,
%   R/uniteInterestingGroups-methods.R
\name{uniteInterestingGroups}
\alias{uniteInterestingGroups}
\alias{uniteInterestingGroups,DFrame-method}
\title{Unite interesting groups into a single column}
\usage{
uniteInterestingGroups(object, ...)

\S4method{uniteInterestingGroups}{DFrame}(object, interestingGroups)
}
\arguments{
\item{object}{Object.}

\item{interestingGroups}{\code{character}.
Groups of interest to use for visualization.
Corresponds to factors describing the columns of the object.}

\item{...}{Additional arguments.}
}
\value{
Modified object.
Now contains an \code{interestingGroups} column.
}
\description{
Create a single interesting groups column (\code{interestingGroups}) used for
coloring in plots. When multiple interesting groups are present, unite into a
single column, delimited by a colon.
}
\note{
Updated 2021-02-03.
}
\examples{
data(RangedSummarizedExperiment, package = "AcidTest")
rse <- RangedSummarizedExperiment

## DFrame ====
object <- rse
x <- uniteInterestingGroups(
    object = sampleData(object),
    interestingGroups = interestingGroups(object)
)
print(x)
}
