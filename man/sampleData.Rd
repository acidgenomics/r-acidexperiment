% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/AllGenerics.R, R/sampleData-methods.R,
%   R/sampleNames-methods.R
\name{sampleData}
\alias{sampleData}
\alias{sampleData<-}
\alias{sampleData,SummarizedExperiment-method}
\alias{sampleData<-,SummarizedExperiment,DFrame-method}
\alias{sampleNames,SummarizedExperiment-method}
\title{Sample data}
\usage{
sampleData(object, ...)

sampleData(object, ...) <- value

\S4method{sampleData}{SummarizedExperiment}(
  object,
  clean = TRUE,
  ignoreCols = c("^description$", "^genomeBuild$", "^qualityFormat$", "^samRef$")
)

\S4method{sampleData}{SummarizedExperiment,DFrame}(object) <- value

\S4method{sampleNames}{SummarizedExperiment}(object)
}
\arguments{
\item{object}{Object.}

\item{clean}{\code{logical(1)}.
Only return \code{factor} columns. Useful when working with objects that contain
quality control metrics in \code{colData()}. For example, \code{bcbioRNASeq} and
\code{DESeqDataSet} objects often contain additional columns that aren't
informative sample metadata.}

\item{ignoreCols}{\code{character} or \code{NULL}.
Only applies when \code{clean = TRUE}. Additional factor columns defined in
\code{colData} to be ignored as sample-level metadata.
Particularly useful for \code{SingleCellExperiment} objects, where
cell-to-sample mappings are defined using the \code{sampleId} column.}

\item{value}{Value to assign.}

\item{...}{Additional arguments.}
}
\value{
\code{DataFrame}.
}
\description{
Metadata that describes the samples.
}
\details{
All columns defined in \code{colData} of the object must be named in strict
lower camel case, otherwise this function will intentionally error.
}
\note{
Updated 2021-02-22.
}
\section{All supported S4 classes}{


Illegal \code{colData}:
\itemize{
\item \code{interestingGroups}: Generated automatically, based on the criteria
slotted into the object using \code{interestingGroups()}. The function will
error intentionally if this column is manually defined in \code{colData()}.
}

Recommended \code{colData}:
\itemize{
\item \code{sampleName}: Human readable sample names used by basejump plotting
functions in favor of object column names, which should be syntactically
valid (but not always very readable). See
\code{make.names()} for more information on syntactically valid names. Note that
if this column is not defined in the object, it will be returned
automatically by \code{sampleData()}.
}
}

\section{SummarizedExperiment}{


Required \code{colData}:
\itemize{
\item None.
}

Illegal \code{colData}:
\itemize{
\item \code{sampleId}: Redundant; already defined in the object column names.
}
}

\examples{
data(RangedSummarizedExperiment, package = "AcidTest")

## SummarizedExperiment ====
object <- RangedSummarizedExperiment
sampleData(object)

## Assignment support.
sampleData(object)[["batch"]] <- 1L
## `batch` column should be now defined.
sampleData(object)
}
