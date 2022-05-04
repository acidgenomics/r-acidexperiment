#' Coerce to `SummarizedExperiment`.
#'
#' Improved S4 methods for reliably coercing objects that extend
#' `RangedSummarizedExperiment` to a standard `SummarizedExperiment`, that
#' doesn't drop `rowData()`.
#'
#' @name as.SummarizedExperiment
#' @note Updated 2022-05-04.
#'
#' @inheritParams AcidRoxygen::params
#' @param ... Additional arguments.
#'
#' @return `SummarizedExperiment`.
#'
#' @examples
#' suppressPackageStartupMessages(library(SummarizedExperiment))
#' data(RangedSummarizedExperiment, package = "AcidTest")
#'
#' ## RangedSummarizedExperiment ===
#' ## Current S4 coercion drops row metadata.
#' names(metadata(rowRanges(RangedSummarizedExperiment)))
#' se <- as(RangedSummarizedExperiment, "SummarizedExperiment")
#' names(metadata(rowData(se)))
#' ## Our alternate S3 method preserves the metadata.
#' se <- as.SummarizedExperiment(RangedSummarizedExperiment)
#' names(metadata(rowData(se)))
NULL



## Updated 2019-08-23.
`as.SummarizedExperiment,SE` <- # nolint
    function(x) {
        rowMeta <- metadata(rowData(x))
        x <- as(x, "SummarizedExperiment")
        metadata(rowData(x)) <- rowMeta
        x
    }



## Updated 2019-08-23.
`as.SummarizedExperiment,RSE` <- # nolint
    function(x) {
        rowMeta <- metadata(rowRanges(x))
        x <- as(x, "RangedSummarizedExperiment")
        x <- as(x, "SummarizedExperiment")
        metadata(rowData(x)) <- rowMeta
        x
    }



#' @rdname as.SummarizedExperiment
#' @export
setMethod(
    f = "as.SummarizedExperiment",
    signature = signature(x = "RangedSummarizedExperiment"),
    definition = `as.SummarizedExperiment,RSE`
)

#' @rdname as.SummarizedExperiment
#' @export
setMethod(
    f = "as.SummarizedExperiment",
    signature = signature(x = "SummarizedExperiment"),
    definition = `as.SummarizedExperiment,SE`
)
