#' @inherit AcidGenomes::EnsemblToNcbi
#' @name EnsemblToNcbi
#' @note Updated 2023-04-27.
#'
#' @param ... Additional arguments.
#'
#' @examples
#' suppressPackageStartupMessages(library(SummarizedExperiment))
#' data(RangedSummarizedExperiment, package = "AcidTest")
#'
#' ## SummarizedExperiment ====
#' object <- RangedSummarizedExperiment
#' rowRanges(object) <- as(rowRanges(object), "EnsemblGenes")
#' x <- EnsemblToNcbi(object)
#' print(x)
NULL



## Updated 2023-03-01.
`EnsemblToNcbi,RSE` <- # nolint
    function(object, ...) {
        EnsemblToNcbi(rowRanges(object), ...)
    }



#' @rdname EnsemblToNcbi
#' @export
setMethod(
    f = "EnsemblToNcbi",
    signature = signature(object = "RangedSummarizedExperiment"),
    definition = `EnsemblToNcbi,RSE`
)
