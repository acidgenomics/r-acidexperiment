#' @inherit AcidGenomes::Ensembl2Ncbi
#' @name Ensembl2Ncbi
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
#' x <- Ensembl2Ncbi(object)
#' print(x)
NULL



## Updated 2023-03-01.
`Ensembl2Ncbi,RSE` <- # nolint
    function(object, ...) {
        Ensembl2Ncbi(rowRanges(object), ...)
    }



#' @rdname Ensembl2Ncbi
#' @export
setMethod(
    f = "Ensembl2Ncbi",
    signature = signature(object = "RangedSummarizedExperiment"),
    definition = `Ensembl2Ncbi,RSE`
)
