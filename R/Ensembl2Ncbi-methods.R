#' @inherit AcidGenomes::Ensembl2Ncbi
#' @name Ensembl2Ncbi
#' @note Updated 2023-03-01.
#'
#' @param ... Additional arguments.
#'
#' @examples
#' data(RangedSummarizedExperiment, package = "AcidTest")
#' rse <- RangedSummarizedExperiment
#'
#' ## SummarizedExperiment ====
#' x <- Ensembl2Ncbi(rse)
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
