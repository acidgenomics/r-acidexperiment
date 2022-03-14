#' @inherit AcidGenomes::Ensembl2Entrez
#' @name Ensembl2Entrez
#' @note Updated 2021-08-10.
#'
#' @param ... Additional arguments.
#'
#' @examples
#' data(RangedSummarizedExperiment, package = "AcidTest")
#' rse <- RangedSummarizedExperiment
#'
#' ## SummarizedExperiment ====
#' x <- Ensembl2Entrez(rse)
#' print(x)
NULL



## Updated 2021-08-10.
`Ensembl2Entrez,RSE` <- # nolint
    function(object, ...) {
        Ensembl2Entrez(rowRanges(object), ...)
    }



#' @rdname Ensembl2Entrez
#' @export
setMethod(
    f = "Ensembl2Entrez",
    signature = signature(object = "RangedSummarizedExperiment"),
    definition = `Ensembl2Entrez,RSE`
)
