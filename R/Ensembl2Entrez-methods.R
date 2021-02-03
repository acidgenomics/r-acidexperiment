#' @inherit AcidGenomes::Ensembl2Entrez title description return
#' @name Ensembl2Entrez
#' @note Updated 2021-02-02.
#'
#' @inheritParams AcidRoxygen::params
#'
#' @examples
#' data(RangedSummarizedExperiment, package = "AcidTest")
#' rse <- RangedSummarizedExperiment
#'
#' ## SummarizedExperiment ====
#' x <- Ensembl2Entrez(rse)
#' print(x)
NULL



## Updated 2021-02-02.
`Ensembl2Entrez,RSE` <-  # nolint
    function(object, format) {
        Ensembl2Entrez(
            object = rowRanges(object),
            format = match.arg(format)
        )
    }

formals(`Ensembl2Entrez,RSE`) <-
    methodFormals(
        f = "Ensembl2Entrez",
        signature = "GRanges",
        package = "AcidGenomes"
    )



#' @rdname Ensembl2Entrez
#' @export
setMethod(
    f = "Ensembl2Entrez",
    signature = signature("RangedSummarizedExperiment"),
    definition = `Ensembl2Entrez,RSE`
)
