#' @name TxToGene
#' @inherit AcidGenomes::TxToGene
#' @note Updated 2021-02-03.
#'
#' @inheritParams AcidRoxygen::params
#' @param ... Additional arguments.
#'
#' @examples
#' data(SummarizedExperiment_transcripts, package = "AcidTest")
#'
#' ## SummarizedExperiment ====
#' object <- SummarizedExperiment_transcripts
#' t2g <- TxToGene(object)
#' print(t2g)
NULL



## Updated 2021-01-29.
`TxToGene,SE` <- # nolint
    function(object) {
        object <- rowData(object, use.names = TRUE)
        TxToGene(object)
    }



#' @rdname TxToGene
#' @export
setMethod(
    f = "TxToGene",
    signature = signature(object = "SummarizedExperiment"),
    definition = `TxToGene,SE`
)
