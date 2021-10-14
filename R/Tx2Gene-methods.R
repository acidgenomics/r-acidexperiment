#' @name Tx2Gene
#' @inherit AcidGenomes::Tx2Gene
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
#' t2g <- Tx2Gene(object)
#' print(t2g)
NULL



## Updated 2021-01-29.
`Tx2Gene,SE` <-  # nolint
    function(object) {
        object <- rowData(object, use.names = TRUE)
        Tx2Gene(object)
    }



#' @rdname Tx2Gene
#' @export
setMethod(
    f = "Tx2Gene",
    signature = signature(object = "SummarizedExperiment"),
    definition = `Tx2Gene,SE`
)
