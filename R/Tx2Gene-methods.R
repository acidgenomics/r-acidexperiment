## Updated 2021-01-29.
`Tx2Gene,SummarizedExperiment` <-  # nolint
    function(object) {
        object <- rowData(object, use.names = TRUE)
        Tx2Gene(object)
    }



#' @rdname Tx2Gene
#' @export
setMethod(
    f = "Tx2Gene",
    signature = signature("SummarizedExperiment"),
    definition = `Tx2Gene,SummarizedExperiment`
)
