#' @name GeneToSymbol
#' @inherit AcidGenomes::GeneToSymbol
#' @note Updated 2021-08-09.
#'
#' @param ... Additional arguments.
#'
#' @examples
#' data(RangedSummarizedExperiment, package = "AcidTest")
#'
#' ## SummarizedExperiment ====
#' object <- RangedSummarizedExperiment
#' x <- GeneToSymbol(object)
#' print(x)
NULL



## Updated 2021-08-10.
`GeneToSymbol,SE` <- # nolint
    function(object, ...) {
        object <- as.SummarizedExperiment(object)
        df <- rowData(object)
        rownames(df) <- rownames(object)
        GeneToSymbol(df, ...)
    }



#' @rdname GeneToSymbol
#' @export
setMethod(
    f = "GeneToSymbol",
    signature = signature(object = "SummarizedExperiment"),
    definition = `GeneToSymbol,SE`
)
