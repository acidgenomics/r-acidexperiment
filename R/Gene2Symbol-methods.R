#' @name Gene2Symbol
#' @inherit AcidGenomes::Gene2Symbol
#' @note Updated 2021-08-09.
#'
#' @param ... Additional arguments.
#'
#' @examples
#' data(RangedSummarizedExperiment, package = "AcidTest")
#'
#' ## SummarizedExperiment ====
#' object <- RangedSummarizedExperiment
#' x <- Gene2Symbol(object)
#' print(x)
NULL



## Updated 2021-08-10.
`Gene2Symbol,SE` <-  # nolint
    function(object, ...) {
        object <- as.SummarizedExperiment(object)
        df <- rowData(object)
        rownames(df) <- rownames(object)
        Gene2Symbol(df, ...)
    }



#' @rdname Gene2Symbol
#' @export
setMethod(
    f = "Gene2Symbol",
    signature = signature("SummarizedExperiment"),
    definition = `Gene2Symbol,SE`
)
