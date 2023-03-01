#' Drop unused levels from factors
#'
#' @name droplevels2
#' @note Updated 2023-03-01.
#'
#' @inheritParams AcidRoxygen::params
#' @param ... Additional arguments.
#'
#' @return Modified object.
#'
#' @examples
#' data(RangedSummarizedExperiment, package = "AcidTest")
#'
#' ## SummarizedExperiment ====
#' object <- RangedSummarizedExperiment
#' object <- droplevels2(object)
NULL



## Updated 2022-04-25.
`droplevels2,SE` <- # nolint
    function(x) {
        if (hasCols(rowData(x))) {
            rowData(x) <- droplevels2(rowData(x))
        }
        if (hasCols(colData(x))) {
            colData(x) <- droplevels2(colData(x))
        }
        x
    }



#' @rdname droplevels2
setMethod(
    f = "droplevels2",
    signature = signature(x = "SummarizedExperiment"),
    definition = `droplevels2,SE`
)
