#' Drop unused levels from factors
#'
#' @name droplevels2
#' @note Updated 2022-04-25.
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
#' droplevels2(object)
NULL



## Updated 2021-02-03.
`droplevels,SE` <- # nolint
    function(x) {
        if (hasCols(rowData(x))) {
            rowData(x) <- droplevels(rowData(x))
        }
        if (hasCols(colData(x))) {
            colData(x) <- droplevels(colData(x))
        }
        x
    }



#' @rdname droplevels2
setMethod(
    f = "droplevels2",
    signature = signature(x = "SummarizedExperiment"),
    definition = `droplevels,SE`
)
