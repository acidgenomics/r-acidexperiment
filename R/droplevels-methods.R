#' Drop unused levels from factors
#'
#' @name droplevels
#' @note Updated 2021-06-04.
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
#' droplevels(object)
NULL



## Updated 2021-02-03.
`droplevels,SE` <-  # nolint
    function(x) {
        if (hasCols(rowData(x))) {
            rowData(x) <- droplevels(rowData(x))
        }
        if (hasCols(colData(x))) {
            colData(x) <- droplevels(colData(x))
        }
        x
    }



#' @rdname droplevels
setMethod(
    f = "droplevels",
    signature = signature("SummarizedExperiment"),
    definition = `droplevels,SE`
)
