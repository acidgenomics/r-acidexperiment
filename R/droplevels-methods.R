#' Drop unused levels from factors
#'
#' @name droplevels
#' @inherit base::droplevels description
#' @note Updated 2021-02-03.
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
#' S4Vectors::droplevels(object)
NULL



## Updated 2021-02-03.
`droplevels,SummarizedExperiment` <-  # nolint
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
    definition = `droplevels,SummarizedExperiment`
)
