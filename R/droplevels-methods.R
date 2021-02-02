#' Drop unused levels from factors
#'
#' @name droplevels
#' @inherit base::droplevels description
#' @note Updated 2020-04-10.
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



## Updated 2021-02-02.
`droplevels,SummarizedExperiment` <-  # nolint
    function(x) {
        rowData <- rowData(x)
        if (hasCols(rowData)) {
            except <- !bapply(decode(rowData), is.factor)
            rowData <- droplevels(rowData, except = except)
            rowData(x) <- rowData
        }
        colData <- colData(x)
        if (hasCols(colData)) {
            except <- !bapply(decode(colData), is.factor)
            colData <- droplevels(colData, except = except)
            colData(x) <- colData
        }
        x
    }



#' @rdname droplevels
setMethod(
    f = "droplevels",
    signature = signature("SummarizedExperiment"),
    definition = `droplevels,SummarizedExperiment`
)
