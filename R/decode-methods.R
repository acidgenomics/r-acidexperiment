#' Decode column data that uses run-length encoding
#'
#' @name decode
#' @note Updated 2021-06-04.
#'
#' @inheritParams AcidRoxygen::params
#' @param ... Additional arguments.
#'
#' @return Modified object.
#' Columns will be decoded and no longer `Rle` class.
#'
#' @seealso `S4Vectors::decode()`.
#'
#' @return Modified object.
#'
#' @examples
#' data(RangedSummarizedExperiment, package = "AcidTest")
#'
#' ## SummarizedExperiment ====
#' ## This works on rowData and colData.
#' object <- RangedSummarizedExperiment
#' object <- decode(object)
#' lapply(SummarizedExperiment::rowData(object), class)
NULL



## Updated 2019-07-20.
`decode,SE` <-  # nolint
    function(x) {
        validObject(x)
        if (!is.null(rowData(x))) {
            rowData(x) <- decode(rowData(x))
        }
        if (!is.null(colData(x))) {
            colData(x) <- decode(colData(x))
        }
        validObject(x)
        x
    }



#' @rdname decode
#' @export
setMethod(
    f = "decode",
    signature = signature("SummarizedExperiment"),
    definition = `decode,SE`
)
