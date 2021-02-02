## FIXME INHERIT THIS FROM ACIDGENERICS INSTEAD.

#' Decode column data that uses run-length encoding
#'
#' @name decode
#' @inherit S4Vectors::decode description return
#' @note Updated 2021-02-02.
#'
#' @inheritParams AcidRoxygen::params
#' @param ... Additional arguments.
#'
#' @return Modified object.
#' Columns will be decoded and no longer `Rle` class.
#'
#' @seealso [S4Vectors::decode()].
#'
#' @examples
#' data(RangedSummarizedExperiment, package = "AcidTest")
#' rowData <- SummarizedExperiment::rowData
#'
#' ## DataFrame ====
#' df <- rowData(RangedSummarizedExperiment)
#' lapply(df, class)
#' x <- decode(df)
#' lapply(x, class)
#'
#' ## SummarizedExperiment ====
#' ## This works on rowData and colData.
#' x <- decode(RangedSummarizedExperiment)
#' lapply(rowData(x), class)
NULL



## Updated 2019-07-20.
`decode,SummarizedExperiment` <-  # nolint
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
    definition = `decode,SummarizedExperiment`
)
