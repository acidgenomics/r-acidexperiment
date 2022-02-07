#' @name encode
#' @inherit AcidGenerics::encode
#' @note Updated 2021-02-03.
#'
#' @inheritParams AcidRoxygen::params
#' @param ... Additional arguments.
#'
#' @seealso `S4Vectors::Rle()`.
#'
#' @return Modified object.
#' All `atomic` columns will be encoded to `Rle` S4 class.
#'
#' @examples
#' data(RangedSummarizedExperiment, package = "AcidTest")
#'
#' ## SummarizedExperiment ====
#' ## This works on rowData and colData.
#' object <- RangedSummarizedExperiment
#' object <- encode(object)
#' lapply(
#'     X = SummarizedExperiment::rowData(object),
#'     FUN = class
#' )
NULL



## Updated 2019-07-20.
`encode,SE` <-  # nolint
    function(x) {
        validObject(x)
        if (!is.null(rowData(x))) {
            rowData(x) <- encode(rowData(x))
        }
        if (!is.null(colData(x))) {
            colData(x) <- encode(colData(x))
        }
        validObject(x)
        x
    }



#' @rdname encode
#' @export
setMethod(
    f = "encode",
    signature = signature(x = "SummarizedExperiment"),
    definition = `encode,SE`
)
