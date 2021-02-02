#' @name encode
#' @inherit AcidGenerics::encode
#' @note Updated 2021-02-02.
#'
#' @inheritParams AcidRoxygen::params
#' @param ... Additional arguments.
#'
#' @seealso [S4Vectors::Rle()].
#'
#' @return Modified object.
#' All `atomic` columns will be encoded to `Rle` S4 class.
#'
#' @examples
#' ## FIXME NEED RSE EXAMPLE HERE.
#'
#' ## DataFrame ====
#' binary <- seq(from = 0L, to = 1L)
#' df <- S4Vectors::DataFrame(
#'     a = rep(x = binary, times = 50L),
#'     b = rep(x = binary, each = 50L)
#' )
#' lapply(df, class)
#' x <- encode(df)
#' lapply(x, class)
NULL



## Updated 2019-07-20.
`encode,SummarizedExperiment` <-  # nolint
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
    signature = signature("SummarizedExperiment"),
    definition = `encode,SummarizedExperiment`
)
