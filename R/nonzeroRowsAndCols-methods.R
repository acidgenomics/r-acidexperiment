#' @name nonzeroRowsAndCols
#' @inherit AcidGenerics::nonzeroRowsAndCols
#' @note Updated 2021-02-02.
#'
#' @inheritParams AcidRoxygen::params
#' @param ... Additional arguments.
#'
#' @examples
#' data(RangedSummarizedExperiment, package = "AcidTest")
#'
#' ## SummarizedExperiment ====
#' object <- RangedSummarizedExperiment
#' dim(object)
#' x <- nonzeroRowsAndCols(object)
#' dim(x)
NULL



## Updated 2022-05-04.
`nonzeroRowsAndCols,matrix` <- # nolint
    function(object, quiet = FALSE) {
        assert(isFlag(quiet))
        originalDim <- dim(object)
        nzrows <- rowSums(object) > 0L
        nzcols <- colSums(object) > 0L
        object <- object[nzrows, nzcols, drop = FALSE]
        dim <- dim(object)
        if (
            !identical(dim, originalDim) &&
                isFALSE(quiet)
        ) {
            msg <- "Filtered zero count rows and columns:\n"
            ## Rows.
            msg <- paste0(
                msg,
                sprintf(
                    fmt = "  - %d / %d %s",
                    dim[[1L]],
                    originalDim[[1L]],
                    ngettext(
                        n = dim[[1L]],
                        msg1 = "row",
                        msg2 = "rows"
                    )
                )
            )
            if (requireNamespace("scales", quietly = TRUE)) {
                msg <- paste0(
                    msg,
                    sprintf(
                        fmt = " (%s)",
                        scales::percent(dim[[1L]] / originalDim[[1L]])
                    )
                )
            }
            msg <- paste0(msg, "\n")
            ## Columns.
            msg <- paste0(
                msg,
                sprintf(
                    fmt = "  - %d / %d %s",
                    dim[[2L]],
                    originalDim[[2L]],
                    ngettext(
                        n = dim[[2L]],
                        msg1 = "column",
                        msg2 = "columns"
                    )
                )
            )
            if (requireNamespace("scales", quietly = TRUE)) {
                msg <- paste0(
                    msg,
                    sprintf(
                        fmt = " (%s)",
                        scales::percent(dim[[2L]] / originalDim[[2L]])
                    )
                )
            }
            alertInfo(msg)
        }
        object
    }



## Updated 2021-02-22.
`nonzeroRowsAndCols,Matrix` <- # nolint
    `nonzeroRowsAndCols,matrix`



## Updated 2022-05-04.
`nonzeroRowsAndCols,SE` <- # nolint
    function(object, assay = 1L, quiet = FALSE) {
        assay <- assay(object, i = assay)
        assay <- nonzeroRowsAndCols(object = assay, quiet = quiet)
        object <- object[rownames(assay), colnames(assay)]
        object
    }



#' @rdname nonzeroRowsAndCols
#' @export
setMethod(
    f = "nonzeroRowsAndCols",
    signature = signature(object = "Matrix"),
    definition = `nonzeroRowsAndCols,Matrix`
)

#' @rdname nonzeroRowsAndCols
#' @export
setMethod(
    f = "nonzeroRowsAndCols",
    signature = signature(object = "SummarizedExperiment"),
    definition = `nonzeroRowsAndCols,SE`
)

#' @rdname nonzeroRowsAndCols
#' @export
setMethod(
    f = "nonzeroRowsAndCols",
    signature = signature(object = "matrix"),
    definition = `nonzeroRowsAndCols,matrix`
)
