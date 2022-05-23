#' Correlation
#'
#' @name correlation
#' @inherit AcidGenerics::correlation
#' @note Updated 2021-06-04.
#'
#' @inheritParams AcidRoxygen::params
#' @param ... Additional arguments.
#'
#' @param i `integer(1)` or `character(1)`.
#' For `SummarizedExperiment`, primary assay.
#'
#' @param j `integer(1)`, `character(1)`, or `NULL`.
#' For `SummarizedExperiment`, optional secondary assay.
#' If `NULL`, calculates correlation matrix only on the primary assay.
#'
#' @param method `character(1)`.
#' Which correlation coefficient (or covariance) is to be computed.
#' See `stats::cor` documentation for details.
#'
#' @examples
#' data(correlation, package = "AcidTest")
#' list <- correlation
#'
#' ## vector ====
#' x <- list[["vector_x"]]
#' y <- list[["vector_y"]]
#'
#' head(x)
#' head(y)
#'
#' correlation(x = x, y = y)
#'
#' ## matrix ====
#' x <- list[["matrix_x"]]
#' y <- list[["matrix_y"]]
#'
#' head(x)
#' head(y)
#'
#' stats::cor(x)
#' correlation(x)
#'
#' stats::cor(x = c(x), y = c(y))
#' correlation(x = x, y = y)
#'
#' ## SummarizedExperiment ====
#' x <- list[["SummarizedExperiment_x"]]
#' y <- list[["SummarizedExperiment_y"]]
#'
#' correlation(x = x, i = 1L)
#' correlation(x = x, i = 1L, j = 2L)
#' correlation(x = x, y = y)
NULL



.method <- formals(stats::cor)[["method"]]



## Updated 2021-02-03.
`correlation,numeric,numeric` <- # nolint
    function(x, y, method) {
        assert(
            hasLength(x),
            identical(length(x), length(y)),
            !anyNA(x), !anyNA(y)
        )
        method <- match.arg(method)
        n <- length(x)
        alert(sprintf(
            fmt = "Calculating %s correlation on %d %s.",
            method,
            n,
            ngettext(
                n = n,
                msg1 = "value",
                msg2 = "values"
            )
        ))
        cor(x = x, y = y, method = method)
    }

formals(`correlation,numeric,numeric`)[["method"]] <- # nolint
    .method



## Updated 2021-02-03.
`correlation,matrix,missing` <- # nolint
    function(x, y = NULL, method) {
        assert(!anyNA(x))
        method <- match.arg(method)
        alert(sprintf(
            "Calculating {.val %s} correlation matrix.", method
        ))
        cor(x = x, y = NULL, method = method)
    }

formals(`correlation,matrix,missing`)[["method"]] <- # nolint
    .method



## Updated 2019-11-08.
`correlation,matrix,matrix` <- # nolint
    function(x, y, method) {
        method <- match.arg(method)
        correlation(x = c(x), y = c(y), method = method)
    }

formals(`correlation,matrix,matrix`)[["method"]] <- # nolint
    .method



`correlation,Matrix,missing` <- # nolint
    `correlation,matrix,missing`



`correlation,Matrix,Matrix` <- # nolint
    `correlation,matrix,matrix`



## Updated 2019-11-08.
`correlation,SE,missing` <- # nolint
    function(x, y = NULL,
             i = 1L, j = NULL,
             method) {
        assert(!identical(i, j))
        method <- match.arg(method)
        if (is.null(j)) {
            correlation(
                x = assay(x, i = i),
                method = method
            )
        } else {
            correlation(
                x = assay(x, i = i),
                y = assay(x, i = j),
                method = method
            )
        }
    }

formals(`correlation,SE,missing`)[["method"]] <- # nolint
    .method



## Updated 2019-11-08.
`correlation,SE,SE` <- # nolint
    function(x, y, i = 1L, method) {
        method <- match.arg(method)
        correlation(
            x = assay(x, i = i),
            y = assay(y, i = i),
            method = method
        )
    }

formals(`correlation,SE,SE`)[["method"]] <- # nolint
    .method

rm(.method)



#' @rdname correlation
#' @export
setMethod(
    f = "correlation",
    signature = signature(
        x = "Matrix",
        y = "Matrix"
    ),
    definition = `correlation,Matrix,Matrix`
)

#' @rdname correlation
#' @export
setMethod(
    f = "correlation",
    signature = signature(
        x = "Matrix",
        y = "missingOrNULL"
    ),
    definition = `correlation,Matrix,missing`
)

#' @rdname correlation
#' @export
setMethod(
    f = "correlation",
    signature = signature(
        x = "SummarizedExperiment",
        y = "SummarizedExperiment"
    ),
    definition = `correlation,SE,SE`
)

#' @rdname correlation
#' @export
setMethod(
    f = "correlation",
    signature = signature(
        x = "SummarizedExperiment",
        y = "missingOrNULL"
    ),
    definition = `correlation,SE,missing`
)

#' @rdname correlation
#' @export
setMethod(
    f = "correlation",
    signature = signature(
        x = "matrix",
        y = "matrix"
    ),
    definition = `correlation,matrix,matrix`
)

#' @rdname correlation
#' @export
setMethod(
    f = "correlation",
    signature = signature(
        x = "matrix",
        y = "missingOrNULL"
    ),
    definition = `correlation,matrix,missing`
)

#' @rdname correlation
#' @export
setMethod(
    f = "correlation",
    signature = signature(
        x = "numeric",
        y = "numeric"
    ),
    definition = `correlation,numeric,numeric`
)
