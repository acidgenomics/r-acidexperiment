#' @name integerCounts
#' @inherit AcidGenerics::integerCounts
#' @note Updated 2021-09-11.
#'
#' @note For a `SummarizedExperiment` object, `"counts"` must be explicitly
#'   defined in `assayNames`.
#'
#' @inheritParams AcidRoxygen::params
#' @param ... Additional arguments.
#'
#' @return Matrix.
#' Typically `matrix` or `Matrix` class.
#'
#' @examples
#' data(RangedSummarizedExperiment, package = "AcidTest")
#'
#' ## SummarizedExperiment ====
#' object <- RangedSummarizedExperiment
#' x <- integerCounts(object)
#' class(x)
#' is.integer(x)
#' summary(x)
NULL



## This is inspired by approach used internally by DESeq2.
## Updated 2019-12-04.
`integerCounts,matrix` <-  # nolint
    function(object) {
        object <- round(x = object, digits = 0L)
        mode(object) <- "integer"
        object
    }



## Updated 2019-12-04.
`integerCounts,Matrix` <-  # nolint
    function(object) {
        object <- round(x = object, digits = 0L)
        object
    }



## Updated 2021-09-13.
`integerCounts,SE` <-  # nolint
    function(
        object,
        assay = "counts"
    ) {
        validObject(object)
        assert(isScalar(assay))
        assay <- assay(object, i = assay)
        integerCounts(assay)
    }



#' @rdname integerCounts
#' @export
setMethod(
    f = "integerCounts",
    signature = signature(object = "Matrix"),
    definition = `integerCounts,Matrix`
)

#' @rdname integerCounts
#' @export
setMethod(
    f = "integerCounts",
    signature = signature(object = "SummarizedExperiment"),
    definition = `integerCounts,SE`
)

#' @rdname integerCounts
#' @export
setMethod(
    f = "integerCounts",
    signature = signature(object = "matrix"),
    definition = `integerCounts,matrix`
)
