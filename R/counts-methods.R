#' Counts
#'
#' Count matrix.
#'
#' @note For a `SummarizedExperiment` object, `"counts"` must be explicitly
#'   defined in `assayNames`.
#'
#' @name counts
#' @aliases counts<-
#' @note Updated 2019-12-04.
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
#' x <- counts(object)
#' summary(x)
NULL



## Updated 2019-08-06.
`counts,SE` <-  # nolint
    function(object) {
        validObject(object)
        assay(object, i = "counts")
    }



## Updated 2019-08-06.
`counts<-,SE,matrix` <-  # nolint
    function(object, value) {
        assert(
            all(!is.na(value)),
            all(is.finite(value)),
            all(value >= 0L)
        )
        assay(object, i = "counts") <- value
        validObject(object)
        object
    }



## Updated 2019-08-06.
`counts<-,SE,Matrix` <-  # nolint
    `counts<-,SE,matrix`



#' @rdname counts
#' @export
setMethod(
    f = "counts",
    signature = signature("SummarizedExperiment"),
    definition = `counts,SE`
)



#' @rdname counts
#' @export
setReplaceMethod(
    f = "counts",
    signature = signature(
        object = "SummarizedExperiment",
        value = "Matrix"
    ),
    definition = `counts<-,SE,Matrix`
)

#' @rdname counts
#' @export
setReplaceMethod(
    f = "counts",
    signature = signature(
        object = "SummarizedExperiment",
        value = "matrix"
    ),
    definition = `counts<-,SE,matrix`
)
