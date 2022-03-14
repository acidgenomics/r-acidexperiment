#' @name headtail
#' @inherit AcidGenerics::headtail
#' @note Updated 2020-10-07.
#'
#' @inheritParams AcidRoxygen::params
#' @param ... Additional arguments.
#'
#' @param n `integer(1)`.
#' Positive integer denoting the number of first and last items to include.
#'
#' @examples
#' data(RangedSummarizedExperiment, package = "AcidTest")
#'
#' ## SummarizedExperiment ====
#' object <- RangedSummarizedExperiment
#' headtail(object)
NULL



## Updated 2021-10-13.
`headtail,DataFrame` <- # nolint
    methodFunction(
        f = "headtail",
        signature = "data.frame",
        package = "AcidBase"
    )



## Updated 2021-10-13.
`headtail,GenomicRanges` <- # nolint
    function() {
        headtail(x = as(x, "data.frame"), n = n)
    }

formals(`headtail,GenomicRanges`) <- formals(`headtail,DataFrame`)



## Updated 2020-10-07.
`headtail,Matrix` <- # nolint
    methodFunction(
        f = "headtail",
        signature = "matrix",
        package = "AcidBase"
    )



## Updated 2020-05-11.
`headtail,SE` <- # nolint
    function() {
        headtail(x = assay(x), n = n)
    }

formals(`headtail,SE`) <- formals(`headtail,DataFrame`)



#' @describeIn headtail Same method as `data.frame`.
#' @export
setMethod(
    f = "headtail",
    signature = signature(x = "DataFrame"),
    definition = `headtail,DataFrame`
)

#' @describeIn headtail Summarize the ranges.
#' @export
setMethod(
    f = "headtail",
    signature = signature(x = "GenomicRanges"),
    definition = `headtail,GenomicRanges`
)

#' @describeIn headtail Same method as `matrix`.
#' @export
setMethod(
    f = "headtail",
    signature = signature(x = "Matrix"),
    definition = `headtail,Matrix`
)

#' @describeIn headtail Summarize the primary `assay`.
#' @export
setMethod(
    f = "headtail",
    signature = signature(x = "SummarizedExperiment"),
    definition = `headtail,SE`
)
