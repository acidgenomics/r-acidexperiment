#' @name headtail
#' @inherit AcidGenerics::headtail
#' @note Updated 2020-10-07.
#'
#' @inheritParams AcidRoxygen::params
#' @param n `integer(1)`.
#'   Positive integer denoting the number of first and last items to include.
#' @param ... Additional arguments.
#'
#' @examples
#' data(RangedSummarizedExperiment, package = "AcidTest")
#'
#' ## SummarizedExperiment ====
#' object <- RangedSummarizedExperiment
#' headtail(object)
NULL



## Updated 2021-10-13.
`headtail,DFrame` <-  # nolint
    methodFunction(
        f = "headtail",
        signature = "data.frame",
        package = "AcidBase"
    )



## Updated 2020-10-07.
`headtail,Matrix` <-  # nolint
    methodFunction(
        f = "headtail",
        signature = "matrix",
        package = "AcidBase"
    )



## FIXME Rework the formals approach here.

## Updated 2021-10-13.
`headtail,GRanges` <-  # nolint
    function() {
        headtail(x = as(x, "data.frame"), n = n)
    }

## FIXME Rework the formals approach here.
formals(`headtail,GRanges`) <- formals(`headtail,Matrix`)



## Updated 2020-05-11.
`headtail,SE` <-  # nolint
    function() {
        headtail(x = assay(x), n = n)
    }

## FIXME Rework the formals approach here.
formals(`headtail,SE`) <- formals(`headtail,Matrix`)



#' @describeIn headtail Same method as `data.frame`.
#' @export
setMethod(
    f = "headtail",
    signature = signature(x = "DFrame"),
    definition = `headtail,DFrame`
)

#' @describeIn headtail Summarize the ranges.
#' @export
setMethod(
    f = "headtail",
    signature = signature(x = "GRanges"),
    definition = `headtail,GRanges`
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
