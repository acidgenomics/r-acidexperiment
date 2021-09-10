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



## Updated 2020-10-07.
`headtail,matrix` <-  # nolint
    methodFunction(
        f = "headtail",
        signature = "matrix",
        package = "AcidBase"
    )



## Updated 2020-10-07.
`headtail,DataFrame` <-  # nolint
    getMethod(
        f = "headtail",
        signature = "data.frame",
        where = asNamespace("AcidBase")
    )



## Updated 2020-10-07.
`headtail,Matrix` <-  # nolint
    `headtail,matrix`



## Updated 2020-05-11.
`headtail,GRanges` <-  # nolint
    function() {
        headtail(x = as(x, "data.frame"), n = n)
    }

formals(`headtail,GRanges`) <- formals(`headtail,matrix`)



## Updated 2020-05-11.
`headtail,SE` <-  # nolint
    function() {
        headtail(x = assay(x), n = n)
    }

formals(`headtail,SE`) <- formals(`headtail,matrix`)



#' @describeIn headtail Same method as `data.frame`.
#' @export
setMethod(
    f = "headtail",
    signature = signature("DataFrame"),
    definition = `headtail,DataFrame`
)

#' @describeIn headtail Summarize the ranges.
#' @export
setMethod(
    f = "headtail",
    signature = signature("GRanges"),
    definition = `headtail,GRanges`
)

#' @describeIn headtail Same method as `matrix`.
#' @export
setMethod(
    f = "headtail",
    signature = signature("Matrix"),
    definition = `headtail,Matrix`
)

#' @describeIn headtail Summarize the primary `assay`.
#' @export
setMethod(
    f = "headtail",
    signature = signature("SummarizedExperiment"),
    definition = `headtail,SE`
)
