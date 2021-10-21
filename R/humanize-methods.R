## FIXME Think about how we want to handle NA gene symbols in rows here.
## FIXME Make SummarizedExperiment the only exported method. Simplify.



#' @name humanize
#' @inherit AcidGenerics::humanize
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
#' lapply(dimnames(object), head)
#' x <- humanize(object)
#' lapply(dimnames(x), head)
NULL



## Updated 2020-01-20.
`humanize,SE` <-  # nolint
    function(object) {
        to <- object
        to <- convertSampleIDsToNames(to)
        to <- convertGenesToSymbols(to)
        to
    }



#' @rdname humanize
#' @export
setMethod(
    f = "humanize",
    signature = signature(object = "SummarizedExperiment"),
    definition = `humanize,SE`
)
