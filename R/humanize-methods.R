#' @name humanize
#' @inherit AcidGenerics::humanize
#' @note Updated 2021-10-21.
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



## Updated 2021-10-21.
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
