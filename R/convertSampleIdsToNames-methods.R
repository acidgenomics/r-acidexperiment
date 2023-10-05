#' @name convertSampleIdsToNames
#' @inherit AcidGenerics::convertSampleIdsToNames
#' @note Updated 2021-02-02.
#'
#' @inheritParams AcidRoxygen::params
#' @param ... Additional arguments.
#'
#' @examples
#' data(RangedSummarizedExperiment, package = "AcidTest")
#'
#' ## SummarizedExperiment ===
#' object <- RangedSummarizedExperiment
#' convertSampleIdsToNames(object)
NULL



## Updated 2023-10-05.
`convertSampleIdsToNames,SE` <- # nolint
    function(object) {
        validObject(object)
        sampleNames <- sampleNames(object)
        if (
            identical(
                x = as.character(sampleNames),
                y = colnames(object)
            ) ||
                !identical(
                    x = names(sampleNames),
                    y = colnames(object)
                )
        ) {
            return(object)
        }
        colnames <- as.character(sampleNames)
        assert(hasNoDuplicates(colnames))
        colnames(object) <- colnames
        ## Note that we need to allow invalid dimnames to pass through here,
        ## so don't run validity checks.
        object
    }



#' @rdname convertSampleIdsToNames
#' @export
setMethod(
    f = "convertSampleIdsToNames",
    signature = signature(object = "SummarizedExperiment"),
    definition = `convertSampleIdsToNames,SE`
)
