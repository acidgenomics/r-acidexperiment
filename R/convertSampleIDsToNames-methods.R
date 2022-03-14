#' @name convertSampleIDsToNames
#' @inherit AcidGenerics::convertSampleIDsToNames
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
#' convertSampleIDsToNames(object)
NULL



## Updated 2021-02-02.
`convertSampleIDsToNames,SE` <- # nolint
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
            alertWarning("Returning object with sample names unmodified.")
            return(object)
        }
        colnames <- as.character(sampleNames)
        assert(hasNoDuplicates(colnames))
        colnames(object) <- colnames
        ## Note that we need to allow invalid dimnames to pass through here,
        ## so don't run validity checks.
        object
    }



#' @rdname convertSampleIDsToNames
#' @export
setMethod(
    f = "convertSampleIDsToNames",
    signature = signature(object = "SummarizedExperiment"),
    definition = `convertSampleIDsToNames,SE`
)
