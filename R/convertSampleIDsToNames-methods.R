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



## NULL assignment into a column name doesn't work for DataFrame class.
## You can see this cryptic error on some R installations:
##
## nolint start
## > colData(object)[["sampleName"]] <- NULL
## Error in replaceROWS(x, if (missing(i)) nsbs else i, value) :
##   appending gaps is not supported
## nolint end
##
## Updated 2021-02-02.
`convertSampleIDsToNames,SE` <-  # nolint
    function(object) {
        validObject(object)
        sampleNames <- sampleNames(object)
        if (
            identical(as.character(sampleNames), colnames(object)) ||
            !identical(names(sampleNames), colnames(object))
        ) {
            ## FIXME coverage start
            alertWarning("Returning object with sample names unmodified.")
            return(object)
            ## FIXME coverage end
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
    signature = signature("SummarizedExperiment"),
    definition = `convertSampleIDsToNames,SE`
)
