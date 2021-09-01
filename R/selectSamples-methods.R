#' @name selectSamples
#' @inherit AcidGenerics::selectSamples
#' @note Updated 2021-02-03.
#'
#' @inheritParams AcidRoxygen::params
#'
#' @examples
#' data(RangedSummarizedExperiment, package = "AcidTest")
#'
#' ## SummarizedExperiment ====
#' object <- RangedSummarizedExperiment
#' sample <- sampleNames(object)[[1L]]
#' print(sample)
#' subset <- selectSamples(object, sampleName = sample)
#' print(subset)
NULL



## Updated 2021-09-01.
`selectSamples,SE` <-  # nolint
    function(object, ...) {
        validObject(object)
        args <- list(...)
        assert(
            all(vapply(
                X = args,
                FUN = is.atomic,
                FUN.VALUE = logical(1L)
            )),
            msg = "Arguments must be atomic."
        )
        ## Match the arguments against the sample metadata.
        sampleData <- sampleData(object)
        assert(isSubset(names(args), colnames(sampleData)))
        ## Obtain the sample identifiers.
        list <- mapply(
            col = names(args),
            arg = args,
            MoreArgs = list(data = sampleData),
            FUN = function(col, arg, data) {
                rownames(data[data[[col]] %in% arg, , drop = FALSE])
            },
            SIMPLIFY = FALSE,
            USE.NAMES = TRUE
        )
        samples <- sort(as.character(Reduce(f = intersect, x = list)))
        assert(hasLength(samples))
        ## Return.
        object[, samples, drop = FALSE]
    }



#' @rdname selectSamples
#' @export
setMethod(
    f = "selectSamples",
    signature = signature("SummarizedExperiment"),
    definition = `selectSamples,SE`
)
