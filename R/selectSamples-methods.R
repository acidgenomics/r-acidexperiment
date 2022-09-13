## FIXME Unit test input of multiple values.
## FIXME Error if user passes in a value that's not defined.



#' @name selectSamples
#' @inherit AcidGenerics::selectSamples
#' @note Updated 2022-08-18.
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



## FIXME Need to tighten our assert check about acceptable input.
## FIXME Also need to harden to not let the user select based on sampleName
## or interestingGroups...these are dynamic.
## FIXME Alternatively, just extract colData and pick the factor columns.
## We can generalize this to selectColData instead, which is a bit simpler.


## Updated 2022-08-17.
`selectSamples,SE` <- # nolint
    function(object, ...) {
        args <- list(...)
        assert(
            validObject(object),
            ## FIXME Consider hardening for scalars here.
            all(vapply(
                X = args,
                FUN = is.atomic,
                FUN.VALUE = logical(1L)
            )),
            msg = "Arguments must be atomic."
        )
        ## Match the arguments against the sample metadata.
        ## FIXME Rework using colData and factor selection, to keep this faster.
        sampleData <- sampleData(object)
        assert(isSubset(names(args), colnames(sampleData)))
        ## Obtain the sample identifiers.
        ## FIXME Work on improving the variable names here, a bit confusing.
        ## FIXME Need to handle case where input doesn't match anything.
        ## FIXME What about if user passes in 2 values...both MUST match!
        list <- Map(
            col = names(args),
            arg = args,
            MoreArgs = list("data" = sampleData),
            f = function(col, arg, data) {
                rownames(data[data[[col]] %in% arg, , drop = FALSE])
            }
        )
        samples <- sort(as.character(Reduce(f = intersect, x = list)))
        assert(hasLength(samples))
        ## Return.
        out <- object[, samples, drop = FALSE]
        out <- droplevels2(out)
        out
    }



#' @rdname selectSamples
#' @export
setMethod(
    f = "selectSamples",
    signature = signature(object = "SummarizedExperiment"),
    definition = `selectSamples,SE`
)
