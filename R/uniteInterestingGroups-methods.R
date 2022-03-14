#' @name uniteInterestingGroups
#' @inherit AcidGenerics::uniteInterestingGroups
#' @note Updated 2021-02-03.
#'
#' @inheritParams AcidRoxygen::params
#' @param ... Additional arguments.
#'
#' @examples
#' data(RangedSummarizedExperiment, package = "AcidTest")
#' rse <- RangedSummarizedExperiment
#'
#' ## DataFrame ====
#' object <- rse
#' x <- uniteInterestingGroups(
#'     object = sampleData(object),
#'     interestingGroups = interestingGroups(object)
#' )
#' print(x)
NULL



## Updated 2021-10-13.
`uniteInterestingGroups,DataFrame` <- # nolint
    function(object, interestingGroups) {
        assert(
            isCharacter(interestingGroups),
            isSubset(interestingGroups, colnames(object))
        )
        if (isScalar(interestingGroups)) {
            ## This will retain the factor levels, if they're not alphabetical.
            value <- object[[interestingGroups]]
        } else {
            ## Subset to get only the columns of interest.
            data <- object[, interestingGroups, drop = FALSE]
            ## This approach will return numerics for `DataFrame` class, so
            ## coercing columns to data.frame.
            value <- apply(
                X = as.data.frame(data),
                MARGIN = 1L,
                FUN = paste,
                collapse = ":"
            )
            value <- as.factor(value)
            assert(identical(rownames(object), names(value)))
        }
        object[["interestingGroups"]] <- unname(value)
        object
    }



#' @rdname uniteInterestingGroups
#' @export
setMethod(
    f = "uniteInterestingGroups",
    signature = signature(object = "DataFrame"),
    definition = `uniteInterestingGroups,DataFrame`
)
