#' @name metrics
#' @author Michael Steinbaugh, Rory Kirchner
#' @inherit AcidGenerics::metrics
#' @note Updated 2022-05-04.
#'
#' @inheritParams AcidRoxygen::params
#' @param ... Additional arguments.
#'
#' @details
#' `metrics()` takes data stored in `colData()` and consistently returns a
#' `DFrame` with `sampleName` and `interestingGroups` columns, even when these
#' columns are not defined in `colData()`. This is designed to integrate with
#' plotting functions that use ggplot2 internally.
#'
#' Column names are always returned formatted in strict lower camel case.
#'
#' This function will error intentionally if no numeric columns are defined
#' in `colData()`.
#'
#' @return `DFrame`.
#'
#' @examples
#' data(RangedSummarizedExperiment, package = "AcidTest")
#'
#' ## SummarizedExperiment ====
#' object <- RangedSummarizedExperiment
#' object <- calculateMetrics(object)
#' x <- metrics(object)
#' print(x)
NULL



## Updated 2022-05-04.
`metrics,SE` <- # nolint
    function(object) {
        validObject(object)
        df <- sampleData(object, clean = FALSE)
        assert(identical(
            x = colnames(df),
            y = camelCase(colnames(df), strict = TRUE)
        ))
        sampleCol <- "sampleId"
        assert(areDisjointSets(sampleCol, colnames(df)))
        df <- decode(df)
        df
    }



#' @describeIn metrics Sample-level metrics.
#' @export
setMethod(
    f = "metrics",
    signature = signature(object = "SummarizedExperiment"),
    definition = `metrics,SE`
)
