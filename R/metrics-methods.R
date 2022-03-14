#' @name metrics
#' @author Michael Steinbaugh, Rory Kirchner
#' @inherit AcidGenerics::metrics
#' @note Updated 2021-02-05.
#'
#' @inheritParams AcidRoxygen::params
#' @param ... Additional arguments.
#'
#' @details
#' `metrics()` takes data stored in `colData()` and consistently returns a
#' `tbl_df` or `DataFrame` with `sampleName` and `interestingGroups` columns,
#' even when these columns are not defined in `colData()`. This is designed to
#' integrate with plotting functions that use ggplot2 internally.
#'
#' Column names are always returned formatted in strict lower camel case.
#'
#' This function will error intentionally if no numeric columns are defined
#' in `colData()`.
#'
#' @return Object of class determined by `return` argument.
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



## Updated 2021-10-13.
`metrics,SE` <- # nolint
    function(object, return = c("tbl_df", "DataFrame")) {
        validObject(object)
        return <- match.arg(return)
        data <- sampleData(object, clean = FALSE)
        assert(identical(
            x = colnames(data),
            y = camelCase(colnames(data), strict = TRUE)
        ))
        sampleCol <- "sampleId"
        assert(areDisjointSets(sampleCol, colnames(data)))
        ## Decode columns that contain Rle, if necessary.
        data <- decode(data)
        switch(
            EXPR = return,
            "DataFrame" = data,
            "tbl_df" = as_tibble(data, rownames = sampleCol)
        )
    }



#' @describeIn metrics Sample-level metrics.
#' @export
setMethod(
    f = "metrics",
    signature = signature(object = "SummarizedExperiment"),
    definition = `metrics,SE`
)
