#' @name metrics
#' @author Michael Steinbaugh, Rory Kirchner
#' @inherit AcidGenerics::metrics
#' @note These functions will error intentionally if no numeric columns are
#'   defined in `colData()`.
#' @note Updated 2021-02-02.
#'
#' @inheritParams AcidRoxygen::params
#' @param ... Additional arguments.
#'
#' @details
#' [metrics()] takes data stored in [`colData()`][SummarizedExperiment::colData]
#' and consistently returns a `tbl_df` or `DataFrame` with `sampleName` and
#' `interestingGroups` columns, even when these columns are not defined in
#' `colData()`. This is designed to integrate with plotting functions that use
#' ggplot2 internally.
#'
#' @param fun `character(1)`.
#'   Mathematical function name to apply.
#'   Uses [`match.arg()`][base::match.arg] internally.
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



## Updated 2019-08-18.
`metrics,SummarizedExperiment` <-  # nolint
    function(object, return = c("tbl_df", "DataFrame")) {
        validObject(object)
        return <- match.arg(return)
        data <- sampleData(object, clean = FALSE)
        ## Decode columns that contain Rle, if necessary.
        data <- decode(data)
        switch(
            EXPR = return,
            "DataFrame" = data,
            "tbl_df" = as_tibble(data, rownames = "sampleId")
        )
    }



#' @describeIn metrics Sample-level metrics.
#' @export
setMethod(
    f = "metrics",
    signature = signature("SummarizedExperiment"),
    definition = `metrics,SummarizedExperiment`
)
