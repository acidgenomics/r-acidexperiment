#' @name organism
#' @inherit AcidGenerics::organism
#' @note Updated 2021-02-03.
#'
#' @inheritParams AcidRoxygen::params
#' @param ... Additional arguments.
#'
#' @examples
#' data(RangedSummarizedExperiment, package = "AcidTest")
#'
#' ## SummarizedExperiment ====
#' object <- RangedSummarizedExperiment
#' organism(object)
#' organism(object) <- "xxx"
#' organism(object)
NULL



## Updated 2021-02-03.
`organism,SE` <-  # nolint
    function(object) {
        ## Attempt to use metadata stash, if defined.
        fun <- methodFunction(
            f = "organism",
            signature = "Annotated",
            package = "AcidGenomes"
        )
        organism <- fun(object)
        if (isString(organism)) {
            return(organism)
        }
        ## Fall back to detecting from rowRanges or rownames.
        if (hasColnames(rowData(object))) {
            colnames(rowData(object)) <-
                camelCase(colnames(rowData(object)), strict = TRUE)
        }
        if (isSubset("geneId", colnames(rowData(object)))) {
            x <- as.character(rowData(object)[["geneId"]])
        } else {
            x <- rownames(object)
        }
        detectOrganism(x)
    }



#' @rdname organism
#' @export
setMethod(
    f = "organism",
    signature = signature("SummarizedExperiment"),
    definition = `organism,SE`
)
