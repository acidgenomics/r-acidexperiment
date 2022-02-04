#' Organism
#'
#' Get or set the organism (i.e. species) of an object.
#'
#' @name organism
#' @note Updated 2022-02-03.
#'
#' @inheritParams AcidRoxygen::params
#'
#' @return `character(1)`.
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
    signature = signature(object = "SummarizedExperiment"),
    definition = `organism,SE`
)
