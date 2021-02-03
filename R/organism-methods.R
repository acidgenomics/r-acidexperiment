## Updated 2021-02-01.
`organism,SummarizedExperiment` <-  # nolint
    function(object) {
        ## Attempt to use metadata stash, if defined.
        organism <- `organism,Annotated`(object)
        if (isString(organism)) {
            return(organism)
        }
        ## Fall back to detecting from rowRanges or rownames.
        if (hasColnames(rowData(object))) {
            colnames(rowData(object)) <-
                camelCase(
                    object = colnames(rowData(object)),
                    strict = TRUE
                )
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
    definition = `organism,SummarizedExperiment`
)
