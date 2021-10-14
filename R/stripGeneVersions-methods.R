#' @name stripGeneVersions
#' @inherit AcidGenomes::stripGeneVersions
#' @note Updated 2021-08-09.
#'
#' @inheritParams AcidRoxygen::params
#' @param ... Additional arguments.
#'
#' @examples
#' data(RangedSummarizedExperiment, package = "AcidTest")
#' rowData <- SummarizedExperiment::rowData
#'
#' ## SummarizedExperiment ====
#' object <- RangedSummarizedExperiment
#' head(rowData(object)[["geneId"]])
#' rownames(object) <- as.character(rowData(object)[["geneId"]])
#' head(rownames(object))
#' object <- stripGeneVersions(object)
#' head(rowData(object)[["geneId"]])
#' head(rownames(object))
NULL



## Updated 2021-02-02.
`stripGeneVersions,SE` <-  # nolint
    methodFunction(
        f = "stripGeneVersions",
        signature = "matrix",
        package = "AcidGenomes"
    )



#' @rdname stripGeneVersions
#' @export
setMethod(
    f = "stripGeneVersions",
    signature = signature(object = "SummarizedExperiment"),
    definition = `stripGeneVersions,SE`
)
