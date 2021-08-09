## FIXME Split this out into separate files.



#' @name stripGeneVersions
#' @inherit AcidGenomes::stripGeneVersions
#' @note Updated 2021-08-09.
#'
#' @inheritParams AcidRoxygen::params
#' @param ... Additional arguments.
#'
#' @examples
#' data(SummarizedExperiment_transcripts, package = "AcidTest")
#' rowData <- SummarizedExperiment::rowData
#'
#' ## SummarizedExperiment ====
#' object <- SummarizedExperiment_transcripts
#' head(rowData(object)[["txId"]])
#' head(rowData(object)[["geneId"]])
#' head(rownames(object))
#' object <- stripGeneVersions(object)
#' object <- stripTranscriptVersions(object)
#' head(rowData(object)[["txId"]])
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
    signature = signature("SummarizedExperiment"),
    definition = `stripGeneVersions,SE`
)
