#' @name stripVersions
#' @inherit AcidGenomes::stripVersions
#' @note Updated 2021-02-03.
#'
#' @inheritParams AcidRoxygen::params
#' @param ... Additional arguments.
#'
#' @examples
#' data(SummarizedExperiment_transcripts, package = "AcidTest")
#'
#' ## SummarizedExperiment ====
#' object <- SummarizedExperiment_transcripts
#' head(rowData(object)[["txId"]])
#' head(rowData(object)[["geneId"]])
#' head(rownames(object))
#' object <- stripGeneVersions(object)
#' object <- stripTranscriptVersions(object)
#' head(SummarizedExperiment::rowData(object)[["txId"]])
#' head(SummarizedExperiment::rowData(object)[["geneId"]])
#' head(rownames(object))
NULL



## Updated 2021-02-02.
`stripGeneVersions,SE` <-  # nolint
    methodFunction(
        f = "stripGeneVersions",
        signature = "matrix",
        package = "AcidGenomes"
    )



#' @rdname stripVersions
#' @export
setMethod(
    f = "stripGeneVersions",
    signature = signature("SummarizedExperiment"),
    definition = `stripGeneVersions,SE`
)



## Updated 2021-02-02.
`stripTranscriptVersions,SE` <-  # nolint
    methodFunction(
        f = "stripTranscriptVersions",
        signature = "matrix",
        package = "AcidGenomes"
    )



#' @rdname stripVersions
#' @export
setMethod(
    f = "stripTranscriptVersions",
    signature = signature("SummarizedExperiment"),
    definition = `stripTranscriptVersions,SE`
)
