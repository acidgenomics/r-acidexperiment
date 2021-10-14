#' @name stripTranscriptVersions
#' @inherit AcidGenomes::stripTranscriptVersions
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
#' rownames(object) <- as.character(rowData(object)[["txId"]])
#' head(rownames(object))
#' object <- stripTranscriptVersions(object)
#' head(rowData(object)[["txId"]])
#' head(rownames(object))
NULL



## Updated 2021-02-02.
`stripTranscriptVersions,SE` <-  # nolint
    methodFunction(
        f = "stripTranscriptVersions",
        signature = "matrix",
        package = "AcidGenomes"
    )



#' @rdname stripTranscriptVersions
#' @export
setMethod(
    f = "stripTranscriptVersions",
    signature = signature(object = "SummarizedExperiment"),
    definition = `stripTranscriptVersions,SE`
)
