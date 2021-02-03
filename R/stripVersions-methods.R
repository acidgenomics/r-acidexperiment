## FIXME NEED TO ADD DOCUMENTATION.



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
