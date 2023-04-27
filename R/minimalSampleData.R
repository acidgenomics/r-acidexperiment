#' Minimal sample data
#'
#' @note Updated 2023-04-27.
#' @export
#'
#' @param sample `character`.
#' Sample names (e.g. per-sample directory names).
#'
#' @return `DFrame`.
#'
#' @examples
#' x <- minimalSampleData(sample = c("sample 1", "sample 2"))
#' print(x)
minimalSampleData <- function(sample) {
    assert(
        isCharacter(sample),
        hasNoDuplicates(sample)
    )
    sample <- as.factor(sample)
    DataFrame(sampleName = sample, row.names = makeNames(sample))
}
