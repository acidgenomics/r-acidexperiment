#' Sanitize sample data
#'
#' @section Denylist:
#'
#' Here's the current column denylist:
#'
#' - `interestingGroups`.
#' - `sampleId`.
#'
#' @note Updated 2021-02-25.
#' @export
#'
#' @param object `DataFrame` (recommended) or `data.frame` (legacy).
#' Note that legacy `data.frame` support will be removed in a future update.
#'
#' @return `DataFrame`.
#' Sanitized data frame containing only non-denylist columns and all
#' `character` columns coerced to `factor` (i.e. `stringsAsFactors`).
#'
#' @examples
#' data(RangedSummarizedExperiment, package = "AcidTest")
#' rse <- RangedSummarizedExperiment
#'
#' ## SummarizedExperiment ====
#' from <- sampleData(rse)
#' print(from)
#' to <- sanitizeSampleData(from)
#' all(vapply(to, is.factor, logical(1L)))
#' print(to)
sanitizeSampleData <- function(object) {
    assert(
        is(object, "DataFrame"),
        hasRownames(object),
        hasColnames(object),
        identical(
            x = colnames(object),
            y = camelCase(colnames(object), strict = TRUE)
        ),
        isSubset("sampleName", colnames(object)),
        hasNoDuplicates(object[["sampleName"]])
    )
    denylist <- c("interestingGroups", "sampleId")
    object <- object[, setdiff(colnames(object), denylist), drop = FALSE]
    object <- atomize(object)
    object <- factorize(object)
    assert(
        is(object, "DataFrame"),
        hasRownames(object)
    )
    object
}
