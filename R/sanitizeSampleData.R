#' Sanitize sample data
#'
#' Drop any columns defined in denylist, and ensure `character` columns
#' containing any duplicate values are automatically coerced to `factor`.
#'
#' @section Denylist:
#'
#' Here's the current column denylist:
#'
#' - `interestingGroups`.
#' - `sampleId`.
#'
#' @note Updated 2023-04-27.
#' @export
#'
#' @param object `DFrame` (recommended) or `data.frame` (legacy).
#' Note that legacy `data.frame` support will be removed in a future update.
#'
#' @return `DFrame`.
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
        is(object, "DFrame"),
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
        is(object, "DFrame"),
        hasRownames(object)
    )
    object
}
