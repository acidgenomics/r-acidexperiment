#' @name autopadZeros
#' @inherit AcidGenerics::autopadZeros
#' @note Updated 2021-09-02.
#'
#' @details
#' For methods on objects supporting `dim()` (e.g. `matrix`), the object will be
#' returned with the rows and/or columns resorted by default. This does not
#' apply to the `character` method defined in syntactic.
#'
#' @inheritParams AcidRoxygen::params
#' @param ... Additional arguments.
#'
#' @param sampleNames `logical(1)`.
#' *Applies to `SummarizedExperiment` method.* #' If `sampleName` column is
#' defined in `colData()`, these values will also get padded, if necessary.
#' This improves downstream handling in functions that rely on this feature.
#'
#' @return `character`.
#'
#' @examples
#' data(RangedSummarizedExperiment, package = "AcidTest")
#'
#' ## SummarizedExperiment ====
#' object <- RangedSummarizedExperiment
#' autopadZeros(object, rownames = TRUE, colnames = TRUE)
NULL



## Updated 2019-07-22.
`autopadZeros,matrix` <- # nolint
    function(object,
             rownames = FALSE,
             colnames = TRUE,
             sort = TRUE) {
        assert(
            hasValidDimnames(object),
            isFlag(rownames),
            isFlag(colnames),
            isFlag(sort)
        )
        if (isTRUE(rownames) && hasRownames(object)) {
            rownames(object) <- autopadZeros(rownames(object))
            if (isTRUE(sort)) {
                object <- object[sort(rownames(object)), , drop = FALSE]
            }
        }
        if (isTRUE(colnames) && hasColnames(object)) {
            colnames(object) <- autopadZeros(colnames(object))
            if (isTRUE(sort)) {
                object <- object[, sort(colnames(object)), drop = FALSE]
            }
        }
        object
    }



## Updated 2021-09-02.
`autopadZeros,SE` <- # nolint
    function(object,
             rownames = FALSE,
             colnames = TRUE,
             sampleNames = TRUE,
             sort = TRUE) {
        assert(isFlag(sampleNames))
        what <- `autopadZeros,matrix`
        args <- list(
            "object" = object,
            "rownames" = rownames,
            "colnames" = colnames,
            "sort" = sort
        )
        object <- do.call(what = what, args = args)
        if (
            isTRUE(sampleNames) &&
                "sampleName" %in% colnames(colData(object))
        ) {
            sampleNames(object) <- autopadZeros(sampleNames(object))
        }
        object
    }



#' @rdname autopadZeros
#' @export
setMethod(
    f = "autopadZeros",
    signature = signature(object = "SummarizedExperiment"),
    definition = `autopadZeros,SE`
)

#' @rdname autopadZeros
#' @export
setMethod(
    f = "autopadZeros",
    signature = signature(object = "matrix"),
    definition = `autopadZeros,matrix`
)
