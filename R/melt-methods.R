#' @name melt
#' @inherit AcidPlyr::melt
#'
#' @note Updated 2023-04-27.
#'
#' @inheritParams AcidRoxygen::params
#' @param ... Additional arguments.
#'
#' @examples
#' data(RangedSummarizedExperiment, package = "AcidTest")
#'
#' ## SummarizedExperiment ====
#' object <- RangedSummarizedExperiment
#' dim(object)
#' x <- melt(object)
#' nrow(x)
#' print(x)
NULL



## Updated 2020-10-07.
`melt,matrix` <- # nolint
    methodFunction(
        f = "melt",
        signature = "matrix",
        package = "AcidPlyr"
    )



## Updated 2021-02-22.
`melt,Matrix` <- # nolint
    `melt,matrix`



## Updated 2019-08-24.
`melt,SE` <- # nolint
    function(object,
             assay = 1L,
             min,
             minMethod,
             trans) {
        validObject(object)
        assert(isScalar(assay))
        minMethod <- match.arg(minMethod)
        trans <- match.arg(trans)
        counts <- assay(object, i = assay)
        data <- melt(
            object = counts,
            min = min,
            minMethod = minMethod,
            trans = trans
        )
        colnamesCol <- colnames(data)[[2L]]
        colData <- sampleData(object)
        colData[[colnamesCol]] <- rownames(colData)
        data <- leftJoin(data, colData, by = colnamesCol)
        data <- encode(data)
        data
    }

args <- c("min", "minMethod", "trans")
formals(`melt,SE`)[args] <- # nolint
    formals(`melt,matrix`)[args]
rm(args)



#' @rdname melt
#' @export
setMethod(
    f = "melt",
    signature = signature(object = "Matrix"),
    definition = `melt,Matrix`
)

#' @rdname melt
#' @export
setMethod(
    f = "melt",
    signature = signature(object = "SummarizedExperiment"),
    definition = `melt,SE`
)
