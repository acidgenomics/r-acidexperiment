#' @name sizeFactors
#' @inherit AcidGenerics::sizeFactors
#' @note Updated 2021-02-02.
#'
#' @inheritParams AcidRoxygen::params
#' @param ... Additional arguments.
#'
#' @examples
#' data(RangedSummarizedExperiment, package = "AcidTest")
#'
#' ## SummarizedExperiment ====
#' object <- RangedSummarizedExperiment
#' object <- estimateSizeFactors(object)
#' head(sizeFactors(object))
#' mean(sizeFactors(object))
NULL



## If exporting a numeric value signature for SE, the SE method will mask SCE
## ANY value method. In this case, we need to export a corresponding SCE numeric
## method.
##
## See also:
## - https://github.com/drisso/SingleCellExperiment/pull/34



## Updated 2019-08-06.
`sizeFactors,SE` <-  # nolint
    function(object) {
        if (!"sizeFactor" %in% names(colData(object))) {
            return(NULL)
        }
        sf <- colData(object)[["sizeFactor"]]
        names(sf) <- colnames(object)
        sf
    }



## Updated 2019-08-06.
`sizeFactors<-,SE,ANY` <-  # nolint
    function(object, value) {
        if (!is.null(value)) {
            assert(
                all(!is.na(value)),
                all(is.finite(value)),
                all(value > 0L)
            )
            value <- unname(value)
        }
        colData(object)[["sizeFactor"]] <- value
        validObject(object)
        object
    }



#' @rdname sizeFactors
#' @export
setMethod(
    f = "sizeFactors",
    signature = signature("SummarizedExperiment"),
    definition = `sizeFactors,SE`
)


#' @rdname sizeFactors
#' @export
setReplaceMethod(
    f = "sizeFactors",
    signature = signature(
        object = "SummarizedExperiment",
        value = "ANY"
    ),
    definition = `sizeFactors<-,SE,ANY`
)
