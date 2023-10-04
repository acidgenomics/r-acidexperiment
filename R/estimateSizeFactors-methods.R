#'
#' Define size factors from the library sizes, and then apply centering at
#' unity. This ensures that the library size adjustment yields values comparable
#' to those generated after normalization with other sets of size factors.
#'
#' Centering of size factors at unity ensures that division by size factors
#' yields values on the same scale as the raw counts. This is important for the
#' interpretation of the normalized values, as well as comaprisons between
#' features normalized with different size factors (e.g., spike-ins).
#'
#' The estimated size factors computed by this function can be accessed using
#' the accessor function `sizeFactors()`. Alternative library size estimators
#' can also be supplied using the assignment function `sizeFactors<-()`.
#'
#' @name estimateSizeFactors
#' @note Updated 2023-10-04.
#'
#' @inheritParams AcidRoxygen::params
#' @param ... Additional arguments.
#'
#' @param type `character(1)`.
#' Type of method for estimation.
#'
#' ```
#' libSize <- colSums(counts(object))
#' ```
#'
#' `"mean-ratio"`:
#'
#' ```
#' libSize / mean(libSize)
#' ```
#'
#' `"geometric-mean-ratio"`:
#'
#' ```
#' libSize / geometricMean(libSize)
#' ```
#'
#' `"log-geometric-mean-ratio"`:
#'
#' ```
#' log(libSize) / geometricMean(log(libSize))
#' ```
#'
#' @param center `numeric(1)`.
#' If non-zero, scales all size factors so that the average size factor across
#' cells is equal to the value defined. Set to `0` to disable centering.
#'
#' @seealso
#' DESeq2:
#' - `DESeq2::estimateSizeFactors()`.
#' - `DESeq2::estimateSizeFactorsForMatrix().`
#'
#' scuttle (now inherited in scater):
#' - `scuttle::librarySizeFactors()`.
#' - `scuttle::logNormCounts()`.
#'
#' monocle3:
#' - `monocle3::estimate_size_factors()`.
#' - `monocle3:::estimate_sf_sparse()`.
#'
#' @return Modified object.
#' Use `sizeFactors()` to access the computed size factor numeric.
#'
#' @examples
#' data(RangedSummarizedExperiment, package = "AcidTest")
#'
#' ## SummarizedExperiment ====
#' object <- RangedSummarizedExperiment
#' object <- estimateSizeFactors(object)
#' sizeFactors(object)
NULL



## Updated 2021-09-02.
.librarySizeFactors <- # nolint
    function(counts,
             type = c(
                 "mean-ratio",
                 "geometric-mean-ratio",
                 "log-geometric-mean-ratio"
             )) {
        assert(
            isAny(counts, c("matrix", "Matrix")),
            !anyNA(counts)
        )
        type <- match.arg(type)
        alert(sprintf(
            fmt = paste(
                "Calculating library size factors using {.val %s}",
                "method defined in {.arg %s}."
            ),
            type, "type"
        ))
        ## Get the sum of expression per cell.
        libSizes <- colSums(counts)
        ## Error on detection of cells without any expression.
        assert(
            all(libSizes > 0L),
            msg = "Zero values from 'colSums()' detected."
        )
        ## Calculate the size factors per cell.
        sf <- switch(
            EXPR = type,
            "mean-ratio" = {
                libSizes / mean(libSizes)
            },
            "geometric-mean-ratio" = {
                libSizes / geometricMean(libSizes)
            },
            "log-geometric-mean-ratio" = {
                log(libSizes) / geometricMean(log(libSizes))
            }
        )
        assert(!anyNA(sf))
        names(sf) <- colnames(counts)
        sf
    }





## Updated 2023-03-01.
.centerSizeFactors <- function(sf, center = 1L) {
    assert(
        is.numeric(sf),
        hasNames(sf),
        isNumber(center),
        isPositive(center)
    )
    alert(sprintf("Centering size factors at %d.", center))
    sf <- sf / mean(sf) * center
    assert(isTRUE(all.equal(mean(sf), center)))
    sf
}



## Updated 2023-03-01.
`estimateSizeFactors,SE` <- # nolint
    function(object,
             assay = 1L,
             type,
             center) {
        validObject(object)
        assert(
            isScalar(assay),
            isNumber(center),
            isNonNegative(center)
        )
        type <- match.arg(type)
        counts <- assay(object, i = assay)
        sf <- .librarySizeFactors(counts = counts, type = type)
        if (center > 0L) {
            sf <- .centerSizeFactors(sf = sf, center = center)
        }
        assert(is.numeric(sf), hasNames(sf))
        sizeFactors(object) <- sf
        object
    }

formals(`estimateSizeFactors,SE`)[ # nolint
    c("type", "center")
] <- list(
    type = formals(.librarySizeFactors)[["type"]],
    center = formals(.centerSizeFactors)[["center"]]
)



#' @rdname estimateSizeFactors
#' @export
setMethod(
    f = "estimateSizeFactors",
    signature = signature(object = "SummarizedExperiment"),
    definition = `estimateSizeFactors,SE`
)
