#' @name calculateMetrics
#' @inherit AcidGenerics::calculateMetrics
#' @author Michael Steinbaugh, Rory Kirchner
#' @note Updated 2021-09-11.
#'
#' @details
#' Input a raw count matrix. Do not use size factor adjusted or log normalized
#' counts here.
#'
#' @inheritParams AcidRoxygen::params
#' @param ... Additional arguments.
#'
#' @param prefilter `logical(1)`.
#' Drop very low quality samples/cells from the object.
#' This can resize the number of columns but the rows (i.e. features) do not
#' change with this operation.
#'
#' @return
#' - `matrix` / `Matrix`: `DataFrame` containing metrics.
#' - `SummarizedExperiment`: Modified object, with metrics in
#' `colData()`.
#'
#' @examples
#' data(RangedSummarizedExperiment, package = "AcidTest")
#'
#' ## SummarizedExperiment ====
#' object <- RangedSummarizedExperiment
#' names(SummarizedExperiment::colData(object))
#' object <- calculateMetrics(object)
#' names(SummarizedExperiment::colData(object))
NULL



## Updated 2021-09-02.
`calculateMetrics,matrix` <- # nolint
    function(object,
             rowData = NULL,
             prefilter = FALSE) {
        assert(
            hasValidDimnames(object),
            hasRows(object),
            isAny(rowData, c("DataFrame", "NULL")),
            isFlag(prefilter)
        )
        if (isTRUE(prefilter)) {
            originalDim <- dim(object)
            object <- nonzeroRowsAndCols(object)
        }
        alert(sprintf(
            fmt = "Calculating %d sample %s.",
            ncol(object),
            ngettext(
                n = ncol(object),
                msg1 = "metric",
                msg2 = "metrics"
            )
        ))
        codingFeatures <- character()
        mitoFeatures <- character()
        missingBiotype <- function() {
            alertWarning(sprintf(
                fmt = paste0(
                    "Calculating metrics without biotype information.\n",
                    "{.fun %s} required to calculate: %s."
                ),
                "rowData",
                toInlineString(
                    x = c("nCoding", "nMito", "mitoRatio"),
                    class = "val"
                )
            ))
        }
        ## Calculate nCoding and nMito, which requires annotations.
        if (is.null(rowData)) {
            missingBiotype()
        } else {
            assert(
                is(rowData, "DataFrame"),
                hasRownames(rowData),
                isSubset(rownames(object), rownames(rowData))
            )
            rowData <- rowData[rownames(object), ]
            if (isSubset("broadClass", colnames(rowData))) {
                ## Drop rows with NA broad class.
                keep <- !is.na(rowData[["broadClass"]])
                rowData <- rowData[keep, , drop = FALSE]
                ## Coding features.
                keep <- rowData[["broadClass"]] == "coding"
                codingFeatures <- rownames(rowData[keep, , drop = FALSE])
                alertInfo(sprintf(
                    fmt = "%d coding %s detected.",
                    length(codingFeatures),
                    ngettext(
                        n = length(codingFeatures),
                        msg1 = "feature",
                        msg2 = "features"
                    )
                ))
                ## Mitochondrial features.
                keep <- rowData[["broadClass"]] == "mito"
                mitoFeatures <- rownames(rowData[keep, , drop = FALSE])
                alertInfo(sprintf(
                    fmt = "%d mitochondrial %s detected.",
                    length(mitoFeatures),
                    ngettext(
                        n = length(mitoFeatures),
                        msg1 = "feature",
                        msg2 = "features"
                    )
                ))
            } else {
                missingBiotype()
            }
        }
        ## Using S4 run-length encoding here to reduce memory overhead.
        ## We're following the naming conventions used in Seurat 3.
        ## Note that "nCount" represents "nUMI" for droplet scRNA-seq data.
        nCount <- Rle(as.integer(colSums(object)))
        nFeature <- Rle(as.integer(colSums(object > 0L)))
        nCoding <- if (hasLength(codingFeatures)) {
            mat <- object[codingFeatures, , drop = FALSE]
            Rle(as.integer(colSums(mat)))
        } else {
            Rle(NA_integer_)
        }
        nMito <- if (hasLength(mitoFeatures)) {
            mat <- object[mitoFeatures, , drop = FALSE]
            Rle(as.integer(colSums(mat)))
        } else {
            Rle(NA_integer_)
        }
        log10FeaturesPerCount <- log10(nFeature) / log10(nCount)
        mitoRatio <- nMito / nCount
        data <- DataFrame(
            "nCount" = nCount,
            "nFeature" = nFeature,
            "nCoding" = nCoding,
            "nMito" = nMito,
            "log10FeaturesPerCount" = log10FeaturesPerCount,
            "mitoRatio" = mitoRatio,
            row.names = colnames(object)
        )
        ## Apply low stringency pre-filtering.
        ## This keeps only samples/cells with non-zero features.
        if (isTRUE(prefilter)) {
            ## Novelty score.
            keep <-
                !is.na(data[["log10FeaturesPerCount"]]) &
                    data[["log10FeaturesPerCount"]] < 1L
            data <- data[keep, , drop = FALSE]
            ## Mito ratio.
            keep <-
                is.na(data[["mitoRatio"]]) |
                    data[["mitoRatio"]] < 1L
            data <- data[keep, , drop = FALSE]
            ## Minimum number of read counts per sample.
            keep <- data[["nCount"]] > 0L
            data <- data[keep, , drop = FALSE]
            ## Minimum number of features (i.e. genes) per sample.
            keep <- data[["nFeature"]] > 0L
            data <- data[keep, , drop = FALSE]
            n1 <- nrow(data)
            n2 <- originalDim[[2L]]
            if (isTRUE(n1 < n2)) {
                msg <- sprintf(
                    fmt = "Prefilter: %d / %d %s",
                    n1, n2,
                    ngettext(
                        n = n1,
                        msg1 = "sample",
                        msg2 = "samples"
                    )
                )
                if (requireNamespace("scales", quietly = TRUE)) {
                    msg <- paste0(
                        msg,
                        sprintf(" (%s)", scales::percent(n1 / n2))
                    )
                }
                msg <- paste0(msg, ".")
                alertInfo(msg)
            }
        }
        data
    }



## Updated 2021-02-22.
`calculateMetrics,Matrix` <- # nolint
    `calculateMetrics,matrix`



## Updated 2021-09-11.
`calculateMetrics,SE` <- # nolint
    function(object,
             assay = 1L,
             prefilter = FALSE) {
        validObject(object)
        assert(
            isScalar(assay),
            isFlag(prefilter)
        )
        ## Drop zero rows and columns to first to speed up calculations.
        if (isTRUE(prefilter)) {
            object <- nonzeroRowsAndCols(
                object = object,
                assay = assay
            )
        }
        metrics <- calculateMetrics(
            object = assay(object, i = assay),
            rowData = rowData(object),
            prefilter = prefilter
        )
        if (isTRUE(prefilter)) {
            object <- object[, rownames(metrics), drop = FALSE]
        }
        ## Update the metrics in column data.
        colData <- colData(object)
        colData <- colData[
            ,
            setdiff(colnames(colData), colnames(metrics)),
            drop = FALSE
        ]
        assert(identical(rownames(colData), rownames(metrics)))
        colData <- cbind(colData, metrics)
        colData(object) <- colData
        validObject(object)
        object
    }



#' @rdname calculateMetrics
#' @export
setMethod(
    f = "calculateMetrics",
    signature = signature(object = "Matrix"),
    definition = `calculateMetrics,Matrix`
)

#' @rdname calculateMetrics
#' @export
setMethod(
    f = "calculateMetrics",
    signature = signature(object = "SummarizedExperiment"),
    definition = `calculateMetrics,SE`
)

#' @rdname calculateMetrics
#' @export
setMethod(
    f = "calculateMetrics",
    signature = signature(object = "matrix"),
    definition = `calculateMetrics,matrix`
)
