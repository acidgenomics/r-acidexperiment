#' Combine multiple objects
#'
#' @name combine
#' @note Updated 2022-05-23.
#'
#' @inheritParams AcidRoxygen::params
#' @param ... Additional arguments.
#'
#' @details
#' We're attempting to make this as strict as possible, requiring:
#'
#' - Rows (genes) across objects must be identical.
#' - [rowRanges][SummarizedExperiment::rowRanges] and/or
#' [rowData][SummarizedExperiment::rowData]
#' [metadata][S4Vectors::metadata] must be identical.
#' - [colData][SummarizedExperiment::colData] must contain the same columns.
#' - Specific metadata must be identical (see `metadata` argument).
#'
#' @seealso
#' - `help("merge.Matrix", "Matrix.utils")`.
#'
#' @return A single value of the same class as the input values.
#'
#' @examples
#' data(RangedSummarizedExperiment, package = "AcidTest")
#'
#' ## SummarizedExperiment ====
#' x <- RangedSummarizedExperiment
#' colnames(x) <- paste0(
#'     "sample",
#'     sprintf("%02d", seq_len(ncol(x)))
#' )
#'
#' y <- x
#' colnames(y) <- paste0(
#'     "sample",
#'     sprintf("%02d", seq(from = ncol(y) + 1L, to = ncol(y) * 2L))
#' )
#'
#' ## Combine the two objects.
#' c <- combine(x, y)
#' sampleData(c)
#' print(c)
NULL



# Updated 2020-01-20.
`combine,SE` <- # nolint
    function(x, y) {
        validObject(x)
        validObject(y)
        assert(
            identical(class(x), class(y)),
            identical(assayNames(x), assayNames(y)),
            ## Require that there are no duplicate samples.
            areDisjointSets(colnames(x), colnames(y)),
            ## Currently we're being strict and requiring that the rows
            ## (features) are identical, otherwise zero counts may be
            ## misleading.
            identical(rownames(x), rownames(y))
        )
        ## Coerce the objects to SummarizedExperiment.
        ## Keep as RSE if the data is ranged.
        if (is(x, "RangedSummarizedExperiment")) {
            Class <- "RangedSummarizedExperiment" # nolint
        } else {
            Class <- "SummarizedExperiment" # nolint
        }
        alert(sprintf("Combining objects into {.var %s}.", Class))
        x <- as(object = x, Class = Class)
        y <- as(object = y, Class = Class)
        ## Assays --------------------------------------------------------------
        alert(sprintf(
            "Binding columns in {.fun %s}: %s.",
            "assays",
            toInlineString(assayNames(x), n = 5L)
        ))
        assays <- Map(x = assays(x), y = assays(y), f = cbind)
        assert(is.list(assays))
        assays <- as(assays, "SimpleList")
        ## Row data ------------------------------------------------------------
        alert("Checking row data.")
        ## Require that the gene annotations are identical.
        if (is(x, "RangedSummarizedExperiment")) {
            assert(identical(rowRanges(x), rowRanges(y)))
            rowRanges <- rowRanges(x)
            rowData <- NULL
        } else {
            assert(identical(rowData(x), rowData(y)))
            rowRanges <- NULL
            rowData <- rowData(x)
            ## This provides backward compatibility for BioC 3.7.
            rownames(rowData) <- rownames(x)
        }
        ## Column data ---------------------------------------------------------
        alert("Updating column data.")
        cdx <- colData(x)
        cdy <- colData(y)
        ## Check for column mismatches and restore NA values, if necessary. This
        ## mismatch can occur because our metadata importer will drop columns
        ## with all NA values, which is useful for handling human metadata. This
        ## can create a column mismatch when we're subsetting large sequencing
        ## runs into batches.
        union <- union(names(cdx), names(cdy))
        intersect <- intersect(names(cdx), names(cdy))
        if (!isTRUE(identical(union, intersect))) {
            setdiff <- setdiff(union, intersect)
            alertWarning(sprintf(
                "Fixing %d mismatched %s in {.fun %s}: %s.",
                length(setdiff),
                ngettext(
                    n = length(setdiff),
                    msg1 = "column",
                    msg2 = "columns"
                ),
                "colData",
                toInlineString(setdiff, n = 10L, class = "val")
            ))
            diffx <- setdiff(setdiff, names(cdx))
            for (col in diffx) {
                cdx[[col]] <- NA
            }
            diffy <- setdiff(setdiff, names(cdy))
            for (col in diffy) {
                cdy[[col]] <- NA
            }
            colData(x) <- cdx
            colData(y) <- cdy
        }
        assert(areSetEqual(
            x = colnames(colData(x)),
            y = colnames(colData(y))
        ))
        keep <- sort(intersect(
            x = colnames(colData(x)),
            y = colnames(colData(y))
        ))
        colData <- rbind(
            colData(x)[, keep, drop = FALSE],
            colData(y)[, keep, drop = FALSE]
        )

        ## Metadata ------------------------------------------------------------
        alert("Updating metadata.")
        mx <- metadata(x)
        my <- metadata(y)
        ## We're keeping only metadata elements that are common in both objects.
        keep <- intersect(names(mx), names(my))
        if (!isTRUE(setequal(x = names(mx), y = names(my)))) {
            drop <- setdiff(x = union(names(mx), names(my)), y = keep)
            alertWarning(sprintf(
                "Dropping %d disjoint metadata %s: %s.",
                length(drop),
                ngettext(
                    n = length(drop),
                    msg1 = "element",
                    msg2 = "elements"
                ),
                toInlineString(drop, n = 10L, class = "val")
            ))
        }
        mx <- mx[keep]
        my <- my[keep]
        ## Keep only metadata that is identical across both objects.
        keep <- as.logical(Map(x = mx, y = my, f = identical))
        names(keep) <- names(mx)
        drop <- names(keep)[!keep]
        if (hasLength(drop)) {
            alertWarning(sprintf(
                "Dropping %d non-identical metadata %s: %s.",
                length(drop),
                ngettext(
                    n = length(drop),
                    msg1 = "element",
                    msg2 = "elements"
                ),
                toInlineString(drop, n = 10L, class = "val")
            ))
        }
        assert(identical(x = mx[keep], y = my[keep]))
        metadata <- mx[keep]
        metadata[["combine"]] <- TRUE
        metadata <- Filter(Negate(is.null), metadata)
        ## Return --------------------------------------------------------------
        args <- list(
            "assays" = assays,
            "rowRanges" = rowRanges,
            "rowData" = rowData,
            "colData" = colData,
            "metadata" = metadata
        )
        args <- Filter(Negate(is.null), args)
        out <- do.call(what = SummarizedExperiment, args = args)
        validObject(out)
        out
    }



#' @rdname combine
#' @export
setMethod(
    f = "combine",
    signature = signature(
        x = "SummarizedExperiment",
        y = "SummarizedExperiment"
    ),
    definition = `combine,SE`
)
