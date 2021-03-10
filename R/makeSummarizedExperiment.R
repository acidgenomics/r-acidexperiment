#' Make a SummarizedExperiment object
#'
#' This function is a utility wrapper for `SummarizedExperiment` that provides
#' automatic subsetting for row and column data, as well as automatic handling
#' of transgenes and spike-ins.
#'
#' @section Session information:
#'
#' This function improves upon the standard constructor by slotting useful
#' session information into the `metadata` slot by default:
#'
#' - `date`: Today's date, returned from `Sys.Date`.
#' - `sessionInfo`: [sessioninfo::session_info()] return.
#'   This behavior can be disabled by setting `sessionInfo = FALSE`.
#' - `wd`: Working directory, returned from `getwd`.
#'
#' @export
#' @note Updated 2021-03-10.
#'
#' @inheritParams AcidRoxygen::params
#' @param denylist `logical(1)`.
#'   Apply a denylist check on illegal column names defined in `colData`. This
#'   is useful for catching names that are not considered best practice, and
#'   other values that may conflict with Acid Genomics packages. Refer to
#'   `metadataDenylist` for the current list of offending values.
#' @param sort `logical(1)`.
#'   Ensure all row and column names are sorted alphabetically. This includes
#'   columns inside `rowData` and `colData`, and `metadata` slot names. Assay
#'   names are required to contain `counts` as the first assay.
#' @param sessionInfo `logical(1)`.
#'   Slot session information into `metadata`.
#'
#' @return
#' - Providing `rowRanges`: `RangedSummarizedExperiment`.
#' - Providing `rowData`: `SummarizedExperiment`.
#'
#' @seealso
#' - [`SummarizedExperiment()`][SummarizedExperiment::SummarizedExperiment].
#' - `help("RangedSummarizedExperiment-class", "SummarizedExperiment")`.
#' - `help("SummarizedExperiment-class", "SummarizedExperiment")`.
#'
#' @examples
#' ## Rows (genes)
#' genes <- c(
#'     sprintf("gene%02d", seq_len(3L)),
#'     "EGFP"  # transgene
#' )
#' print(genes)
#'
#' ## Columns (samples)
#' samples <- sprintf("sample%02d", seq_len(4L))
#' print(samples)
#'
#' ## Counts (assay)
#' counts <- matrix(
#'     data = seq_len(length(genes) * length(samples)),
#'     nrow = length(genes),
#'     ncol = length(samples),
#'     dimnames = list(genes, samples)
#' )
#' ## Primary assay must be named "counts".
#' assays <- SimpleList(counts = counts)
#' print(assays)
#'
#' ## Row data (genomic ranges)
#' ## Note that we haven't defined the transgene here.
#' ## It will be handled automatically in the function call.
#' rowRanges <- emptyRanges(names = head(genes, n = length(genes) - 1L))
#' print(rowRanges)
#'
#' ## Column data
#' colData <- DataFrame(
#'     age = rep(
#'         x = c(3L, 6L),
#'         times = length(samples) / 2L
#'     ),
#'     genotype = rep(
#'         x = c("wildtype", "knockout"),
#'         times = 1L,
#'         each = length(samples) / 2L
#'     ),
#'     row.names = samples
#' )
#' print(colData)
#'
#' ## Minimal mode.
#' x <- makeSummarizedExperiment(assays = assays)
#' print(x)
#'
#' x <- makeSummarizedExperiment(
#'     assays = assays,
#'     rowRanges = rowRanges,
#'     colData = colData,
#'     transgeneNames = "EGFP"
#' )
#' print(x)
makeSummarizedExperiment <- function(
    assays = SimpleList(),
    rowRanges = GRanges(),
    rowData = NULL,
    colData = DataFrame(),
    metadata = list(),
    transgeneNames = NULL,
    ## This is intentionally disabled in cBioPortalAnalysis package.
    denylist = TRUE,
    sort = TRUE,
    sessionInfo = TRUE
) {
    assert(
        isAny(assays, c("SimpleList", "list")),
        isAny(rowRanges, c("GRanges", "GRangesList", "NULL")),
        isAny(rowData, c("DataFrame", "NULL")),
        isAny(colData, c("DataFrame", "NULL")),
        isAny(metadata, c("list", "NULL")),
        isAny(transgeneNames, c("character", "NULL")),
        isFlag(denylist),
        isFlag(sort),
        isFlag(sessionInfo)
    )
    if (hasLength(rowRanges)) {
        assert(!hasLength(rowData))
    }
    if (!is(assays, "SimpleList")) {
        assays <- SimpleList(assays)
    }
    ## Assays ------------------------------------------------------------------
    assays <- Filter(f = Negate(is.null), x = assays)
    if (hasLength(assays)) {
        ## Name the primary assay "counts" by default.
        if (!hasNames(assays)) {
            if (hasLength(assays, n = 1L)) {
                names(assays) <- "counts"
            } else {
                stop("Multiple assays defined without names.")
            }
        }
        assert(
            hasNames(assays),
            hasValidNames(assays)
        )
        assay <- assays[[1L]]
        if (hasLength(assay)) {
            assert(
                hasRownames(assay),
                hasColnames(assay),
                hasNoDuplicates(rownames(assay)),
                hasNoDuplicates(colnames(assay))
            )
            ## Inform rather than erroring when row and/or column names are
            ## invalid. Previously, we used this stricter approach:
            ## > assert(hasValidDimnames(assay))
            ok <- validNames(rownames(assay))
            if (!isTRUE(ok)) {
                alertWarning(cause(ok))
            }
            ok <- validNames(colnames(assay))
            if (!isTRUE(ok)) {
                alertWarning(cause(ok))
            }
        }
    }
    ## Row data ----------------------------------------------------------------
    ## Dynamically allow input of rowRanges (recommended) or rowData (fallback):
    ## - Detect rows that don't contain annotations.
    ## - Transgenes should contain `transgene` seqname.
    ## - Non-Ensembl features (e.g. IgG1) will be given `unknown` seqname.
    ## - Error on missing features; indicates a genome build mismatch.
    if (hasLength(rowRanges)) {
        rowData <- NULL
        assert(
            isAny(rowRanges, c("GRanges", "GRangesList")),
            areIntersectingSets(rownames(assay), names(rowRanges))
        )
        ## Ensure we unclass any special classes returned from AcidGenomes.
        if (is(rowRanges, "GRangesList")) {
            rowRanges <- as(rowRanges, "GRangesList")
            if (hasCols(mcols(rowRanges[[1L]]))) {
                assert(
                    identical(
                        x = colnames(mcols(rowRanges)[[1L]]),
                        y = camelCase(
                            object = colnames(mcols(rowRanges)[[1L]]),
                            strict = TRUE
                        )
                    )
                )
            }
        } else {
            rowRanges <- as(rowRanges, "GRanges")
            if (hasCols(mcols(rowRanges))) {
                colnames(mcols(rowRanges)) <-
                    camelCase(colnames(mcols(rowRanges)), strict = TRUE)
            }
        }
        setdiff <- setdiff(rownames(assay), names(rowRanges))
        if (hasLength(setdiff) && !is(rowRanges, "GRanges")) {
            stop(paste(
                "Automatic mismatched feature handling only",
                "curently supported for 'GRanges' (not 'GRangesList')."
            ))
        }
        ## Transgenes.
        if (hasLength(transgeneNames)) {
            assert(
                hasLength(setdiff),
                isSubset(transgeneNames, setdiff)
            )
            transgeneRanges <- emptyRanges(
                names = transgeneNames,
                seqname = "transgene",
                mcolnames = names(mcols(rowRanges))
            )
            suppressWarnings({
                rowRanges <- c(transgeneRanges, rowRanges)
            })
            setdiff <- setdiff(rownames(assay), names(rowRanges))
        }
        ## Additional non-Ensembl gene symbols. Automatically handle extra gene
        ## symbols in 10X Cell Ranger output. For example: CD11b, CD127, HLA-Dr,
        ## IgG1, PD-1, etc.
        pattern <- "^(EN[ST].+[0-9.]+)(_PAR_Y)?$"
        if (
            hasLength(setdiff) &&
            any(grepl(pattern = pattern, x = names(rowRanges)))
        ) {
            symbols <- setdiff[!grepl(pattern = pattern, x = setdiff)]
            alertWarning(sprintf(
                fmt = "%d non-Ensembl %s detected: {.val %s}.",
                length(symbols),
                ngettext(
                    n = length(symbols),
                    msg1 = "feature",
                    msg2 = "features"
                ),
                toString(symbols, width = 200L)
            ))
            alertInfo("Define transgenes using {.arg transgeneNames}.")
            unknownRanges <- emptyRanges(
                names = symbols,
                mcolnames = names(mcols(rowRanges))
            )
            suppressWarnings({
                rowRanges <- c(unknownRanges, rowRanges)
            })
            setdiff <- setdiff(rownames(assay), names(rowRanges))
        }
        ## Error on unannotated Ensembl features. This often indicates an
        ## accidental genome build release version mismatch.
        assert(isSubset(rownames(assay), names(rowRanges)))
        rowRanges <- rowRanges[rownames(assay)]
        rowRanges <- encode(rowRanges)
    } else if (hasRows(rowData)) {
        assert(
            isSubset(rownames(assay), rownames(rowData)),
            hasColnames(rowData)
        )
        colnames(rowData) <- camelCase(colnames(rowData), strict = TRUE)
        rowData <- rowData[rownames(assay), , drop = FALSE]
    }
    ## Column data -------------------------------------------------------------
    ## Allowing some single-cell RNA-seq automatic columns to pass through.
    if (hasLength(colData)) {
        assert(
            hasColnames(colData),
            hasRows(colData)
        )
        colnames(colData) <- camelCase(colnames(colData), strict = TRUE)
        assert(isSubset(colnames(assay), rownames(colData)))
        if (isTRUE(denylist)) {
            denylist <- setdiff(metadataDenylist, c("revcomp", "sampleId"))
            assert(areDisjointSets(colnames(colData), denylist))
        }
        colData <- colData[colnames(assay), , drop = FALSE]
    }
    ## Metadata ----------------------------------------------------------------
    if (is.null(metadata)) {
        metadata <- list()
    }
    if (isTRUE(sessionInfo)) {
        metadata[["date"]] <- Sys.Date()
        metadata[["sessionInfo"]] <- session_info(include_base = TRUE)
        metadata[["wd"]] <- realpath(".")
    }
    metadata <- Filter(f = Negate(is.null), x = metadata)
    if (hasLength(metadata)) {
        assert(hasValidNames(metadata))
    }
    ## Return ------------------------------------------------------------------
    ## Ensure we're not passing any `NULL` or empty arguments to
    ## `SummarizedExperiment` generator function. This step will dynamically
    ## handle `rowRanges` and/or `rowData`.
    args <- list(
        "assays" = assays,
        "rowRanges" = rowRanges,
        "rowData" = rowData,
        "colData" = colData,
        "metadata" = metadata
    )
    args <- Filter(f = Negate(is.null), x = args)
    args <- Filter(f = hasLength, x = args)
    se <- do.call(what = SummarizedExperiment, args = args)
        if (isTRUE(sort)) {
        assayNames <- assayNames(se)
        if (hasLength(assayNames)) {
            ## Always keep (raw) counts first, when defined.
            if (isSubset("counts", assayNames)) {
                assayNames <- unique(c("counts", sort(assayNames)))
            } else {
                assayNames <- sort(assayNames)
            }
            assays(se) <- assays(se)[assayNames]
        }
        se <- se[sort(rownames(se)), sort(colnames(se))]
        rowData(se) <- rowData(se)[, sort(colnames(rowData(se))), drop = FALSE]
        colData(se) <- colData(se)[, sort(colnames(colData(se))), drop = FALSE]
        metadata(se) <- metadata(se)[sort(names(metadata(se)))]
    }
    se <- droplevels(se)
    validObject(se)
    se
}
