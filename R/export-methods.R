## FIXME Rework internal usage of "dir" here, switching to "con" instead.



#' @name export
#' @inherit pipette::export
#' @note Updated 2022-09-22.
#'
#' @inheritParams AcidRoxygen::params
#' @param ... Additional arguments.
#'
#' @param compress `logical(1)`.
#' Apply gzip compression to all files.
#'
#' @param name `character(1)`.
#' Name to use on disk. If `NULL`, will use the name of the object instead.
#'
#' @examples
#' data(RangedSummarizedExperiment, package = "AcidTest")
#'
#' ## SummarizedExperiment ====
#' object <- RangedSummarizedExperiment
#' con <- AcidBase::tempdir2()
#' x <- export(object = object, con = con)
#' print(x)
#' AcidBase::unlink2(con)
NULL



## FIXME Add an option to include rowData here.
## FIXME Consider binding this before the assay (useful in particular for
## MAE objects, to include the gene identifiers and symbols).
## FIXME Ensure all calls set rowData appropriately.
## FIXME Consider renaming "dir" to "con" internally.
## FIXME Rework this, as we've already defined assays as the target.

#' Export assays
#'
#' @note Updated 2022-09-22.
#' @noRd
.exportAssays <-
    function(object,
             con,
             bindRowData,
             compress,
             overwrite,
             quiet) {
        assert(
            is(object, "SummarizedExperiment"),
            hasNoDuplicates(rownames(object)),
            hasNoDuplicates(colnames(object)),
            isADir(con),
            isFlag(bindRowData),
            isFlag(compress),
            isFlag(overwrite),
            isFlag(quiet)
        )
        if (!hasLength(assays(object))) {
            return(NULL)
        }
        if (
            is.null(assayNames(object)) &&
                hasLength(assays(object), n = 1L)
        ) {
            assayNames(object) <- "assay"
        }
        assayNames <- assayNames(object)
        assert(
            isCharacter(assayNames),
            hasNoDuplicates(assayNames)
        )
        alert(sprintf(
            fmt = "Exporting assays %s to {.path %s}.",
            toInlineString(assayNames, n = 5L, class = "val"), con
        ))
        if (isTRUE(bindRowData)) {
            rowData <- rowData(object)
        } else {
            rowData <- NULL
        }
        out <- lapply(
            X = assayNames,
            con = con,
            rowData = rowData,
            FUN = function(name, con, rowData) {
                con <- file.path(con, name)
                assay <- assay(x = object, i = name)
                if (is(assay, "Matrix")) {
                    ext <- "mtx"
                } else {
                    ext <- "csv"
                }
                if (isTRUE(compress)) {
                    ext <- paste0(ext, ".gz")
                }
                con <- paste0(con, ".", ext)
                if (is(rowData, "DataFrame")) {
                    assay <- as(assay, "DataFrame")
                    assay <- cbind(rowData, assay)
                }
                export(
                    object = assay,
                    con = con,
                    overwrite = overwrite,
                    quiet = quiet
                )
            }
        )
        names(out) <- assayNames
        out
    }



## FIXME Generalize this between colData and rowData...same code.

#' Export column data
#'
#' @note Updated 2022-09-22.
#' @noRd
.exportColData <-
    function(object,
             con,
             overwrite,
             quiet) {
        assert(
            is(object, "DataFrame"),
            isString(con),
            isFlag(overwrite),
            isFlag(quiet)
        )
        if (!hasCols(object)) {
            return(NULL)
        }
        ## FIXME What about as.data.frame coercion here?
        object <- atomize(object)
        if (!hasCols(object)) {
            return(NULL)
        }
        if (hasColnames(object)) {
            assert(identical(rownames(data), colnames(object)))
        }
        export(
            object = data,
            con = file.path(dir, paste0("colData", ext)),
            overwrite = overwrite,
            quiet = quiet
        )
    }



## FIXME Consider slotting the colData in per nested SE, to make the flat
## file exports easier to parse programatically.
##
## FIXME If rowData contains "Hugo_Symbol","Entrez_Gene_Id", consider assigning
## rownames from the `Entrez_GeneId`, but only if there are no duplicates.
## This is cBioPortalData specific, so maybe don't do this here...
## FIXME Consider exporting with rowData bound to assay automatically.

#' Export MultiAssayExperiment experiments
#'
#' @note Updated 2022-09-21.
#' @noRd
.exportExperiments <-
    function(object,
             dir,
             compress,
             overwrite,
             quiet) {
        assert(
            is(object, "MultiAssayExperiment"),
            isString(dir),
            isFlag(compress),
            isFlag(overwrite),
            isFlag(quiet)
        )
        exp <- experiments(object)
        assert(
            is(exp, "ExperimentList"),
            hasNames(exp)
        )
        ## Drop complex objects, such as RaggedExperiment.
        keep <- bapply(X = exp, FUN = is, class2 = "SummarizedExperiment")
        exp <- exp[keep]
        if (!hasLength(exp)) {
            return(NULL)
        }
        assert(hasNoDuplicates(names(exp)))
        ## Ensure nested objects do not contain duplicated dimnames. This has
        ## been observed with cBioPortalData MAE objects.
        exp <- lapply(
            X = exp,
            FUN = function(object) {
                assert(hasNoDuplicates(colnames(object)))
                ## FIXME Can we rework using goalie::hasDuplicates instead?
                if (anyDuplicated(rownames(object)) > 0L) {
                    alertWarning("Duplicate rownames detected.")
                    rownames(object) <- NULL
                }
                object
            }
        )
        Map(
            f = export,
            object = exp,
            con = file.path(dir, "experiments", names(exp)),
            compress = compress,
            overwrite = overwrite,
            quiet = quiet
        )
    }



#' Export row data
#'
#' @note Updated 2022-09-22.
#' @noRd
#'
#' @details
#' The standard `rowData()` output is okay but doesn't include genomic ranges
#' coordinates. That's why we're coercing from `rowRanges()` for RSE.
.exportRowData <-
    function(object,
             con,
             overwrite,
             quiet) {
        assert(
            is(object, "DataFrame"),
            isString(con),
            isFlag(overwrite),
            isFlag(quiet)
        )
        if (!hasCols(object)) {
            return(NULL)
        }
        rn <- rownames(object)
        object <- as.data.frame(object)
        object <- atomize(object)
        if (!hasCols(object)) {
            return(NULL)
        }
        if (hasRownames(object)) {
            assert(identical(rownames(object), rn))
        }
        export(
            object = object,
            con = con,
            overwrite = overwrite,
            quiet = quiet
        )
    }



## FIXME Add option for `bindRowData` here. Set as FALSE by default.
## FIXME Rework usage of "dir" here, switching to "con".

#' Export MultiAssayExperiment
#'
#' @note Updated 2022-09-22.
#' @noRd
`export,MAE` <- # nolint
    function(object,
             con,
             format, # missing
             bindRowData = FALSE,
             compress = getOption(
                 x = "acid.export.compress",
                 default = FALSE
             ),
             overwrite = getOption(
                 x = "acid.overwrite",
                 default = TRUE
             ),
             quiet = getOption(
                 x = "acid.quiet",
                 default = FALSE
             )) {
        if (missing(format)) {
            format <- NULL
        }
        assert(
            validObject(object),
            isString(con),
            is.null(format),
            isFlag(bindRowData),
            isFlag(compress),
            isFlag(overwrite),
            isFlag(quiet)
        )
        dir <- initDir(con)
        if (!isTRUE(quiet)) {
            alert(sprintf(
                fmt = "Exporting {.cls %s} to {.path %s}.",
                "MultiAssayExperiment", dir
            ))
        }
        files <- list()
        ## This extension only applies to colData and rowData below.
        ext <- "csv"
        if (isTRUE(compress)) {
            ext <- paste0(ext, ".gz")
        }
        ext <- paste0(".", ext)
        colData <- colData(object)
        if (anyDuplicated(rownames(colData)) > 0L) {
            alertWarning(sprintf(
                "Duplicate column identifiers detected in {.var %s}.",
                "colData"
            ))
            rownames(colData) <- NULL
        }
        ## FIXME Switch to `.exportColData` call here.
        ## FIXME Do it on the colData directly here?
        files[["colData"]] <-
            export(
                object = colData,
                con = file.path(dir, paste0("colData", ext)),
                overwrite = overwrite,
                quiet = quiet
            )
        sampleMap <- sampleMap(object)
        assert(
            is(sampleMap, "DataFrame"),
            hasNoDuplicates(rownames(sampleMap)),
            hasNoDuplicates(colnames(sampleMap))
        )
        files[["sampleMap"]] <-
            export(
                object = sampleMap,
                con = file.path(dir, paste0("sampleMap", ext)),
                overwrite = overwrite,
                quiet = quiet
            )
        files[["experiments"]] <-
            .exportExperiments(
                object = object,
                dir = dir,
                compress = compress,
                overwrite = overwrite,
                quiet = quiet
            )
        files <- Filter(Negate(is.null), files)
        invisible(files)
    }



## FIXME Need to add support for `bindRowData` here. Set as FALSE by default.

#' Export SummarizedExperiment
#'
#' @note Updated 2022-09-22.
#' @noRd
#'
#' @details
#' Ensure the assays list is always named. Note that valid SE objects don't have
#' to contain named assays (e.g. DESeqTransform). In the event that an SE object
#' contains a single, unnamed assay, we make sure to rename it internally to
#' "assay" before exporting.
`export,SE` <- # nolint
    function(object,
             con,
             format, # missing
             bindRowData = FALSE,
             compress = getOption(
                 x = "acid.export.compress",
                 default = FALSE
             ),
             overwrite = getOption(
                 x = "acid.overwrite",
                 default = TRUE
             ),
             quiet = getOption(
                 x = "acid.quiet",
                 default = FALSE
             )) {
        if (missing(format)) {
            format <- NULL
        }
        assert(
            validObject(object),
            hasNoDuplicates(rownames(object)),
            hasNoDuplicates(colnames(object)),
            isString(con),
            is.null(format),
            isFlag(bindRowData),
            isFlag(compress),
            isFlag(overwrite),
            isFlag(quiet)
        )
        con <- initDir(con)
        if (!isTRUE(quiet)) {
            alert(sprintf(
                fmt = "Exporting {.cls %s} to {.path %s}.",
                "SummarizedExperiment", con
            ))
        }
        files <- list()
        ## This extension only applies to colData and rowData below.
        ext <- "csv"
        if (isTRUE(compress)) {
            ext <- paste0(ext, ".gz")
        }
        ext <- paste0(".", ext)
        files[["assays"]] <-
            .exportAssays(
                object = object,
                con = initDir(file.path(con, "assays")),
                bindRowData = bindRowData,
                compress = compress,
                overwrite = overwrite,
                quiet = quiet
            )
        ## FIXME Do it on the colData directly instead here.
        files[["colData"]] <-
            .exportColData(
                object = object,
                con = file.path(con, paste0("colData.", ext)),
                overwrite = overwrite,
                quiet = quiet
            )
        ## FIXME Do it directly on the rowData instead here.
        files[["rowData"]] <-
            .exportRowData(
                object = rowData(object, use.names = TRUE),
                con = file.path(con, paste0("rowData.", ext)),
                overwrite = overwrite,
                quiet = quiet
            )
        files <- Filter(Negate(is.null), files)
        invisible(files)
    }



#' @rdname export
#' @export
setMethod(
    f = "export",
    signature = signature(
        object = "MultiAssayExperiment",
        con = "character",
        format = "missing"
    ),
    definition = `export,MAE`
)

#' @rdname export
#' @export
setMethod(
    f = "export",
    signature = signature(
        object = "SummarizedExperiment",
        con = "character",
        format = "missing"
    ),
    definition = `export,SE`
)
