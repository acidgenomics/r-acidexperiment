#' @name export
#' @inherit pipette::export
#' @note Updated 2022-09-20.
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



#' Export assays
#'
#' @note Updated 2022-09-20.
#' @noRd
.exportAssays <-
    function(object,
             dir,
             compress,
             overwrite,
             quiet) {
        assert(
            is(object, "SummarizedExperiment"),
            isString(dir),
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
        dir <- initDir(dir)
        alert(sprintf(
            fmt = "Exporting assays %s to {.path %s}.",
            toInlineString(assayNames, n = 5L, class = "val"), dir
        ))
        out <- lapply(
            X = assayNames,
            dir = initDir(file.path(dir, "assays")),
            FUN = function(name, dir) {
                file <- file.path(dir, name)
                assay <- assay(x = object, i = name)
                if (is(assay, "Matrix")) {
                    ext <- "mtx"
                } else {
                    ext <- "csv"
                }
                if (isTRUE(compress)) {
                    ext <- paste0(ext, ".gz")
                }
                file <- paste0(file, ".", ext)
                export(
                    object = assay,
                    con = file,
                    overwrite = overwrite,
                    quiet = quiet
                )
            }
        )
        names(out) <- assayNames
        out
    }



#' Export column data
#'
#' @note Updated 2022-09-20.
#' @noRd
.exportColData <-
    function(object,
             ext,
             dir,
             overwrite,
             quiet) {
        assert(
            is(object, "SummarizedExperiment"),
            hasNoDuplicates(colnames(object)),
            isString(ext),
            isString(dir),
            isFlag(overwrite),
            isFlag(quiet)
        )
        data <- colData(object)
        if (!hasCols(data)) {
            return(NULL)
        }
        data <- atomize(data)
        if (!hasCols(data)) {
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



#' Export MultiAssayExperiment experiments
#'
#' @note Updated 2022-09-20.
#' @noRd
.exportExperiments <-
    function(object,
             ext,
             dir,
             overwrite,
             quiet) {
        assert(
            is(object, "MultiAssayExperiment"),
            isString(ext),
            isString(dir),
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
        ## Ensure nested objects contain unique dimanmes, which is not always
        ## the case currently for cBioPortalData objects.
        exp <- lapply(X = exp, FUN = makeDimnames)
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
#' @note Updated 2022-09-20.
#' @noRd
#'
#' @details
#' The standard `rowData()` output is okay but doesn't include genomic ranges
#' coordinates. That's why we're coercing from `rowRanges()` for RSE.
.exportRowData <-
    function(object,
             ext,
             dir,
             overwrite,
             quiet) {
        assert(
            is(object, "SummarizedExperiment"),
            hasNoDuplicates(rownames(object)),
            isString(ext),
            isString(dir),
            isFlag(overwrite),
            isFlag(quiet)
        )
        data <- rowData(object, use.names = TRUE)
        if (!hasCols(data)) {
            return(NULL)
        }
        data <- as.data.frame(data)
        data <- atomize(data)
        if (!hasCols(data)) {
            return(NULL)
        }
        if (hasRownames(object)) {
            assert(identical(rownames(data), rownames(object)))
        }
        export(
            object = data,
            con = file.path(dir, paste0("rowData", ext)),
            overwrite = overwrite,
            quiet = quiet
        )
    }



#' Export MultiAssayExperiment
#'
#' @note Updated 2022-09-20.
#' @noRd
`export,MAE` <- # nolint
    function(object,
             con,
             format, # missing
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
                ext = ext,
                dir = dir,
                overwrite = overwrite,
                quiet = quiet
            )
        files <- Filter(Negate(is.null), files)
        invisible(files)
    }



#' Export SummarizedExperiment
#'
#' @note Updated 2022-05-25.
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
            isFlag(compress),
            isFlag(overwrite),
            isFlag(quiet)
        )
        dir <- initDir(con)
        if (!isTRUE(quiet)) {
            alert(sprintf(
                fmt = "Exporting {.cls %s} to {.path %s}.",
                "SummarizedExperiment", dir
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
                dir = dir,
                compress = compress,
                overwrite = overwrite,
                quiet = quiet
            )
        files[["colData"]] <-
            .exportColData(
                object = object,
                ext = ext,
                dir = dir,
                overwrite = overwrite,
                quiet = quiet
            )
        files[["rowData"]] <-
            .exportRowData(
                object = object,
                ext = ext,
                dir = dir,
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
