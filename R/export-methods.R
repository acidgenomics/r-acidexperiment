#' @name export
#' @inherit pipette::export
#' @note Updated 2022-05-25.
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
#' con <- file.path(tempdir(), "example")
#' x <- export(object = object, con = con)
#' print(x)
#' unlink(con, recursive = TRUE)
NULL



#' Export assays
#'
#' @note Updated 2022-05-25.
#' @noRd
.exportAssays <-
    function(object,
             dir,
             compress,
             overwrite,
             quiet) {
        assert(
            validObject(object),
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
        assert(isCharacter(assayNames))
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



#' Export column data (in SummarizedExperiment)
#'
#' @note Updated 2022-05-25.
#' @noRd
.exportColData <-
    function(object,
             ext,
             dir,
             overwrite,
             quiet) {
        validObject(object)
        assert(
            is(object, "SummarizedExperiment"),
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
        assert(identical(rownames(data), colnames(object)))
        export(
            object = data,
            con = file.path(dir, paste0("colData", ext)),
            overwrite = overwrite,
            quiet = quiet
        )
    }



#' Export row data (in SummarizedExperiment)
#'
#' @note Updated 2022-05-25.
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
            validObject(object),
            is(object, "SummarizedExperiment"),
            isString(ext),
            isString(dir),
            isFlag(overwrite),
            isFlag(quiet)
        )
        data <- rowData(object, use.names = TRUE)
        if (!hasCols(data)) {
            return(NULL)
        }
        ## This step is necessary to keep track of genomic coordinates.
        data <- as.data.frame(data)
        data <- atomize(data)
        if (!hasCols(data)) {
            return(NULL)
        }
        assert(identical(rownames(data), rownames(object)))
        export(
            object = data,
            con = file.path(dir, paste0("rowData", ext)),
            overwrite = overwrite,
            quiet = quiet
        )
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
             format, # NULL
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



## Updated 2022-05-25.
`export,SE,deprecated` <- # nolint
    function(object,
             con, # NULL,
             format, # NULL,
             name = NULL,
             dir,
             ...) {
        ## > .Deprecated(msg = sprintf(
        ## >     "Use '%s' instead of '%s'.",
        ## >     "con", "dir"
        ## > ))
        if (missing(con)) {
            con <- NULL
        }
        if (missing(format)) {
            format <- NULL
        }
        assert(
            validObject(object),
            is.null(con),
            is.null(format),
            isString(dir)
        )
        if (is.null(name)) {
            call <- standardizeCall()
            sym <- call[["object"]]
            assert(is.symbol(sym))
            name <- as.character(sym)
        }
        export(
            object = object,
            con = file.path(dir, name),
            format = format,
            ...
        )
    }



#' @rdname export
#' @export
setMethod(
    f = "export",
    signature = signature(
        object = "SummarizedExperiment",
        con = "character",
        format = "missingOrNULL"
    ),
    definition = `export,SE`
)

#' @rdname export
#' @export
setMethod(
    f = "export",
    signature = signature(
        object = "SummarizedExperiment",
        con = "missingOrNULL",
        format = "missingOrNULL"
    ),
    definition = `export,SE,deprecated`
)
