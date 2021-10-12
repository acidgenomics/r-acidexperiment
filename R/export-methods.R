## FIXME Should we map "dir" to "con" here instead?
## That seems to make more sense. Ensure "dir" argument is deprecated.

## FIXME Need to ensure we're providing legacy support for "name" and "dir"
## arguments here, which are used by bcbioRNASeq and other packages.



#' @name export
#' @inherit pipette::export
#' @note Updated 2021-10-12.
#'
#' @inheritParams AcidRoxygen::params
#' @param compress `logical(1)`.
#'   Apply gzip compression to all files.
#' @param name `character(1)`.
#'   Name to use on disk. If `NULL`, will use the name of the object instead.
#' @param ... Additional arguments.
#'
#' @examples
#' data(RangedSummarizedExperiment, package = "AcidTest")
#'
#' ## SummarizedExperiment ====
#' object <- RangedSummarizedExperiment
#' dir <- file.path(tempdir(), "example")
#' x <- export(object = object, dir = dir)
#' print(x)
#' unlink(dir, recursive = TRUE)
NULL



#' Export assays
#'
#' @note Updated 2021-10-12.
#' @noRd
.exportAssays <-
    function(
        object,
        dir,
        compress,
        overwrite,
        quiet
    ) {
        validObject(object)
        assert(
            is(object, "SummarizedExperiment"),
            isString(dir),
            isFlag(compress),
            isFlag(overwrite),
            isFlag(quiet)
        )
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
#' @note Updated 2021-10-12.
#' @noRd
.exportColData <-
    function(
        object,
        ext,
        dir,
        overwrite,
        quiet
    ) {
        validObject(object)
        assert(
            is(object, "SummarizedExperiment"),
            isString(ext),
            isString(dir),
            isFlag(overwrite),
            isFlag(quiet)
        )
        data <- atomize(colData(object))
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
#' @note Updated 2021-10-12.
#' @noRd
#'
#' @details
#' The standard `rowData()` output is okay but doesn't include genomic ranges
#' coordinates. That's why we're coercing from `rowRanges()` for RSE.
.exportRowData <-
    function(
        object,
        ext,
        dir,
        overwrite,
        quiet
    ) {
        validObject(object)
        assert(
            is(object, "SummarizedExperiment"),
            isString(ext),
            isString(dir),
            isFlag(overwrite),
            isFlag(quiet)
        )
        data <- rowData(object, use.names = TRUE)
        data <- atomize(data)
        ## This step is necessary to keep track of genomic coordinates.
        data <- as.data.frame(data)
        assert(identical(rownames(data), rownames(object)))
        export(
            object = data,
            con = file.path(dir, paste0("rowData", ext)),
            overwrite = overwrite,
            quiet = quiet
        )
    }



## FIXME Allow the user to request CSV or TSV.
## FIXME Need to handle sparseMatrix / Matrix defined in assays.
## FIXME The name argument here now doesn't really make sense. Need to re-work.



#' export SummarizedExperiment method
#'
#' @note Updated 2021-10-12.
#' @noRd
#'
#' @details
#' Ensure the assays list is always named. Note that valid SE objects don't have
#' to contain named assays (e.g. DESeqTransform). In the event that an SE object
#' contains a single, unnamed assay, we make sure to rename it internally to
#' "assay" before exporting.
`export,SE` <-  # nolint
    function(
        object,
        con = getOption("acid.export.dir", default = "."),
        ## FIXME Use a default argument here?
        format = c("csv", "tsv"),
        compress = getOption("acid.export.compress", default = FALSE),
        overwrite = getOption("acid.overwrite", default = TRUE),
        quiet = getOption("acid.quiet", default = FALSE),


        name = NULL,  # deprecated
        dir = NULL  # deprecated
    ) {
        validObject(object)
        if (!is.null(dir)) {
            con <- dir
        }
        assert(
            isString(con),
            isString(name, nullOK = TRUE),
            isFlag(compress),
            isFlag(overwrite),
            isFlag(quiet)
        )
        format <- match.arg(format)
        call <- standardizeCall()
        if (is.null(name)) {
            sym <- call[["object"]]
            assert(is.symbol(sym))
            name <- as.character(sym)
        }
        ## FIXME Need to rename this to "con" instead of "dir".
        dir <- initDir(file.path(dir, name))
        if (!isTRUE(quiet)) {
            alert(sprintf(
                fmt = "Exporting {.envvar %s} to {.path %s}.",
                name, dir
            ))
        }
        files <- list()
        ext <- "csv"
        if (isTRUE(compress)) ext <- paste0(ext, ".gz")
        ext <- paste0(".", ext)
        if (is.null(assayNames(object))) {
            assayNames(object) <- "assay"
        }
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
        assert(hasNames(files))
        invisible(files)
    }



#' @rdname export
#' @export
setMethod(
    f = "export",
    signature = signature(
        object = "SummarizedExperiment",
        con = "character",
        format = "missingOrNULL"  # FIXME
    ),
    definition = `export,SE`
)
