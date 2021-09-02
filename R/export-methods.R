#' @name export
#' @inherit pipette::export
#' @note Updated 2021-09-02.
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
#' dir <- "example"
#' x <- export(object = object, dir = dir)
#' print(x)
#' unlink(dir, recursive = TRUE)
NULL



#' Export assays
#'
#' @note Updated 2021-09-02.
#' @noRd
.exportAssays <-
    function(object, name, dir, compress, overwrite, quiet) {
        validObject(object)
        assert(
            is(object, "SummarizedExperiment"),
            isString(name),
            isString(dir),
            isFlag(compress),
            isFlag(overwrite),
            isFlag(quiet)
        )
        assayNames <- assayNames(object)
        assert(isCharacter(assayNames))
        dir <- realpath(initDir(dir))
        alert(sprintf(
            fmt = "Exporting assays %s to {.path %s}.",
            toInlineString(assayNames, n = 5L, class = "val"), dir
        ))
        out <- lapply(
            X = assayNames,
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
                    file = file,
                    overwrite = overwrite,
                    quiet = quiet
                )
            },
            dir = initDir(file.path(dir, "assays"))
        )
        names(out) <- assayNames
        out
    }



#' Export column data (in SummarizedExperiment)
#'
#' @note Updated 2020-08-11.
#' @noRd
.exportColData <-
    function(object, ext, dir, overwrite, quiet) {
        validObject(object)
        assert(
            is(object, "SummarizedExperiment"),
            isString(ext),
            isString(dir),
            isFlag(overwrite),
            isFlag(quiet)
        )
        export(
            object = atomize(colData(object)),
            file = file.path(dir, paste0("colData", ext)),
            overwrite = overwrite,
            quiet = quiet
        )
    }



#' Export row data (in SummarizedExperiment)
#'
#' @note Updated 2020-08-11.
#' @noRd
#'
#' @details
#' The standard `rowData()` output is okay but doesn't include genomic ranges
#' coordinates. That's why we're coercing from `rowRanges()` for RSE.
.exportRowData <-
    function(object, ext, dir, overwrite, quiet) {
        validObject(object)
        assert(
            is(object, "SummarizedExperiment"),
            isString(ext),
            isString(dir),
            isFlag(overwrite),
            isFlag(quiet)
        )
        data <- rowData(object)
        ## Note that SummarizedExperiment in BioC 3.6/R 3.4 release doesn't
        ## set row names properly, so keep this step here for compatibility.
        if (!hasRownames(data)) {
            rownames(data) <- rownames(object)  # nocov
        }
        data <- atomize(data)
        data <- as.data.frame(data)
        assert(identical(rownames(data), rownames(object)))
        export(
            object = data,
            file = file.path(dir, paste0("rowData", ext)),
            overwrite = overwrite,
            quiet = quiet
        )
    }



#' export SummarizedExperiment method
#'
#' @note Updated 2020-08-11
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
        name = NULL,
        dir,
        compress,
        overwrite,
        quiet
    ) {
        validObject(object)
        assert(
            isString(name, nullOK = TRUE),
            isString(dir),
            isFlag(compress),
            isFlag(overwrite),
            isFlag(quiet)
        )
        call <- standardizeCall()
        if (is.null(name)) {
            sym <- call[["object"]]
            assert(is.symbol(sym))
            name <- as.character(sym)
        }
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
                name = name,
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

formals(`export,SE`)[
    c("compress", "dir", "overwrite", "quiet")] <-
    formalsList[c("export.compress", "export.dir", "overwrite", "quiet")]



#' @rdname export
#' @export
setMethod(
    f = "export",
    signature = signature("SummarizedExperiment"),
    definition = `export,SE`
)
