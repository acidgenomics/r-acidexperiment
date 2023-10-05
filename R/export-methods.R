#' @name export
#' @inherit AcidGenerics::export
#' @note Updated 2023-10-05.
#'
#' @inheritParams AcidRoxygen::params
#' @param ... Additional arguments.
#'
#' @param bindRowData `logical(1)`.
#' Whether to column bind row data (e.g. gene annotations), slotted in
#' `rowData`, to each exported assay matrix, defined in `assays`.
#'
#' @param compress `logical(1)`.
#' Apply gzip compression to all files.
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
#' @note Updated 2022-10-24.
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
            isADirectory(con),
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
                if (is(rowData, "DFrame")) {
                    assay <- as(assay, "DFrame")
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



#' Export DFrame
#'
#' @note Updated 2023-04-27.
#' @noRd
.exportDF <-
    function(object,
             con,
             overwrite,
             quiet) {
        assert(
            is(object, "DFrame"),
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
             bindRowData = FALSE,
             compress = FALSE,
             overwrite = TRUE,
             quiet = FALSE) {
        assert(
            validObject(object),
            hasNoDuplicates(rownames(object)),
            hasNoDuplicates(colnames(object)),
            isString(con),
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
        files[["assays"]] <-
            .exportAssays(
                object = object,
                con = initDir(file.path(con, "assays")),
                bindRowData = bindRowData,
                compress = compress,
                overwrite = overwrite,
                quiet = quiet
            )
        ext <- "csv"
        if (isTRUE(compress)) {
            ext <- paste0(ext, ".gz")
        }
        ext <- paste0(".", ext)
        ## Can coerce `rowRanges` to `DFrame` to include more genomic
        ## coordinate information, rather than calling `rowData` directly.
        rowData <- rowData(object, use.names = TRUE)
        files[["rowData"]] <-
            .exportDF(
                object = rowData,
                con = file.path(con, paste0("rowData", ext)),
                overwrite = overwrite,
                quiet = quiet
            )
        colData <- colData(object)
        files[["colData"]] <-
            .exportDF(
                object = colData,
                con = file.path(con, paste0("colData", ext)),
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
        object = "SummarizedExperiment",
        con = "character"
    ),
    definition = `export,SE`
)
