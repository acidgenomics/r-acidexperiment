#' Import sample metadata
#'
#' This function imports user-defined sample metadata saved in a spreadsheet.
#'
#' @section bcbio pipeline:
#'
#' **Required column names.** The `"description"` column is always required, and
#' must match the bcbio per sample directory names exactly. Inclusion of the
#' `"fileName"` column isn't required but is recommended for data provenance.
#' Note that some bcbio examples on readthedocs use `"samplename"` (note case)
#' instead of `"fileName"`. This function checks for that and will rename the
#' column to `"fileName"` automatically. We're using the `sampleName` column
#' (note case) to define unique sample names, in the event that bcbio has
#' processed multiplexed samples.
#'
#' **Demultiplexed samples.** The samples in the bcbio run must map to the
#' `"description"` column. The values provided in description for demultiplexed
#' samples must be unique. They must also be *syntactically valid*, meaning that
#' they cannot contain illegal characters (e.g. spaces, non-alphanumerics,
#' *dashes*) or *begin with a number*. Consult the documentation in `help(topic
#' = "make.names")` for more information on valid names in R.
#'
#' **Multiplexed samples.** This applies to some single-cell RNA-seq formats,
#' including inDrops. In this case, bcbio will output per-sample directories
#' with this this structure: `description-revcomp`. The function checks to
#' see if the `"description"` column is unique. If the values are duplicated,
#' the function assumes that bcbio processed multiplexed FASTQs, where multiple
#' samples of interest are barcoded inside a single FASTQ. This this case, you
#' must supply additional `"index"`, `"sequence"`, and `"sampleName"` columns.
#' Note that bcbio currently outputs the reverse complement index sequence in
#' the sample directory names (e.g. `"sample-ATAGAGAG"`). Define the forward
#' index barcode in the `sequence` column here, not the reverse complement. The
#' reverse complement will be calculated automatically and added as the
#' `revcomp` column in the sample metadata.
#'
#' @note Works with local or remote files.
#'
#' @author Michael Steinbaugh
#' @note Updated 2022-05-04.
#' @export
#'
#' @inheritParams AcidRoxygen::params
#'
#' @param lanes `integer(1)`.
#' Number of lanes used to split the samples into technical replicates
#' suffix (i.e. `_LXXX`).
#'
#' @param pipeline `character(1)`.
#' Analysis pipeline:
#' - `"none"`: Simple mode, requiring only "sampleId" column.
#' - `"bcbio"`: bcbio mode. See section here in documentation for details.
#' - `"cellranger"`: Cell Ranger mode. Currently requires "directory" column.
#' Used by Chromium R package.
#'
#' @param autopadZeros `logical(1)`.
#' Autopad zeros in sample identifiers, for improved sorting.
#' Currently supported only for non-multiplexed samples.
#' For example: `sample_1`, `sample_2`, ... `sample_10` becomes
#' `sample_01`, `sample_02`, ... `sample10`.
#'
#' @param ... Passthrough arguments to `import` method.
#' For example, supports `sheet` argument for Microsoft Excel files.
#'
#' @return `DataFrame`.
#'
#' @examples
#' ## Demultiplexed ====
#' file <- file.path(
#'     AcidExperimentTestsURL,
#'     "bcbio-metadata-demultiplexed.csv"
#' )
#' x <- importSampleData(file, pipeline = "bcbio")
#' print(x)
#'
#' ## Multiplexed ====
#' file <- file.path(
#'     AcidExperimentTestsURL,
#'     "bcbio-metadata-multiplexed-indrops.csv"
#' )
#' x <- importSampleData(file, pipeline = "bcbio")
#' print(x)
importSampleData <-
    function(file,
             lanes = 0L,
             pipeline = c("none", "bcbio", "cellranger"),
             autopadZeros = FALSE,
             ...) {
        ## Coerce `detectLanes()` empty integer return to 0.
        if (!hasLength(lanes)) {
            lanes <- 0L # nocov
        }
        assert(
            isAFile(file) || isAURL(file),
            isInt(lanes),
            isNonNegative(lanes),
            isFlag(autopadZeros)
        )
        lanes <- as.integer(lanes)
        pipeline <- match.arg(pipeline)
        requiredCols <- switch(
            EXPR = pipeline,
            "none" = "sampleId",
            "bcbio" = "description",
            "cellranger" = "directory"
        )
        ## Convert lanes to a sequence, if necessary.
        if (hasLength(lanes, n = 1L) && isTRUE(lanes > 1L)) {
            lanes <- seq_len(lanes)
        }
        ## Import --------------------------------------------------------------
        data <- import(file, ...)
        data <- as(data, "DataFrame")
        colnames(data) <- camelCase(colnames(data), strict = TRUE)
        data <- removeNA(data)
        ## Manual "sampleId" column not allowed for bcbio or Cell Ranger input.
        if (isSubset(pipeline, c("bcbio", "cellranger"))) {
            assert(areDisjointSets("sampleId", colnames(data)))
        }
        switch(
            EXPR = pipeline,
            "none" = {
                idCol <- "sampleId"
            },
            "bcbio" = {
                ## Look for bcbio "samplename" column and rename to "fileName".
                if (isSubset("samplename", colnames(data))) {
                    alertWarning("Renaming 'samplename' column to 'fileName'.")
                    assert(areDisjointSets(x = "fileName", y = colnames(data)))
                    colnames(data)[colnames(data) == "samplename"] <- "fileName"
                }
                idCol <- "description"
            },
            "cellranger" = {
                ## nocov start
                ## Consider renaming this to `sampleId`, for consistency.
                idCol <- "directory"
                ## nocov end
            }
        )
        ## Check that input passes denylist, and has all required columns.
        assert(
            .isSampleData(object = data, requiredCols = requiredCols),
            isString(idCol), isSubset(idCol, colnames(data))
        )
        ## Valid rows must contain a non-empty sample identifier.
        data <- data[!is.na(data[[idCol]]), , drop = FALSE]
        ## Determine whether the samples are multiplexed.
        if (
            isSubset(c("index", "sequence"), colnames(data)) &&
                (any(duplicated(data[[idCol]])) || identical(nrow(data), 1L))
        ) {
            multiplexed <- TRUE
            alertInfo("Multiplexed samples detected.")
            requiredCols <- c(requiredCols, "sampleName", "index")
            ## Note that sample ID column is now expected to have duplicates.
            assert(
                isSubset(requiredCols, colnames(data)),
                hasNoDuplicates(data[["sampleName"]])
            )
        } else if (hasNoDuplicates(data[[idCol]])) {
            multiplexed <- FALSE
            ## Note that `sampleName` column isn't required for demultiplexed
            ## samples. We can assign from bcbio `description` automatically.
            if (!"sampleName" %in% colnames(data)) {
                data[["sampleName"]] <- data[[idCol]]
            }
            ## Requiring syntactically valid names on direct "sampleId" input.
            ## Sanitize sample IDs into snake case, if necessary.
            if (
                identical(idCol, "sampleId") &&
                    !validNames(unique(data[[idCol]]))
            ) {
                alertInfo(sprintf(
                    fmt = paste0(
                        "Sanitizing sample identifiers defined in ",
                        "{.var %s} column into snake case."
                    ),
                    idCol
                ))
                data[[idCol]] <- snakeCase(data[[idCol]])
            }
            ## Autopad zeros in sample IDs to improve sorting.
            if (isTRUE(autopadZeros)) {
                data[[idCol]] <- autopadZeros(data[[idCol]]) # nocov
            }
        } else {
            abort(sprintf(
                fmt = paste(
                    "Sample data input file is malformed.",
                    "Refer to {.fun %s} for formatting requirements.",
                    sep = "\n"
                ),
                "importSampleData"
            ))
        }
        ## Multiplexed samples -------------------------------------------------
        ## This step applies to handling single-cell metadata.
        ## - bcbio subdirs (e.g. inDrops): `description`-`revcomp`.
        ## - Note that forward `sequence` is required in metadata file.
        ## - Index number is also required here for data preservation, but is
        ## not used in generation of the sample directory names.
        ## - Require at least 6 nucleotides in the index sequence.
        ## - inDrops currently uses 8 but SureCell uses 6.
        if (identical(pipeline, "bcbio") && isTRUE(multiplexed)) {
            requireNamespaces("Biostrings")
            assert(isSubset(c("index", "sequence"), colnames(data)))
            sequence <- data[["sequence"]]
            assert(allAreMatchingRegex(sequence, pattern = "^[ACGT]{6,}"))
            data[["revcomp"]] <- vapply(
                X = sequence,
                FUN = function(x) {
                    x <- as(x, "character")
                    x <- as(x, "DNAStringSet")
                    x <- Biostrings::reverseComplement(x)
                    x <- as(x, "character")
                    x
                },
                FUN.VALUE = character(1L),
                USE.NAMES = FALSE
            )
            ## Match the sample directories exactly here, using the hyphen.
            data[[idCol]] <- paste(data[[idCol]], data[["revcomp"]], sep = "-")
        }
        ## Lane-split replicates -----------------------------------------------
        ## Prepare metadata for lane split replicates. This step will expand
        ## rows into the number of desired replicates (e.g. "L001").
        ## `lapply()` approach here inspired by `mefa::rep.data.frame()`.
        if (isTRUE(length(lanes) > 1L)) {
            assert(requireNamespaces("stringi"))
            split <- split(data, f = data[[idCol]])
            split <- SplitDataFrameList(lapply(
                X = split,
                FUN = function(x) {
                    x <- rep(x, times = length(lanes))
                    x[["lane"]] <- paste0(
                        "L",
                        stringi::stri_pad_left(
                            str = lanes,
                            width = 3L,
                            pad = "0"
                        )
                    )
                    x
                }
            ))
            data <- unsplit(split, f = unlist(split[, idCol]))
            pasteLanes <- function(nameCol, laneCol) {
                makeNames(paste(nameCol, laneCol, sep = "_"), unique = FALSE)
            }
            nameCols <- c(idCol, "sampleName")
            data <- mutateAt(
                object = data,
                vars = nameCols,
                fun = pasteLanes,
                laneCol = data[["lane"]]
            )
            ## Fix the lane-split bcbio description. This is an uncommon edge
            ## case, but we're still providing support here.
            ## Example: `indrops1_AGAGGATA_L001` to `indrops1_L001_AGAGGATA`.
            if (identical(pipeline, "bcbio") && isTRUE(multiplexed)) {
                assert(requireNamespaces("stringi"))
                match <- stringi::stri_match_first_regex(
                    str = data[["description"]],
                    pattern = paste0(
                        "^",
                        "(.+)",
                        "_",
                        "(", data[["revcomp"]], ")",
                        "_",
                        "(", data[["lane"]], ")",
                        "$"
                    )
                )
                data[["description"]] <- apply(
                    X = match,
                    MARGIN = 1L,
                    FUN = function(x) {
                        paste0(x[[2L]], "_", x[[4L]], "_", x[[3L]])
                    }
                )
            }
        }
        ## Return.
        rownames(data) <- makeNames(data[[idCol]], unique = TRUE)
        makeSampleData(data)
    }



## Sample metadata assert check for goalie engine.
## Updated 2021-09-01.
.isSampleData <- function(object, requiredCols = "sampleName") {
    assert(isCharacter(requiredCols))
    ok <- isAny(object, c("data.frame", "DataFrame"))
    if (!isTRUE(ok)) {
        return(ok) # nocov
    }
    ok <- hasRows(object)
    if (!isTRUE(ok)) {
        return(ok) # nocov
    }
    ## Check for denylist columns.
    intersect <- intersect(colnames(object), metadataDenylist)
    ok <- !hasLength(intersect)
    if (!isTRUE(ok)) {
        return(false(sprintf(
            fmt = paste0(
                "Denylist columns detected: %s.\n",
                "Refer to '%s' for formatting requirements."
            ),
            toString(intersect, width = 100L),
            "importSampleData()"
        )))
    }
    ## Check for required columns (e.g. description).
    ok <- isSubset(requiredCols, colnames(object))
    if (!isTRUE(ok)) {
        setdiff <- setdiff(requiredCols, colnames(object))
        return(false(sprintf(
            fmt = paste0(
                "Required columns missing: %s.\n",
                "Refer to '%s' for formatting requirements."
            ),
            toString(setdiff, width = 100L),
            "importSampleData()"
        )))
    }
    TRUE
}
