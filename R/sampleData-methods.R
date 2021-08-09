#' @name sampleData
#' @inherit AcidGenerics::sampleData
#' @note Updated 2021-02-22.
#'
#' @details
#' All columns defined in `colData` of the object must be named in strict
#' lower camel case, otherwise this function will intentionally error.
#'
#' @section All supported S4 classes:
#'
#' Illegal `colData`:
#'
#' - `interestingGroups`: Generated automatically, based on the criteria
#'   slotted into the object using `interestingGroups()`. The function will
#'   error intentionally if this column is manually defined in `colData()`.
#'
#' Recommended `colData`:
#'
#' - `sampleName`: Human readable sample names used by basejump plotting
#'   functions in favor of object column names, which should be syntactically
#'   valid (but not always very readable). See
#'   `make.names()` for more information on syntactically valid names. Note that
#'   if this column is not defined in the object, it will be returned
#'   automatically by `sampleData()`.
#'
#' @section SummarizedExperiment:
#'
#' Required `colData`:
#'
#' - None.
#'
#' Illegal `colData`:
#'
#' - `sampleId`: Redundant; already defined in the object column names.
#'
#' @inheritParams AcidRoxygen::params
#' @param clean `logical(1)`.
#'   Only return `factor` columns. Useful when working with objects that contain
#'   quality control metrics in `colData()`. For example, `bcbioRNASeq` and
#'   `DESeqDataSet` objects often contain additional columns that aren't
#'   informative sample metadata.
#' @param ignoreCols `character` or `NULL`.
#'   Only applies when `clean = TRUE`. Additional factor columns defined in
#'   `colData` to be ignored as sample-level metadata.
#'   Particularly useful for `SingleCellExperiment` objects, where
#'   cell-to-sample mappings are defined using the `sampleId` column.
#' @param ... Additional arguments.
#'
#' @examples
#' data(RangedSummarizedExperiment, package = "AcidTest")
#'
#' ## SummarizedExperiment ====
#' object <- RangedSummarizedExperiment
#' sampleData(object)
#'
#' ## Assignment support.
#' sampleData(object)[["batch"]] <- 1L
#' ## `batch` column should be now defined.
#' sampleData(object)
NULL



## Don't run validity checks here.
## Updated 2021-02-08.
`sampleData,SE` <-  # nolint
    function(
        object,
        clean = TRUE,
        ignoreCols = c(
            "^description$",
            "^genomeBuild$",
            "^qualityFormat$",
            "^samRef$"
        )
    ) {
        data <- colData(object)
        if (!hasRows(data)) return(data)
        assert(
            hasRownames(data),
            isFlag(clean),
            isCharacter(ignoreCols, nullOK = TRUE)

        )
        ## This provides backward compatibility for legacy objects, such as
        ## bcbioRNASeq.
        if (hasColnames(data)) {
            colnames(data) <- camelCase(colnames(data), strict = TRUE)
        }
        ## Require `sampleName` column.
        if (!isSubset("sampleName", colnames(data))) {
            ## Bioconductor 3.10 is converting to "DFrame" class here.
            data[["sampleName"]] <- as.factor(rownames(data))
        } else if (!is.factor(data[["sampleName"]])) {
            stop("'sampleData()' requires 'sampleName' factor in 'colData()'.")
        }
        ## Clean mode.
        if (isTRUE(clean)) {
            ## Return only a subset of factor columns.
            keep <- bapply(X = data, FUN = is.factor)
            data <- data[, keep, drop = FALSE]
            ## Drop any additional uninformative columns to ignore.
            if (is.character(ignoreCols)) {
                keep <- !grepl(
                    pattern = paste(ignoreCols, collapse = "|"),
                    x = camelCase(colnames(data), strict = TRUE)
                )
                data <- data[, keep, drop = FALSE]
            }
        }
        ## Add interesting groups column.
        data <- uniteInterestingGroups(
            object = data,
            interestingGroups = matchInterestingGroups(object)
        )
        ## Return.
        assert(
            is.factor(data[["interestingGroups"]]),
            is.factor(data[["sampleName"]])
        )
        data
    }



## nolint start
##
## Note that attempting to use `NULL` to remove columns on a DataFrame
## will result in `S4Vectors::V_recycle()` errors, prior to BioC 3.8.
## https://stat.ethz.ch/pipermail/bioc-devel/2017-November/012343.html
##
## nolint end



## Updated 2021-02-25.
`sampleData<-,SE,DataFrame` <-  # nolint
    function(object, value) {
        assert(hasRownames(value))
        denylist <- c("interestingGroups", "rowname", "sampleId")
        keep <- setdiff(colnames(value), denylist)
        assert(hasLength(keep))
        value <- value[, keep, drop = FALSE]
        colData(object) <- value
        validObject(object)
        object
    }



#' @rdname sampleData
#' @export
setMethod(
    f = "sampleData",
    signature = signature("SummarizedExperiment"),
    definition = `sampleData,SE`
)



#' @rdname sampleData
#' @export
setReplaceMethod(
    f = "sampleData",
    signature = signature(
        object = "SummarizedExperiment",
        value = "DataFrame"
    ),
    definition = `sampleData<-,SE,DataFrame`
)
