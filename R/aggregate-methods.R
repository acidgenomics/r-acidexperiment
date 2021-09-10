## FIXME Need to update SingleCellExperiment method in AcidSingleCell.



#' Aggregate
#'
#' @name aggregate
#' @author Michael Steinbaugh, Rory Kirchner
#' @note Updated 2021-09-10.
#'
#' @section Methods (by class):
#'
#' - `matrix`, `Matrix`:
#'   Aggregate using a grouping `factor`.
#' - `SummarizedExperiment`:
#'   Aggregate data slotted in `assays()` using an automatically generated
#'   grouping `factor`, which is obtained from a user-defined column
#'   (`col` argument) in either the `rowData()` or `colData()` of the object.
#'   Slot an `aggregate` column into `rowData()` for `aggregateRows()`, or into
#'   `colData()` for `aggregateCols()`. This method will define the `groupings`
#'   automatically, and perform the aggregation.
#'
#' @inheritParams AcidRoxygen::params
#' @param by `factor`.
#'   Aggregation groupings. The new aggregate names are defined as the `factor`
#'   `levels`, and the original, unaggregated names are defined as the `names`.
#' @param col `character(1)`.
#'   Name of column in either `rowData()` or `colData()` that defines the
#'   desired aggregation groupings.
#' @param fun `character(1)`.
#'   Name of the aggregation function to apply.
#'   Uses `match.arg()` internally.
#' @param ... Additional arguments.
#'
#' @seealso
#' - `stats::aggregate()`.
#' - `S4Vectors::aggregate()`.
#' - `Matrix.utils::aggregate.Matrix()`.
#' - `muscat::aggregateData()`.
#'
#' @return Modified object.
#'
#' @examples
#' counts <- matrix(
#'     data = c(
#'         0L, 2L, 2L, 2L,
#'         2L, 0L, 2L, 2L,
#'         2L, 2L, 0L, 2L,
#'         2L, 2L, 2L, 0L
#'     ),
#'     nrow = 4L,
#'     ncol = 4L,
#'     byrow = TRUE,
#'     dimnames = list(
#'         paste0("transcript", seq_len(4L)),
#'         paste(
#'             paste0("sample", rep(seq_len(2L), each = 2L)),
#'             paste0("replicate", rep(seq_len(2L), times = 2L)),
#'             sep = "_"
#'         )
#'     )
#' )
#'
#' genes <- factor(paste0("gene", rep(seq_len(2L), each = 2L)))
#' names(genes) <- rownames(counts)
#' print(genes)
#'
#' samples <- factor(paste0("sample", rep(seq_len(2L), each = 2L)))
#' names(samples) <- colnames(counts)
#' print(samples)
#'
#' ## matrix ====
#' object <- counts
#' print(object)
#' aggregate(object, by = genes, MARGIN = 1L)
#' aggregateRows(object, by = genes)
#' aggregate(object, by = samples, MARGIN = 2L)
#' aggregateCols(object, by = samples)
#'
#' ## Matrix ====
#' object <- as(counts, "sparseMatrix")
#' print(object)
#' aggregate(object, by = genes, MARGIN = 1L)
#' aggregateRows(object, by = genes)
#' aggregate(object, by = samples, MARGIN = 2L)
#' aggregateCols(object, by = samples)
#'
#' ## SummarizedExperiment ====
#' object <- SummarizedExperiment(
#'     assays = SimpleList(
#'         "counts" = counts
#'     ),
#'     rowData = DataFrame(
#'         "aggregate" = genes
#'     ),
#'     colData = DataFrame(
#'         "sampleName" = as.factor(names(samples)),
#'         "aggregate" = samples
#'     )
#' )
#' print(object)
#' aggregate(object, MARGIN = 1L)
#' aggregateRows(object)
#' aggregate(object, MARGIN = 2L)
#' aggregateCols(object)
NULL



## aggregate ===================================================================
## Using the `stats::aggregate.data.frame()` S3 method internally here.
## Updated 2021-09-10.
`aggregate,matrix` <-  # nolint
    function(
        x,
        by,
        fun = c("sum", "mean", "median", "geometricMean", "n"),
        MARGIN = 1L  # nolint
    ) {
        assert(
            hasDimnames(x),
            is.factor(by),
            isInt(MARGIN),
            isInRange(MARGIN, lower = 1L, upper = 2L)
        )
        fun <- match.arg(fun)
        if (MARGIN == 2L) {
            x <- t(x)
        }
        assert(identical(rownames(x), names(by)))
        if (fun == "n") {
            x <- x != 0L
            mode(x) <- "integer"
            fun <- "sum"
        }
        x <- aggregate(
            x = as.data.frame(x),
            by = list(rowname = by),
            FUN = get(x = fun, inherits = TRUE)
        )
        rownames(x) <- x[["rowname"]]
        x[["rowname"]] <- NULL
        x <- as.matrix(x)
        if (MARGIN == 2L) {
            x <- t(x)
        }
        x
    }



## Matrix multiplication using sparse model (design matrix).
## Note that this works row-wise, like stats data.frame method.
## Updated 2021-09-10.
`aggregate,Matrix` <-  # nolint
    function(
        x,
        by,
        fun = c("sum", "mean", "n"),
        MARGIN = 1L  # nolint
    ) {
        requireNamespaces("Matrix")
        assert(
            hasDimnames(x),
            is.factor(by),
            isInt(MARGIN),
            isInRange(MARGIN, lower = 1L, upper = 2L)
        )
        fun <- match.arg(fun)
        if (MARGIN == 2L) {
            x <- t(x)
        }
        assert(identical(names(by), rownames(x)))
        if (identical(fun, "n")) {
            x <- x != 0L
        }
        model <- Matrix::fac2sparse(by)
        ## This step calculates the sum.
        result <- model %*% x
        if (identical(fun, "mean")) {
            n <- aggregate(x = x, by = by, fun = "n")
            ## Avoid NaN from diving by zero.
            n[n == 0L] <- 1L
            result <- result / n
        }
        if (MARGIN == 2L) {
            result <- t(result)
        }
        result
    }



## Updated 2021-09-10.
`aggregate,SE` <-  # nolint
    function(
        x,
        col = "aggregate",
        fun = "sum",
        MARGIN = 1L  # nolint
    ) {
        validObject(x)
        assert(
            hasDimnames(x),
            isString(col),
            isString(fun),
            isInt(MARGIN),
            isInRange(MARGIN, lower = 1L, upper = 2L)
        )
        ## Groupings -----------------------------------------------------------
        annoDataFun <- get(
            x = switch(
                EXPR = as.character(MARGIN),
                "1" = "rowData",
                "2" = "colData"
            ),
            envir = asNamespace("SummarizedExperiment"),
            inherits = FALSE
        )
        assert(is.function(annoDataFun))
        annoData <- annoDataFun(x)
        assert(
            is(annoData, "DataFrame"),
            isSubset(col, colnames(annoData))
        )
        by <- decode(annoData[[col]])
        assert(is.factor(by))
        names(by) <- switch(
            EXPR = as.character(MARGIN),
            "1" = rownames(x),
            "2" = colnames(x)
        )
        ## Assays --------------------------------------------------------------
        assays <- lapply(
            X = assays(x),
            FUN = function(x) {
                aggregate(
                    x = x,
                    by = by,
                    fun = fun,
                    MARGIN = MARGIN
                )
            }
        )
        ## Return --------------------------------------------------------------
        args <- list()
        args[["assays"]] <- assays
        switch(
            EXPR = as.character(MARGIN),
            "1" = {
                args[["colData"]] <- colData(x)
            },
            "2" = {
                if (is(x, "RangedSummarizedExperiment")) {
                    args[["rowRanges"]] <- rowRanges(x)  # nocov
                } else {
                    args[["rowData"]] <- rowData(x)
                }
            }
        )
        out <- do.call(what = SummarizedExperiment, args = args)
        metadata(out)[["aggregate"]] <- TRUE
        validObject(out)
        out
    }



## Legacy methods ==============================================================
## Updated 2021-09-10.
`aggregateCols,matrix` <-  # nolint
    function(x, ...) {
        aggregate(x = x, MARGIN = 2L, ...)
    }



## Updated 2021-09-10.
`aggregateCols,Matrix` <-  # nolint
    `aggregateCols,matrix`



## Updated 2021-09-10.
`aggregateCols,SE` <-  # nolint
    `aggregateCols,matrix`



## Updated 2021-09-10.
`aggregateRows,matrix` <-  # nolint
    function(x, ...) {
        aggregate(x = x, MARGIN = 1L, ...)
    }



## Updated 2021-09-10.
`aggregateRows,Matrix` <-  # nolint
    `aggregateRows,matrix`



## Updated 2021-09-10.
`aggregateRows,SE` <-  # nolint
    `aggregateRows,matrix`



## S4 method exports ===========================================================
#' @rdname aggregate
#' @export
setMethod(
    f = "aggregate",
    signature = signature("Matrix"),
    definition = `aggregate,Matrix`
)

#' @describeIn aggregate
#' Arguments pass through to `matrix` or `Matrix` method, depending on the class
#' of matrix defined in requested `assay`.
#' @export
setMethod(
    f = "aggregate",
    signature = signature("SummarizedExperiment"),
    definition = `aggregate,SE`
)

#' @rdname aggregate
#' @export
setMethod(
    f = "aggregate",
    signature = signature("matrix"),
    definition = `aggregate,matrix`
)



#' @rdname aggregate
#' @export
setMethod(
    f = "aggregateCols",
    signature = signature("Matrix"),
    definition = `aggregateCols,Matrix`
)

#' @rdname aggregate
#' @export
setMethod(
    f = "aggregateCols",
    signature = signature("SummarizedExperiment"),
    definition = `aggregateCols,SE`
)

#' @rdname aggregate
#' @export
setMethod(
    f = "aggregateCols",
    signature = signature("matrix"),
    definition = `aggregateCols,matrix`
)



#' @rdname aggregate
#' @export
setMethod(
    f = "aggregateRows",
    signature = signature("Matrix"),
    definition = `aggregateRows,Matrix`
)

#' @rdname aggregate
#' @export
setMethod(
    f = "aggregateRows",
    signature = signature("SummarizedExperiment"),
    definition = `aggregateRows,SE`
)

#' @rdname aggregate
#' @export
setMethod(
    f = "aggregateRows",
    signature = signature("matrix"),
    definition = `aggregateRows,matrix`
)
