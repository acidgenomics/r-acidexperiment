## FIXME Needs to loop across the assays for SE method.
## FIXME Need to make this clearer about direction...

## FIXME Allow user to set MARGIN here?
## FIXME If we do this, how is it different from aggregateRows, aggregateCols?
## FIXME These seem a bit confusing currently.

## FIXME If we change the MARGIN, need to keep track here.
## FIXME Allow the user to calculate across columns with MARGIN argument.
##
## FIXME SE methods need to work on all assays.

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
#' data(RangedSummarizedExperiment, package = "AcidTest")
#'
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
#' ## FIXME Need to add a primary `aggregate()` example here.
#'
#' se <- SummarizedExperiment(
#'     assays = SimpleList("counts" = counts),
#'     rowData = DataFrame("aggregate" = genes)
#' )
#' print(se)
#' aggregateRows(se)
#'
#' se <- SummarizedExperiment(
#'     assays = SimpleList("counts" = counts),
#'     colData = DataFrame(
#'         "sampleName" = as.factor(names(samples)),
#'         "aggregate" = samples
#'     )
#' )
#' print(se)
#' aggregateCols(se)
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
            isInt(MARGIN)
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
            isInt(MARGIN)
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



## FIXME Need to rework this.
## Updated 2021-09-10.
`aggregate,SE` <-  # nolint
    function(x, ...) {
        validObject(x)
        assays <- lapply(X = assays(x), FUN = aggregate, ...)
        aggregate(
            x = assay(x, i = assay),
            ...
        )
    }



## aggregateRows ===============================================================
## Updated 2021-09-10.
`aggregateRows,matrix` <-  # nolint
    function(x, ...) {
        aggregate(x = x, MARGIN = 1L, ...)
    }



## Updated 2021-09-10.
`aggregateRows,Matrix` <-  # nolint
    `aggregateRows,matrix`



## FIXME Rework this, considering how we can fold into main `aggregate` method.
## FIXME Needs to work on all assays, not just the primary counts.
## Updated 2020-05-22.
`aggregateRows,SE` <-  # nolint
    function(
        x,
        col = "aggregate",
        fun = "sum"
    ) {
        validObject(x)
        assert(
            hasDimnames(x),
            isString(col),
            isString(fun)
        )
        ## Groupings -----------------------------------------------------------
        assert(isSubset(col, colnames(rowData(x))))
        by <- rowData(x)[[col]]
        assert(is.factor(by))
        names(by) <- rownames(x)
        ## Counts --------------------------------------------------------------
        counts <- aggregateRows(x = counts(x), by = by, fun = fun)
        ## Return --------------------------------------------------------------
        args <- list(
            assays = SimpleList(counts = counts),
            colData = colData(x)
        )
        if (is(x, "RangedSummarizedExperiment")) {
            args[["rowRanges"]] <- emptyRanges(names = rownames(counts)) # nocov
        } else {
            args[["rowData"]] <- DataFrame(row.names = rownames(counts))
        }
        se <- do.call(what = SummarizedExperiment, args = args)
        metadata(se)[["aggregate"]] <- TRUE
        validObject(se)
        se
    }




## aggregateCols ===============================================================
## FIXME Rework this.
## Updated 2020-01-30.
`aggregateCols,matrix` <-  # nolint
    function(
        x,
        by,
        fun = c("sum", "mean", "median", "geometricMean")
    ) {
        fun <- match.arg(fun)
        x <- t(x)
        x <- aggregateRows(x = x, by = by, fun = fun)
        x <- t(x)
        x
    }



## FIXME Rework this.
## Updated 2021-02-22.
`aggregateCols,Matrix` <-  # nolint
    function(
        x,
        by,
        fun = c("sum", "mean")
    ) {
        fun <- match.arg(fun)
        x <- t(x)
        x <- aggregateRows(x = x, by = by, fun = fun)
        x <- t(x)
        x
    }



## Updated 2021-02-08.
`aggregateCols,SE` <-  # nolint
    function(
        x,
        col = "aggregate",
        fun = "sum"
    ) {
        validObject(x)
        assert(
            hasDimnames(x),
            isString(col),
            isString(fun)
        )
        ## Groupings -----------------------------------------------------------
        assert(
            all(isSubset(col, colnames(colData(x)))),
            msg = sprintf(
                "'%s' column not defined in '%s'.",
                col, "colData()"
            )
        )
        by <- colData(x)[[col]]
        assert(
            is.factor(by),
            identical(length(by), ncol(x))
        )
        names(by) <- colnames(x)
        ## Counts --------------------------------------------------------------
        counts <- aggregateCols(x = counts(x), by = by, fun = fun)
        assert(identical(nrow(counts), nrow(x)))
        ## Return --------------------------------------------------------------
        args <- list(
            "assays" = SimpleList(counts = counts),
            "colData" = DataFrame(row.names = colnames(counts))
        )
        if (is(x, "RangedSummarizedExperiment")) {
            args[["rowRanges"]] <- rowRanges(x)  # nocov
        } else {
            args[["rowData"]] <- rowData(x)
        }
        se <- do.call(what = SummarizedExperiment, args = args)
        metadata(se)[["aggregate"]] <- TRUE
        validObject(se)
        se
    }



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
