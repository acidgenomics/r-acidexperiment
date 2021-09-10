## FIXME Needs to loop across the assays for SE method.
## FIXME Need to make this clearer about direction...

## FIXME Allow user to set MARGIN here?
## FIXME If we do this, how is it different from aggregateRows, aggregateCols?
## FIXME These seem a bit confusing currently.

## FIXME If we change the MARGIN, need to keep track here.
## FIXME Allow the user to calculate across columns with MARGIN argument.



#' Aggregate
#'
#' This function
#'
#' @name aggregate
#' @note Updated 2021-09-10.
#'
#' @inheritParams AcidRoxygen::params
#' @param by `factor`.
#'   Aggregation groupings. The new aggregate names are defined as the `factor`
#'   `levels`, and the original, unaggregated names are defined as the `names`.
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
#' print(counts)
#' aggregate(counts, by = genes, MARGIN = 1L)
#'
#' ## Matrix ====
#' sparse <- as(counts, "sparseMatrix")
#' print(sparse)
#' aggregate(sparse, by = genes, MARGIN = 1L)
#'
#' ## SummarizedExperiment ====
#' ## FIXME
#' ## FIXME Need to define the "by" argument here.
NULL



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



## FIXME Attempt to consolidate the code in aggregateRows and aggregateCols
## here into a single, simpler method.
##
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
