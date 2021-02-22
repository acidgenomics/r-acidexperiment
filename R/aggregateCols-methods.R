#' @name aggregateCols
#' @inherit AcidGenerics::aggregateCols
#' @author Michael Steinbaugh, Rory Kirchner
#' @note Updated 2021-02-22.
#'
#' @inherit aggregateRows
#' @param ... Additional arguments.
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
#' samples <- factor(paste0("sample", rep(seq_len(2L), each = 2L)))
#' names(samples) <- colnames(counts)
#' print(samples)
#'
#' ## matrix ====
#' print(counts)
#' aggregateCols(counts, by = samples)
#'
#' ## Matrix ====
#' sparse <- as(counts, "sparseMatrix")
#' print(sparse)
#' aggregateCols(sparse, by = samples)
#'
#' ## SummarizedExperiment ====
#' se <- SummarizedExperiment(
#'     assays = SimpleList(counts = counts),
#'     colData = DataFrame(
#'         sampleName = as.factor(names(samples)),
#'         aggregate = samples
#'     )
#' )
#' print(se)
#' aggregateCols(se)
NULL



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



#' @rdname aggregateCols
#' @export
setMethod(
    f = "aggregateCols",
    signature = signature("matrix"),
    definition = `aggregateCols,matrix`
)



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



#' @rdname aggregateCols
#' @export
setMethod(
    f = "aggregateCols",
    signature = signature("Matrix"),
    definition = `aggregateCols,Matrix`
)



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
        if (!all(isSubset(col, colnames(colData(x))))) {
            stop(sprintf(
                "'%s' column not defined in 'colData()'.", deparse(col)
            ))
        }
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
            args[["rowRanges"]] <- rowRanges(x)
        } else {
            args[["rowData"]] <- rowData(x)
        }
        se <- do.call(what = SummarizedExperiment, args = args)
        metadata(se)[["aggregate"]] <- TRUE
        validObject(se)
        se
    }



#' @rdname aggregateCols
#' @export
setMethod(
    f = "aggregateCols",
    signature = signature("SummarizedExperiment"),
    definition = `aggregateCols,SE`
)
