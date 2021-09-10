counts <- matrix(
    data = c(
        0L, 1L, 2L, 3L,
        4L, 0L, 5L, 6L,
        7L, 8L, 0L, 9L,
        10L, 11L, 12L, 0L
    ),
    nrow = 4L,
    ncol = 4L,
    byrow = TRUE,
    dimnames = list(
        paste0("transcript", seq_len(4L)),
        paste(
            paste0("sample", rep(seq_len(2L), each = 2L)),
            paste0("replicate", rep(seq_len(2L), times = 2L)),
            sep = "_"
        )
    )
)

sparse <- as(counts, "sparseMatrix")

genes <- factor(paste0("gene", rep(seq_len(2L), each = 2L)))
names(genes) <- rownames(counts)

samples <- factor(paste0("sample", rep(seq_len(2L), each = 2L)))
names(samples) <- colnames(counts)

se <- SummarizedExperiment(
    assays = SimpleList("counts" = counts),
    colData = DataFrame(
        "sampleName" = as.factor(names(samples)),
        "aggregate" = samples
    ),
    rowData = DataFrame("aggregate" = genes)
)



context("aggregate")

test_that("'fun' argument", {
    invisible(mapply(
        object = list(
            "matrix" = counts,
            "Matrix" = sparse
        ),
        class = c(
            "matrix",
            "Matrix"
        ),
        FUN = function(object, class) {
            invisible(mapply(
                fun = c(
                    "sum",
                    "mean",
                    "n"
                ),
                expectedRows = list(
                    "sum" = matrix(
                        data = c(
                            4L, 1L, 7L, 9L,
                            17L, 19L, 12L, 9L
                        ),
                        nrow = 2L,
                        ncol = 4L,
                        byrow = TRUE,
                        dimnames = list(
                            c(
                                "gene1",
                                "gene2"
                            ),
                            c(
                                "sample1_replicate1",
                                "sample1_replicate2",
                                "sample2_replicate1",
                                "sample2_replicate2"
                            )
                        )
                    ),
                    "mean" = "FIXME",
                    "n" = "FIXME"
                ),
                expectedCols = list(
                    "sum" = "FIXME",
                    "mean" = "FIXME",
                    "n" = "FIXME"
                ),
                MoreArgs = list(
                    object = object,
                    genes = genes,
                    samples = samples
                ),
                FUN = function(
                    object,
                    fun,
                    genes,
                    samples,
                    expectedRows,
                    expectedCols
                ) {
                    aggObject <- aggregate(
                        x = object,
                        by = genes,
                        fun = fun,
                        MARGIN = 1L
                    )
                    expect_is(aggObject, class)
                    aggObject <- as.matrix(aggObject)
                    mode(aggObject) <- "integer"
                    expect_identical(
                        object = aggObject,
                        expected = expectedRows
                    )
                    aggObject <- aggregate(
                        x = object,
                        by = samples,
                        fun = fun,
                        MARGIN = 2L
                    )
                    expect_is(aggObject, class)
                    aggObject <- as.matrix(aggObject)
                    mode(aggObject) <- "integer"
                    expect_identical(
                        object = aggObject,
                        expected = expectedCols
                    )
                },
                SIMPLIFY = FALSE,
                USE.NAMES = FALSE
            ))
        },
        SIMPLIFY = FALSE
    ))
})

test_that("'mean' count mode", {
    invisible(mapply(
        object = list(
            "matrix" = counts,
            "Matrix" = sparse
        ),
        class = c(
            "matrix",
            "Matrix"
        ),
        data = list(
            "matrix" = c(
                2L, 0L, 3L, 4L,
                8L, 9L, 6L, 4L
            ),
            "Matrix" = c(
                4L, 1L, 3L, 4L,
                8L, 9L, 12L, 9L
            )
        ),
        FUN = function(object, class, data) {
            object <- aggregate(
                x = object,
                by = genes,
                fun = "mean",
                MARGIN = 1L
            )
            expect_is(object, class)
            object <- as.matrix(object)
            mode(object) <- "integer"
            expect_identical(
                object = object,
                expected = matrix(
                    data = data,
                    nrow = 2L,
                    ncol = 4L,
                    byrow = TRUE,
                    dimnames = list(
                        c(
                            "gene1",
                            "gene2"
                        ),
                        c(
                            "sample1_replicate1",
                            "sample1_replicate2",
                            "sample2_replicate1",
                            "sample2_replicate2"
                        )
                    )
                )
            )
        },
        SIMPLIFY = FALSE
    ))
})

test_that("'n' count mode", {
    for (object in list(
        "matrix" = counts,
        "Matrix" = sparse
    )) {
        object <- aggregate(
            x = object,
            by = genes,
            fun = "n",
            MARGIN = 1L
        )
        object <- as.matrix(object)
        mode(object) <- "integer"
        expect_identical(
            object = object,
            expected = matrix(
                data = c(
                    1L, 1L, 2L, 2L,
                    2L, 2L, 1L, 1L
                ),
                nrow = 2L,
                ncol = 4L,
                byrow = TRUE,
                dimnames = list(
                    c(
                        "gene1",
                        "gene2"
                    ),
                    c(
                        "sample1_replicate1",
                        "sample1_replicate2",
                        "sample2_replicate1",
                        "sample2_replicate2"
                    )
                )
            )
        )
    }
})

test_that("SummarizedExperiment", {
    object <- aggregate(se, col = "aggregate", MARGIN = 1L)
    expect_s4_class(object, "SummarizedExperiment")
    expect_identical(
        object = assayNames(object),
        expected = assayNames(se)
    )
    expect_identical(
        object = dimnames(object),
        expected = list(
            c("gene1", "gene2"),
            colnames(se)
        )
    )
    object <- aggregate(se, col = "aggregate", MARGIN = 2L)
    expect_identical(
        object = dimnames(object),
        expected = list(
            rownames(se),
            c("sample1", "sample2")
        )
    )
})

test_that("Invalid groupings", {
    expect_error(
        object = aggregate(counts, by = "XXX"),
        regexp = "is.factor"
    )
    expect_error(
        object = aggregate(counts, by = factor(c("XXX", "YYY"))),
        regexp = "identical"
    )
})
