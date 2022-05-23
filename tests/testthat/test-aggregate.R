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



test_that("'sum' count mode", {
    invisible(Map(
        object = list(
            "matrix" = counts,
            "Matrix" = sparse
        ),
        class = c(
            "matrix",
            "Matrix"
        ),
        f = function(object, class) {
            ## Aggregate down the rows.
            aggObject <- aggregate(
                x = object,
                by = genes,
                fun = "sum",
                MARGIN = 1L
            )
            aggObject <- as.matrix(aggObject)
            mode(aggObject) <- "integer"
            expect_identical(
                object = aggObject,
                expected = matrix(
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
                )
            )
            ## Aggregate across the columns.
            aggObject <- aggregate(
                x = object,
                by = samples,
                fun = "sum",
                MARGIN = 2L
            )
            aggObject <- as.matrix(aggObject)
            mode(aggObject) <- "integer"
            expect_identical(
                object = aggObject,
                expected = matrix(
                    data = c(
                        1L, 5L,
                        4L, 11L,
                        15L, 9L,
                        21L, 12L
                    ),
                    nrow = 4L,
                    ncol = 2L,
                    byrow = TRUE,
                    dimnames = list(
                        c(
                            "transcript1",
                            "transcript2",
                            "transcript3",
                            "transcript4"
                        ),
                        c(
                            "sample1",
                            "sample2"
                        )
                    )
                )
            )
        }
    ))
})

test_that("'mean' count mode", {
    invisible(Map(
        object = list(
            "matrix" = counts,
            "Matrix" = sparse
        ),
        class = c(
            "matrix",
            "Matrix"
        ),
        rowExpectedData = list(
            "matrix" = c(
                2.0, 0.5, 3.5, 4.5,
                8.5, 9.5, 6.0, 4.5
            ),
            "Matrix" = c(
                4.0, 1.0, 3.5, 4.5,
                8.5, 9.5, 12.0, 9.0
            )
        ),
        colExpectedData = list(
            "matrix" = c(
                0.5, 2.5,
                2.0, 5.5,
                7.5, 4.5,
                10.5, 6.0
            ),
            "Matrix" = c(
                1.0, 2.5,
                4.0, 5.5,
                7.5, 9.0,
                10.5, 12.0
            )
        ),
        f = function(object,
                     class,
                     rowExpectedData,
                     colExpectedData) {
            ## Aggregate down the rows.
            aggObject <- aggregate(
                x = object,
                by = genes,
                fun = "mean",
                MARGIN = 1L
            )
            aggObject <- as.matrix(aggObject)
            expect_identical(
                object = aggObject,
                expected = matrix(
                    data = rowExpectedData,
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
            ## Aggregate across the columns.
            aggObject <- aggregate(
                x = object,
                by = samples,
                fun = "mean",
                MARGIN = 2L
            )
            aggObject <- as.matrix(aggObject)
            expect_identical(
                object = aggObject,
                expected = matrix(
                    data = colExpectedData,
                    nrow = 4L,
                    ncol = 2L,
                    byrow = TRUE,
                    dimnames = list(
                        c(
                            "transcript1",
                            "transcript2",
                            "transcript3",
                            "transcript4"
                        ),
                        c(
                            "sample1",
                            "sample2"
                        )
                    )
                )
            )
        }
    ))
})

test_that("'n' count mode", {
    for (object in list(
        "matrix" = counts,
        "Matrix" = sparse
    )) {
        ## Aggregate down the rows.
        aggObject <- aggregate(
            x = object,
            by = genes,
            fun = "n",
            MARGIN = 1L
        )
        aggObject <- as.matrix(aggObject)
        mode(aggObject) <- "integer"
        expect_identical(
            object = aggObject,
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
        ## Aggregate across the columns.
        aggObject <- aggregate(
            x = object,
            by = samples,
            fun = "n",
            MARGIN = 2L
        )
        aggObject <- as.matrix(aggObject)
        mode(aggObject) <- "integer"
        expect_identical(
            object = aggObject,
            expected = matrix(
                data = c(
                    1L, 2L,
                    1L, 2L,
                    2L, 1L,
                    2L, 1L
                ),
                nrow = 4L,
                ncol = 2L,
                byrow = TRUE,
                dimnames = list(
                    c(
                        "transcript1",
                        "transcript2",
                        "transcript3",
                        "transcript4"
                    ),
                    c(
                        "sample1",
                        "sample2"
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
