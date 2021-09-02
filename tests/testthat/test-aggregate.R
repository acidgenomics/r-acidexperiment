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
    assays = SimpleList(counts = counts),
    colData = DataFrame(
        sampleName = as.factor(names(samples)),
        aggregate = samples
    ),
    rowData = DataFrame(aggregate = genes)
)



context("aggregate")

test_that("'sum' count mode", {
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
            object <- aggregate(object, by = genes, fun = "sum")
            expect_is(object, class)
            object <- as.matrix(object)
            mode(object) <- "integer"
            expect_identical(
                object = object,
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
            object <- aggregate(object, by = genes, fun = "mean")
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
        object <- aggregate(object, by = genes, fun = "n")
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



context("aggregateCols")

expected <- matrix(
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
        rownames(counts),
        levels(samples)
    )
)

test_that("matrix", {
    object <- aggregateCols(counts, by = samples)
    expect_identical(object, expected)
})

test_that("matrix : AcidTest example", {
    by <- as.factor(paste0("sample", rep(seq_len(2L), each = 2L)))
    names(by) <- colnames(mat)
    object <- aggregateCols(mat, by = by)
    expect_is(object, "matrix")
    expected <- matrix(
        data = c(
            3L, 11L, 19L, 27L,
            7L, 15L, 23L, 31L
        ),
        nrow = 4L,
        ncol = 2L,
        byrow = FALSE,
        dimnames = list(
            rownames(mat),
            levels(by)
        )
    )
    expect_identical(object, expected)
})

test_that("matrix : Invalid groupings", {
    expect_error(
        object = aggregateCols(counts, by = "XXX"),
        regexp = "is.factor"
    )
    expect_error(
        object = aggregateCols(counts, by = factor(c("XXX", "YYY"))),
        regexp = "identical"
    )
})

test_that("sparseMatrix", {
    object <- aggregateCols(sparse, by = samples)
    expect_is(object, "sparseMatrix")
    object <- as.matrix(object)
    mode(object) <- "integer"
    expect_identical(object, expected)
})

test_that("SummarizedExperiment", {
    object <- aggregateCols(se)
    expect_s4_class(object, "SummarizedExperiment")
    expect_identical(assayNames(object), "counts")
    expect_identical(counts(object), expected)
})



context("aggregateRows")

expected <- matrix(
    data = c(
        4L, 1L, 7L, 9L,
        17L, 19L, 12L, 9L
    ),
    nrow = 2L,
    ncol = 4L,
    byrow = TRUE,
    dimnames = list(
        levels(genes),
        colnames(counts)
    )
)

test_that("matrix", {
    object <- aggregateRows(counts, by = genes)
    expect_identical(object, expected)
})

test_that("matrix : AcidTest example", {
    by <- as.factor(paste0("gene", rep(seq_len(2L), each = 2L)))
    names(by) <- rownames(mat)
    object <- aggregateRows(mat, by = by)
    expect_is(object, "matrix")
    expected <- matrix(
        data = c(
             6L,  8L, 10L, 12L,
            22L, 24L, 26L, 28L
        ),
        nrow = 2L,
        ncol = 4L,
        byrow = TRUE,
        dimnames = list(
            levels(by),
            colnames(mat)
        )
    )
    expect_identical(object, expected)
})

test_that("sparseMatrix", {
    object <- aggregateRows(sparse, by = genes)
    expect_is(object, "sparseMatrix")
    object <- as.matrix(object)
    mode(object) <- "integer"
    expect_identical(object, expected)
})

## Now requiring "counts" assay to be defined, as of v0.11.4.
test_that("SummarizedExperiment", {
    object <- aggregateRows(se)
    expect_s4_class(object, "SummarizedExperiment")
    expect_identical(assayNames(object), "counts")
    expect_identical(counts(object), expected)
})

test_that("Invalid groupings", {
    expect_error(
        object = aggregateRows(counts, by = "XXX"),
        regexp = "is.factor"
    )
    expect_error(
        object = aggregateRows(counts, by = factor(c("XXX", "YYY"))),
        regexp = "identical"
    )
})
