context("integerCounts")

## This is primarily intended for use with DESeq2.
test_that("SE", {
    object <- SummarizedExperiment(
        assays = list(
            "counts" = matrix(
                data = c(
                    0.1, 0.2, 0.3,
                    1.1, 1.2, 1.3,
                    2.1, 2.2, 2.3
                ),
                nrow = 3L,
                ncol = 3L,
                byrow = TRUE
            )
        )
    )
    expected <- matrix(
        data = c(
            0L, 0L, 0L,
            1L, 1L, 1L,
            2L, 2L, 2L
        ),
        nrow = 3L,
        ncol = 3L,
        byrow = TRUE
    )
    expect_identical(
        object = integerCounts(object),
        expected = expected
    )
})
