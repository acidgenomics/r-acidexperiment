context("organism")

test_that("SummarizedExperiment", {
    object <- rse
    expect_identical(
        object = organism(object),
        expected = "Homo sapiens"
    )
})

test_that("Metadata stash", {
    object <- rse
    org <- "xxx"
    metadata(object)[["organism"]] <- org
    expect_identical(organism(object), org)
})
