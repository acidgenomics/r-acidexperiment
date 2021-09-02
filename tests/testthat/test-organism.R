context("organism")

test_that("SE", {
    object <- rse
    expect_identical(
        object = organism(object),
        expected = "Homo sapiens"
    )
})

test_that("SE : metadata stash", {
    object <- rse
    org <- "xxx"
    metadata(object)[["organism"]] <- org
    expect_identical(organism(object), org)
})

test_that("SE : no rowData", {
    object <- rse
    rowData(object) <- NULL
    expect_error(
        object = organism(object),
        regexp = "detect"
    )
})
