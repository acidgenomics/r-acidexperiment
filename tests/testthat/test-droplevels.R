context("droplevels")

test_that("SummarizedExperiment", {
    object <- rse
    x <- droplevels(object)
    expect_s4_class(x, "RangedSummarizedExperiment")
    ## Check for factor columns.
    ok <- any(vapply(
        X = decode(rowData(x)),
        FUN = is.factor,
        FUN.VALUE = logical(1L)
    ))
    expect_true(ok)
    ok <- any(vapply(
        X = decode(colData(x)),
        FUN = is.factor,
        FUN.VALUE = logical(1L)
    ))
    expect_true(ok)
})
