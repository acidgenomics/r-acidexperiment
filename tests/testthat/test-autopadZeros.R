context("autopadZeros")

test_that("SummarizedExperiment", {
    object <- rse
    object <- autopadZeros(
        object = object,
        rownames = TRUE,
        colnames = TRUE,
        sort = TRUE
    )
    expect_s4_class(object, "RangedSummarizedExperiment")
})
