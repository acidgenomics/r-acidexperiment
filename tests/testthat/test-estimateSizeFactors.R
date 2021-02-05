context("estimateSizeFactors")

test_that("SummarizedExperiment", {
    object <- rse
    expect_null(sizeFactors(object))
    object <- estimateSizeFactors(object)
    expect_type(sizeFactors(object), "double")
    expect_identical(
        unname(sizeFactors(object)),
        colData(object)[["sizeFactor"]]
    )
})
