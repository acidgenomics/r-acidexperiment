test_that("SummarizedExperiment", {
    object <- rse
    x <- encode(object)
    expect_s4_class(rowData(x)[[1L]], "Rle")
    expect_s4_class(colData(x)[[1L]], "Rle")
    y <- decode(object)
    expect_s3_class(rowData(y)[[1L]], "factor")
    expect_s3_class(colData(y)[[1L]], "factor")
})
