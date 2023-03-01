test_that("SummarizedExperiment", {
    ## FIXME This step is now failing, need to rethink.
    object <- selectSamples(rse, condition = "A")
    expect_s4_class(object, class = "SummarizedExperiment")
    expect_identical(
        object = colnames(object),
        expected = paste0("sample0", seq_len(6L))
    )
})
