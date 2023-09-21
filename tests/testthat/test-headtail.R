test_that("GRanges", {
    expect_match(
        object = headtail(gr)[[2L]],
        regexp = "ENSG00000000003"
    )
})

test_that("SummarizedExperiment", {
    expect_identical(
        headtail(rse),
        headtail(assay(rse))
    )
})
