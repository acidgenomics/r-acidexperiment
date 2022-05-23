object <- calculateMetrics(rse)

test_that("SummarizedExperiment", {
    x <- metrics(object)
    expect_s4_class(x, "DataFrame")
    expect_identical(
        object = colnames(x),
        expected = c(
            "condition",
            "nCount",
            "nFeature",
            "nCoding",
            "nMito",
            "log10FeaturesPerCount",
            "mitoRatio",
            "sampleName",
            "interestingGroups"
        )
    )
    expect_true(hasRownames(x))
})
