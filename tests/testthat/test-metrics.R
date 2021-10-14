context("metrics")

object <- calculateMetrics(rse)

test_that("SummarizedExperiment : tibble", {
    x <- metrics(object, return = "tbl_df")
    expect_s3_class(x, "tbl_df")
    expect_identical(
        object = colnames(x),
        expected = c(
            "sampleId",
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
})

test_that("SummarizedExperiment : DataFrame", {
    x <- metrics(object, return = "DataFrame")
    expect_s4_class(x, "DataFrame")
    expect_true(hasRownames(x))
})
