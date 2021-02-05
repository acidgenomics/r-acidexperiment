context("calculateMetrics")

test_that("SummarizedExperiment", {
    object <- rse
    cols <- c(
        "log10FeaturesPerCount",
        "mitoRatio",
        "nCount",
        "nFeature",
        "nCoding",
        "nMito"
    )
    expect_false(any(cols %in% colnames(colData(object))))
    object <- calculateMetrics(object)
    expect_true(all(cols %in% colnames(colData(object))))
    expect_s4_class(object, "SummarizedExperiment")
    x <- decode(colData(object))[1L, cols, drop = TRUE]
    x <- lapply(X = x, FUN = round, digits = 3L)
    expect_equal(
        object = x,
        expected = list(
            "log10FeaturesPerCount" = 0.619,
            "mitoRatio" = NA_integer_,
            "nCount" = 19671,  # nolint
            "nFeature" = 457,  # nolint
            "nCoding" = 19578,  # nolint
            "nMito" = NA_integer_
        )
    )
})

test_that("Low pass prefiltering", {
    ## All barcodes in example should pass.
    object <- rse
    x <- calculateMetrics(object, prefilter = TRUE)
    expect_identical(ncol(x), ncol(object))
    ## Simulate some poor barcodes.
    counts(object)[, seq_len(2L)] <- 0L
    x <- calculateMetrics(object, prefilter = TRUE)
    expect_identical(ncol(x), ncol(object) - 2L)
})
