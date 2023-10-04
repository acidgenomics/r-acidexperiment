## NOTE These values can change when we update AcidTest.
test_that("SummarizedExperiment", {
    types <- eval(methodFormals(
        f = "estimateSizeFactors",
        signature = "SummarizedExperiment",
        package = "AcidExperiment"
    )[["type"]])
    for (type in types) {
        object <- rse
        expect_null(sizeFactors(object))
        object <- estimateSizeFactors(object, type = type)
        expect_identical(
            unname(sizeFactors(object)),
            colData(object)[["sizeFactor"]]
        )
        expected <- switch(
            EXPR = type,
            "mean-ratio" = c(
                "sample01" = 0.888456,
                "sample02" = 0.887597,
                "sample03" = 0.883668,
                "sample04" = 0.903451,
                "sample05" = 0.904851,
                "sample06" = 0.908509,
                "sample07" = 1.12088,
                "sample08" = 1.09518,
                "sample09" = 1.08249,
                "sample10" = 1.12535,
                "sample11" = 1.08732,
                "sample12" = 1.11225
            ),
            "geometric-mean-ratio" = c(
                "sample01" = 0.888456,
                "sample02" = 0.887597,
                "sample03" = 0.883668,
                "sample04" = 0.903451,
                "sample05" = 0.904851,
                "sample06" = 0.908509,
                "sample07" = 1.12088,
                "sample08" = 1.09518,
                "sample09" = 1.08249,
                "sample10" = 1.12535,
                "sample11" = 1.08732,
                "sample12" = 1.11225
            ),
            "log-geometric-mean-ratio" = c(
                "sample01" = 0.988724,
                "sample02" = 0.988627,
                "sample03" = 0.988184,
                "sample04" = 0.990398,
                "sample05" = 0.990552,
                "sample06" = 0.990956,
                "sample07" = 1.01196,
                "sample08" = 1.00964,
                "sample09" = 1.00848,
                "sample10" = 1.01236,
                "sample11" = 1.00892,
                "sample12" = 1.01119
            )
        )
        expect_identical(
            object = signif(sizeFactors(object), digits = 6L),
            expected = expected
        )
    }
})
