


test_that("SummarizedExperiment", {
    for (
        type in eval(methodFormals(
            f = "estimateSizeFactors",
            signature = "SummarizedExperiment",
            package = .pkgName
        )[["type"]])
    ) {
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
                "sample07" = 1.120880,
                "sample08" = 1.095180,
                "sample09" = 1.082490,
                "sample10" = 1.125350,
                "sample11" = 1.087320,
                "sample12" = 1.112250
            ),
            "geometric-mean-ratio" = c(
                "sample01" = 0.888456,
                "sample02" = 0.887597,
                "sample03" = 0.883668,
                "sample04" = 0.903451,
                "sample05" = 0.904851,
                "sample06" = 0.908509,
                "sample07" = 1.120880,
                "sample08" = 1.095180,
                "sample09" = 1.082490,
                "sample10" = 1.125350,
                "sample11" = 1.087320,
                "sample12" = 1.112250
            ),
            "log-geometric-mean-ratio" = c(
                "sample01" = 0.988724,
                "sample02" = 0.988627,
                "sample03" = 0.988184,
                "sample04" = 0.990398,
                "sample05" = 0.990552,
                "sample06" = 0.990956,
                "sample07" = 1.011960,
                "sample08" = 1.009640,
                "sample09" = 1.008480,
                "sample10" = 1.012360,
                "sample11" = 1.008920,
                "sample12" = 1.011190
            )
        )
        expect_identical(
            object = signif(sizeFactors(object), digits = 6L),
            expected = expected
        )
    }
})
