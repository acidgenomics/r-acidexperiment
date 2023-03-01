## NOTE These values can change if we update example RSE object.
## FIXME Our example object is now failing with "log-geometric-mean-ratio"...hmmm

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
                "sample01" = 0.912314,
                "sample02" = 0.890313,
                "sample03" = 0.880676,
                "sample04" = 0.905678,
                "sample05" = 0.896723,
                "sample06" = 0.914905,
                "sample07" = 1.12351,
                "sample08" = 1.10232,
                "sample09" = 1.08996,
                "sample10" = 1.11951,
                "sample11" = 1.09414,
                "sample12" = 1.06996
            ),
            "geometric-mean-ratio" = c(
                "sample01" = 0.912314,
                "sample02" = 0.890313,
                "sample03" = 0.880676,
                "sample04" = 0.905678,
                "sample05" = 0.896723,
                "sample06" = 0.914905,
                "sample07" = 1.12351,
                "sample08" = 1.10232,
                "sample09" = 1.08996,
                "sample10" = 1.11951,
                "sample11" = 1.09414,
                "sample12" = 1.06996
            ),
            ## FIXME This is now failing to center with our example dataset.
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
