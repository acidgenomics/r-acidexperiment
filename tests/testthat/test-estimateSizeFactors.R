## NOTE These values can change when we update AcidTest.

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
            "log-geometric-mean-ratio" = c(
                "sample01" = 0.99133,
                "sample02" = 0.988888,
                "sample03" = 0.987799,
                "sample04" = 0.9906,
                "sample05" = 0.989605,
                "sample06" = 0.991614,
                "sample07" = 1.01217,
                "sample08" = 1.01026,
                "sample09" = 1.00913,
                "sample10" = 1.01181,
                "sample11" = 1.00952,
                "sample12" = 1.00728
            )
        )
        expect_identical(
            object = signif(sizeFactors(object), digits = 6L),
            expected = expected
        )
    }
})
