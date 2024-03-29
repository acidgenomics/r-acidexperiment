test_that("Default", {
    for (object in list(
        "DFrame" = df,
        "Matrix" = sparse,
        "SummarizedExperiment" = rse,
        "matrix" = mat
    )) {
        x <- melt(object)
        expect_s4_class(x, "DFrame")
    }
})

## Here we're checking the handling of zero count genes.
test_that("Per row filtering", {
    object <- rse
    assay(object)[seq_len(2L), ] <- 0L
    x <- melt(object, min = 1L, minMethod = "perRow")
    ## Note that this step shouldn't drop all zeros, only all-zero genes.
    expect_true(any(x[["value"]] == 0L))
})

## NOTE These values can change when we update AcidTest.
test_that("trans", {
    Map(
        trans = eval(formals(`melt,SE`)[["trans"]]),
        expected = list(
            "identity" = c(58, 14, 49), # nolint
            "log2" = c(5.88, 3.91, 5.64),
            "log10" = c(1.77, 1.18, 1.70)
        ),
        f = function(trans, expected) {
            object <- rse
            object <- melt(
                object = object,
                min = 1L,
                minMethod = "perRow",
                trans = trans
            )
            expect_s4_class(object, "DFrame")
            object <- decode(round(
                head(object[["value"]], n = 3L),
                digits = 2L
            ))
            expect_identical(object, expected)
        }
    )
})
