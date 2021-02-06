context("melt")

test_that("Default", {
    for (object in list(
        "matrix" = mat,
        "Matrix" = sparse,
        "DataFrame" = df,
        "SummarizedExperiment" = rse
    )) {
        x <- melt(object)
        expect_s4_class(x, "DataFrame")
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
    mapply(
        trans = eval(formals(`melt,SE`)[["trans"]]),
        expected = list(
            "identity" = c(17, 4, 27),  # nolint
            "log2" = c(4.170, 2.322, 4.807),
            "log10" = c(1.255, 0.699, 1.447)
        ),
        FUN = function(trans, expected) {
            object <- rse
            object <- melt(
                object = object,
                min = 1L,
                minMethod = "perRow",
                trans = trans
            )
            expect_s4_class(object, "DataFrame")
            object <-
                decode(round(head(object[["value"]], n = 3L), digits = 3L))
            expect_identical(object, expected)
        },
        SIMPLIFY = FALSE
    )
})
