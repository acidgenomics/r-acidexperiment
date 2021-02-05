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

test_that("trans", {
    mapply(
        trans = eval(formals(`melt,SE`)[["trans"]]),
        expected = list(
            "identity" = c(58, 14, 49),  # nolint
            "log2" = c(5.883, 3.907, 5.644),
            "log10" = c(1.771, 1.176, 1.699)
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
