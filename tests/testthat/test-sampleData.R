context("sampleData")

test_that("Return", {
    object <- rse
    ## Check that `sampleName` and `interestingGroups` auto-populate.
    expect_identical(
        object = setdiff(
            x = colnames(sampleData(object)),
            y = colnames(colData(object))
        ),
        expected = c("sampleName", "interestingGroups")
    )
    x <- sampleData(object)[, colnames(colData(object)), drop = FALSE]
    y <- colData(object)
    expect_identical(
        object = as.data.frame(x),
        expected = as.data.frame(y)
    )
    ## Empty `colData` is supported.
    object <- rse
    colData(object) <- DataFrame(row.names = colnames(object))
    interestingGroups(object) <- NULL
    expect_silent(sampleData(object))
})

test_that("Empty return", {
    se <- SummarizedExperiment()
    expect_identical(sampleData(se), DataFrame())
})

test_that("Assignment", {
    object <- rse
    sampleData(object)[["test"]] <- as.factor(seq_len(ncol(object)))
    expect_is(sampleData(object)[["test"]], "factor")
})
