test_that("Annotated", {
    object <- SimpleList()
    intgroups <- c("XXX", "YYY")
    interestingGroups(object) <- intgroups
    expect_identical(
        object = interestingGroups(object),
        expected = intgroups
    )
})

test_that("SE", {
    expect_identical(
        object = interestingGroups(rse),
        expected = "condition"
    )
})

test_that("SE : object with no metadata", {
    object <- rse
    metadata(object) <- list()
    expect_null(interestingGroups(object))
})

test_that("SE : assignment method", {
    object <- rse
    intgroup <- interestingGroups(object)[[1L]]
    interestingGroups(object) <- intgroup
    expect_identical(
        object = interestingGroups(object),
        expected = intgroup
    )
    expect_error(
        object = interestingGroups(object) <- "XXX",
        regexp = "XXX"
    )
})
