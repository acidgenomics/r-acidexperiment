context("matchesInterestingGroups")

test_that("Column defined in 'sampleData()'", {
    ok <- matchesInterestingGroups(x = rse, interestingGroups = "condition")
    expect_true(ok)
})

test_that("Allow 'NULL' to pass", {
    ok <- matchesInterestingGroups(x = rse, interestingGroups = NULL)
    expect_true(ok)
})

test_that("Match failure", {
    ok <- matchesInterestingGroups(rse, c("XXX", "YYY"))
    expect_false(ok)
    expect_s4_class(ok, "goalie")
    expect_identical(
        object = cause(ok),
        expected = "Interesting groups are not defined in 'sampleData()'."
    )
})

test_that("Invalid input", {
    ok <- matchesInterestingGroups(x = FALSE, interestingGroups = FALSE)
    expect_false(ok)
    expect_s4_class(ok, "goalie")
    expect_identical(
        object = cause(ok),
        expected = "'FALSE' is not S4 class."
    )
    ok <- matchesInterestingGroups(x = rse, interestingGroups = FALSE)
    expect_false(ok)
    expect_s4_class(ok, "goalie")
    expect_identical(
        object = cause(ok),
        expected = "'interestingGroups' is not character."
    )
    ok <- matchesInterestingGroups(
        x = DataFrame(),
        interestingGroups = "a"
    )
    expect_false(ok)
    expect_s4_class(ok, "goalie")
    expect_identical(
        object = cause(ok),
        expected = "'sampleData()' return error."
    )
    object <- rse
    colData(object)[["aaa"]] <- NULL
    ok <- matchesInterestingGroups(
        x = object,
        interestingGroups = "aaa"
    )
    expect_false(ok)
    expect_s4_class(ok, "goalie")
    expect_identical(
        object = cause(ok),
        expected = "Interesting groups are not defined in 'sampleData()'."
    )
})
