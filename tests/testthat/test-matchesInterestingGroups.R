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
        cause(ok),
        "Interesting groups are not defined in 'sampleData()'."
    )
})
