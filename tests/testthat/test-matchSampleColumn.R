context("matchSampleColumn")

test_that("matchSampleColumn", {
    expect_identical(
        object = matchSampleColumn(
            object = DataFrame(
                "sampleId" = "XXX",
                "sampleID" = "XXX",
                "sampleid" = "XXX",
                "sample" = "XXX"
            )
        ),
        expected = "sampleId"
    )
    expect_null(
        object = matchSampleColumn(
            object = DataFrame(
                "aaa" = "AAA",
                "bbb" = "BBB"
            )
        )
    )
})
