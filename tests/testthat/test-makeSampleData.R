test_that("data.frame with row names", {
    expect_identical(
        object = makeSampleData(
            object = data.frame(
                "genotype" = rep(c("control", "wildtype"), times = 2L),
                "treatment" = rep(c("vector", "RNAi"), each = 2L),
                row.names = paste0("GSM000000", seq_len(4L))
            )
        ),
        expected = DataFrame(
            "sampleName" = as.factor(paste0("GSM000000", seq_len(4L))),
            "genotype" = as.factor(rep(c("control", "wildtype"), times = 2L)),
            "treatment" = as.factor(rep(c("vector", "RNAi"), each = 2L)),
            row.names = paste0("GSM000000", seq_len(4L))
        )
    )
})

test_that("data.frame with 'rowname' column", {
    expect_identical(
        object = makeSampleData(
            object = data.frame(
                "rowname" = c("sample1", "sample2")
            )
        ),
        expected = DataFrame(
            "sampleName" = as.factor(c("sample1", "sample2")),
            "rowname" = as.factor(c("sample1", "sample2")),
            row.names = c("sample1", "sample2")
        )
    )
})
