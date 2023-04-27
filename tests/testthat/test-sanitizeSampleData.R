object <- DataFrame(
    "genotype" = rep(c("wt", "ko"), times = 2L),
    "batch" = rep(seq_len(2L), each = 2L),
    row.names = paste0("sample", seq_len(4L))
)

test_that("`sampleName` column is required", {
    object[["sampleName"]] <- NULL
    expect_error(
        object = sanitizeSampleData(object),
        regexp = "sampleName"
    )
})

test_that("`sampleName` column can't contain duplicates", {
    object[["sampleName"]] <- "XXX"
    expect_error(
        object = sanitizeSampleData(object),
        regexp = "hasNoDuplicates"
    )
})

test_that("Character without duplicates", {
    object[["sampleName"]] <- paste("sample", seq_len(nrow(object)))
    object <- sanitizeSampleData(object)
    expect_s4_class(object, "DFrame")
    expect_identical(
        object = vapply(
            X = object,
            FUN = simpleClass,
            FUN.VALUE = character(1L)
        ),
        expected = c(
            "genotype" = "factor",
            "batch" = "factor",
            "sampleName" = "character"
        )
    )
})
