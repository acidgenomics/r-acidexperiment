context("convertGenesToSymbols")

test_that("SummarizedExperiment", {
    object <- rse
    object <- convertGenesToSymbols(object)
    expect_error(
        object = convertGenesToSymbols(object, strict = TRUE),
        regexp = "Strict mode"
    )
    expect_identical(
        object = rownames(object),
        expected = as.character(mcols(rowRanges(object))[["geneName"]])
    )
})



context("convertSymbolsToGenes")

test_that("SummarizedExperiment", {
    object <- rse
    object <- convertGenesToSymbols(rse)
    object <- convertSymbolsToGenes(object)
    expect_identical(
        object = rownames(object),
        expected = as.character(mcols(rowRanges(object))[["geneId"]])
    )
})
