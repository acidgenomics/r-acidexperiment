# > skip_if_not_installed("AnnotationHub")
# > skip_if_not_installed("ensembldb")

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

test_that("SummarizedExperiment", {
    object <- rse
    object <- convertGenesToSymbols(rse)
    object <- convertSymbolsToGenes(object)
    expect_identical(
        object = rownames(object),
        expected = as.character(mcols(rowRanges(object))[["geneId"]])
    )
})
