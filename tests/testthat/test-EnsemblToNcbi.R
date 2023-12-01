test_that("RangedSummarizedExperiment", {
    rowRanges(rse) <- as(rowRanges(rse), "EnsemblGenes")
    object <- rse
    object <- EnsemblToNcbi(object)
    expect_s4_class(object, "EnsemblToNcbi")
})
