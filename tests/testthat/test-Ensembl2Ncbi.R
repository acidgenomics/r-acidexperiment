test_that("RangedSummarizedExperiment", {
    rowRanges(rse) <- as(rowRanges(rse), "EnsemblGenes")
    formats <- eval(methodFormals(
        f = "Ensembl2Ncbi",
        signature = "EnsemblGenes",
        package = "AcidGenomes"
    )[["format"]])
    for (format in formats) {
        object <- rse
        object <- Ensembl2Ncbi(object = object, format = format)
        expect_s4_class(object, "Ensembl2Ncbi")
    }
})
