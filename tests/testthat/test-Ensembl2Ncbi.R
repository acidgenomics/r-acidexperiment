test_that("RangedSummarizedExperiment", {
    formats <- eval(methodFormals(
        f = "Ensembl2Ncbi",
        signature = "GenomicRanges",
        package = "AcidGenomes"
    )[["format"]])
    for (format in formats) {
        object <- rse
        object <- Ensembl2Ncbi(object = object, format = format)
        expect_s4_class(object, "Ensembl2Ncbi")
    }
})
