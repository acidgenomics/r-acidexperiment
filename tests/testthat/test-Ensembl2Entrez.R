context("Ensembl2Entrez")

test_that("RangedSummarizedExperiment", {
    formats <- eval(methodFormals(
        f = "Ensembl2Entrez",
        signature = "GenomicRanges",
        package = "AcidGenomes"
    )[["format"]])
    for (format in formats) {
        object <- rse
        object <- Ensembl2Entrez(object = object, format = format)
        expect_s4_class(object, "Ensembl2Entrez")
    }
})
