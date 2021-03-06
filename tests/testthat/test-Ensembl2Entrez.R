context("Ensembl2Entrez")

formats <- eval(formals(`Ensembl2Entrez,RSE`)[["format"]])
test_that("RangedSummarizedExperiment", {
    for (format in formats) {
        object <- rse
        object <- Ensembl2Entrez(object = object, format = format)
        expect_s4_class(object, "Ensembl2Entrez")
    }
})
rm(formats)
