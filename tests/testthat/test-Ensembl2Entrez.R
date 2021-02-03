context("Ensembl2Entrez")

formats <- eval(formals(`Ensembl2Entrez,SE`)[["format"]])

test_that("RangedSummarizedExperiment", {
    for (format in formats) {
        object <- Ensembl2Entrez(object = rse, format = format)
        expect_s4_class(object, "Ensembl2Entrez")
    }
})
