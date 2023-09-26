test_that("RangedSummarizedExperiment", {
    rowRanges(rse) <- as(rowRanges(rse), "EnsemblGenes")
    formats <- eval(methodFormals(
        f = "EnsemblToNcbi",
        signature = "EnsemblGenes",
        package = "AcidGenomes"
    )[["format"]])
    for (format in formats) {
        object <- rse
        object <- EnsemblToNcbi(object = object, format = format)
        expect_s4_class(object, "EnsemblToNcbi")
    }
})
