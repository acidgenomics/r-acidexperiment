test_that("GeneToSymbol", {
    formats <- eval(methodFormals(
        f = "GeneToSymbol",
        signature = "DFrame",
        package = "AcidGenomes"
    )[["format"]])
    for (format in formats) {
        object <- rse
        object <- GeneToSymbol(object, format = format)
        expect_s4_class(object, "GeneToSymbol")
        expect_identical(colnames(object), c("geneId", "geneName"))
    }
})

test_that("summary", {
    object <- rse
    x <- GeneToSymbol(object)
    output <- capture.output(summary(x))
    expect_identical(
        head(output, n = 1L),
        paste("genes:", length(object))
    )
})
