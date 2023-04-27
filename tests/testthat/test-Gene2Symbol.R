test_that("Gene2Symbol", {
    formats <- eval(methodFormals(
        f = "Gene2Symbol",
        signature = "DFrame",
        package = "AcidGenomes"
    )[["format"]])
    for (format in formats) {
        object <- rse
        object <- Gene2Symbol(object, format = format)
        expect_s4_class(object, "Gene2Symbol")
        expect_identical(colnames(object), c("geneId", "geneName"))
    }
})

test_that("summary", {
    object <- rse
    x <- Gene2Symbol(object)
    output <- capture.output(summary(x))
    expect_identical(
        head(output, n = 1L),
        paste("genes:", length(object))
    )
})
