context("export")

test_that("'dir' argument, no 'name'", {
    out <- export(rse, name = NULL, dir = "XXX", compress = TRUE)
    prefix <- realpath(file.path("XXX", "rse"))
    expect_identical(
        out,
        list(
            assays = list(
                counts = file.path(prefix, "assays", "counts.csv.gz")
            ),
            colData = file.path(prefix, "colData.csv.gz"),
            rowData = file.path(prefix, "rowData.csv.gz")
        )
    )
    unlink("XXX", recursive = TRUE)
})

test_that("Both 'name' and 'dir' declared", {
    out <- export(rse, name = "test", dir = "XXX", compress = FALSE)
    prefix <- realpath(file.path("XXX", "test"))
    expect_identical(
        out,
        list(
            assays = list(
                counts = file.path(prefix, "assays", "counts.csv")
            ),
            colData = file.path(prefix, "colData.csv"),
            rowData = file.path(prefix, "rowData.csv")
        )
    )
    unlink("XXX", recursive = TRUE)
})

test_that("Unnamed primary assay", {
    se <- as(rse, "SummarizedExperiment")
    ## Note that `assayNames()` assignment doesn't work here.
    names(assays(se)) <- NULL
    expect_null(assayNames(se))
    x <- export(se, dir = "XXX")
    expect_identical(names(x[["assays"]]), "assay")
    unlink("XXX", recursive = TRUE)
})
