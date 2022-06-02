test_that("New 'con' BiocIO approach, instead of deprecated 'dir'", {
    testdir <- tempdir2()
    object <- rse
    out <- export(
        object = object,
        con = testdir,
        compress = TRUE
    )
    prefix <- realpath(testdir)
    expect_identical(
        object = out,
        expected = list(
            "assays" = list(
                "counts" = file.path(prefix, "assays", "counts.csv.gz")
            ),
            "colData" = file.path(prefix, "colData.csv.gz"),
            "rowData" = file.path(prefix, "rowData.csv.gz")
        )
    )
    unlink2(testdir)
})

test_that("Compressed sparse matrix support", {
    testdir <- tempdir2()
    object <- rse
    counts(object) <- as(counts(object), "sparseMatrix")
    out <- export(
        object = object,
        con = testdir,
        compress = TRUE
    )
    prefix <- realpath(testdir)
    expect_identical(
        object = out,
        expected = list(
            "assays" = list(
                "counts" = c(
                    "matrix" = file.path(
                        prefix,
                        "assays",
                        "counts.mtx.gz"
                    ),
                    "barcodes" = file.path(
                        prefix,
                        "assays",
                        "counts.mtx.gz.colnames"
                    ),
                    "genes" = file.path(
                        prefix,
                        "assays",
                        "counts.mtx.gz.rownames"
                    )
                )
            ),
            "colData" = file.path(prefix, "colData.csv.gz"),
            "rowData" = file.path(prefix, "rowData.csv.gz")
        )
    )
    unlink2(testdir)
})

test_that("Unnamed primary assay", {
    testdir <- tempdir2()
    object <- as(rse, "SummarizedExperiment")
    ## Note that `assayNames()` assignment doesn't work here.
    names(assays(object)) <- NULL
    expect_null(assayNames(object))
    x <- export(object = object, con = testdir)
    expect_named(x[["assays"]], "assay")
    unlink2(testdir)
})

## NOTE This assigns object name nested into directory substructure.
## This method likely will be removed in a future release.
test_that("Deprecated : 'dir' argument, no 'name'", {
    testdir <- tempdir2()
    object <- rse
    out <- export(
        object = object,
        dir = testdir,
        compress = TRUE
    )
    prefix <- realpath(file.path(testdir, "object"))
    expect_identical(
        object = out,
        expected = list(
            "assays" = list(
                "counts" = file.path(prefix, "assays", "counts.csv.gz")
            ),
            "colData" = file.path(prefix, "colData.csv.gz"),
            "rowData" = file.path(prefix, "rowData.csv.gz")
        )
    )
    unlink2(testdir)
})

test_that("Deprecated : both 'name' and 'dir' declared", {
    testdir <- tempdir2()
    object <- rse
    out <- export(
        object = object,
        name = "test",
        dir = testdir,
        compress = FALSE
    )
    prefix <- realpath(file.path(testdir, "test"))
    expect_identical(
        object = out,
        expected = list(
            "assays" = list(
                "counts" = file.path(prefix, "assays", "counts.csv")
            ),
            "colData" = file.path(prefix, "colData.csv"),
            "rowData" = file.path(prefix, "rowData.csv")
        )
    )
    unlink2(testdir)
})

test_that("Empty SummarizedExperiment", {
    testdir <- tempdir2()
    object <- SummarizedExperiment()
    out <- export(object = object, con = testdir)
    expect_identical(out, list())
    unlink2(testdir)
})
