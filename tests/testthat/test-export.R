test_that("New 'con' BiocIO approach", {
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
            "rowData" = file.path(prefix, "rowData.csv.gz"),
            "colData" = file.path(prefix, "colData.csv.gz")
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
                    "rownames" = file.path(
                        prefix,
                        "assays",
                        "counts.mtx.gz.rownames"
                    ),
                    "colnames" = file.path(
                        prefix,
                        "assays",
                        "counts.mtx.gz.colnames"
                    )
                )
            ),
            "rowData" = file.path(prefix, "rowData.csv.gz"),
            "colData" = file.path(prefix, "colData.csv.gz")
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

test_that("Empty SummarizedExperiment", {
    testdir <- tempdir2()
    object <- SummarizedExperiment()
    out <- export(object = object, con = testdir)
    expect_identical(out, list())
    unlink2(testdir)
})
