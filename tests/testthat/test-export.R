testdir <- file.path(tempdir(), "example")

test_that("New 'con' BiocIO approach, instead of deprecated 'dir'", {
    unlink(testdir, recursive = TRUE)
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
    unlink(testdir, recursive = TRUE)
})

test_that("Compressed sparse matrix support", {
    unlink(testdir, recursive = TRUE)
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
    unlink(testdir, recursive = TRUE)
})

test_that("Unnamed primary assay", {
    unlink(testdir, recursive = TRUE)
    object <- as(rse, "SummarizedExperiment")
    ## Note that `assayNames()` assignment doesn't work here.
    names(assays(object)) <- NULL
    expect_null(assayNames(object))
    x <- export(object = object, con = testdir)
    expect_identical(names(x[["assays"]]), "assay")
    unlink(testdir, recursive = TRUE)
})

## NOTE This assigns object name nested into directory substructure.
## This method likely will be removed in a future release.
test_that("Deprecated : 'dir' argument, no 'name'", {
    unlink(testdir, recursive = TRUE)
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
    unlink(testdir, recursive = TRUE)
})

test_that("Deprecated : both 'name' and 'dir' declared", {
    unlink(testdir, recursive = TRUE)
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
    unlink(testdir, recursive = TRUE)
})
