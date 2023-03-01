## NOTE These values can change when we update AcidTest.

test_that("SE", {
    object <- rse
    cols <- c(
        "log10FeaturesPerCount",
        "mitoRatio",
        "nCount",
        "nFeature",
        "nCoding",
        "nMito"
    )
    expect_false(any(cols %in% colnames(colData(object))))
    object <- calculateMetrics(object)
    expect_true(all(cols %in% colnames(colData(object))))
    expect_s4_class(object, "SummarizedExperiment")
    x <- decode(colData(object))[1L, cols, drop = TRUE]
    x <- lapply(X = x, FUN = round, digits = 3L)
    expect_equal(
        object = x,
        expected = list(
            "log10FeaturesPerCount" = 0.617,
            "mitoRatio" = NA_integer_,
            "nCount" = 20070, # nolint
            "nFeature" = 451, # nolint
            "nCoding" = 20049, # nolint
            "nMito" = NA_integer_
        )
    )
})

test_that("SE : Low pass prefiltering of zero counts", {
    ## All barcodes in example should pass.
    object <- rse
    x <- calculateMetrics(object, prefilter = TRUE)
    expect_identical(ncol(x), ncol(object))
    ## Simulate some poor barcodes.
    counts(object)[, seq_len(2L)] <- 0L
    x <- calculateMetrics(object, prefilter = TRUE)
    expect_identical(ncol(x), ncol(object) - 2L)
})

test_that("SE : prefiltering based on metrics", {
    object <- SummarizedExperiment(
        assays = list(
            "counts" = matrix(
                data = rep(1L, 16L),
                nrow = 4L,
                ncol = 4L,
                dimnames = list(
                    paste0("gene", seq_len(4L)),
                    paste0("sample", seq_len(4L))
                )
            )
        ),
        rowData = DataFrame(
            "geneId" = c(
                "ENSG00000198899",
                "ENSG00000228253",
                "ENSG00000198804",
                "ENSG00000198712"
            ),
            "geneName" = c(
                "MT-ATP6",
                "MT-ATP8",
                "MT-CO1",
                "MT-CO2"
            ),
            "geneBiotype" = c(
                "protein_coding",
                "protein_coding",
                "protein_coding",
                "protein_coding"
            ),
            "broadClass" = c(
                "mito",
                "mito",
                "mito",
                "mito"
            )
        )
    )
    x <- calculateMetrics(object, prefilter = TRUE)
    expect_identical(
        object = dim(x),
        expected = c(4L, 0L)
    )
})

test_that("SE : Missing rowData", {
    object <- rse
    rowData(object) <- NULL
    expect_message(
        object = calculateMetrics(object),
        regexp = "biotype"
    )
    object <- rse
    rowData(object)[["broadClass"]] <- NULL
    expect_message(
        object = calculateMetrics(object),
        regexp = "biotype"
    )
})

test_that("matrix : rowData mismatch", {
    counts <- counts(rse)
    rowData <- rowData(rse)
    rowData <- rowData[2L:nrow(rowData), , drop = FALSE]
    expect_error(
        object = calculateMetrics(
            object = counts,
            rowData = rowData
        ),
        regexp = "gene001"
    )
})

test_that("matrix : no rowData", {
    object <- mat
    expect_message(
        object = calculateMetrics(object),
        regexp = "biotype"
    )
})
