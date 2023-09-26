object <- head(rse, n = 2L)
rownames <- rownames(object)
g2s <- GeneToSymbol(object)
geneIds <- g2s[["geneId"]]
geneNames <- g2s[["geneName"]]



test_that("SummarizedExperiment", {
    expect_identical(
        object = mapGenesToRownames(rse, genes = rownames),
        expected = c(
            "gene001" = "gene001",
            "gene002" = "gene002"
        )
    )
    expect_identical(
        object = mapGenesToRownames(rse, genes = geneIds),
        expected = c(
            "ENSG00000000003.15" = "gene001",
            "ENSG00000000005.6" = "gene002"
        )
    )
    expect_identical(
        object = mapGenesToRownames(rse, genes = geneNames),
        expected = c(
            "TSPAN6" = "gene001",
            "TNMD" = "gene002"
        )
    )
})

test_that("Mapping priority and failure handling", {
    rowData <- DataFrame(
        "geneId" = c(
            "ENSG00000000003.15",
            "ENSG00000000005.6",
            "ENSG00000000419.14"
        ),
        "geneName" = c(
            "TSPAN6",
            "TNMD",
            "DPM1"
        ),
        "geneSynonyms" = CharacterList(
            c("T245", "TM4SF6", "TSPAN-6"),
            c("BRICD4", "CHM1L", "TEM"),
            c("CDGIE", "MPDS")
        ),
        row.names = c(
            "gene1",
            "gene2",
            "gene3"
        )
    )
    object <- SummarizedExperiment(
        assays = list(),
        rowData = rowData
    )
    ## geneId
    expect_identical(
        object =
            mapGenesToRownames(
                object = object,
                genes = c(
                    "ENSG00000000419.14",
                    "ENSG00000000003.15",
                    "ENSG00000000005.6"
                )
            ),
        expected = c(
            "ENSG00000000419.14" = "gene3",
            "ENSG00000000003.15" = "gene1",
            "ENSG00000000005.6" = "gene2"
        )
    )
    ## geneIdNoVersion
    expect_identical(
        object =
            mapGenesToRownames(
                object = object,
                genes = c(
                    "ENSG00000000419",
                    "ENSG00000000003",
                    "ENSG00000000005"
                )
            ),
        expected = c(
            "ENSG00000000419" = "gene3",
            "ENSG00000000003" = "gene1",
            "ENSG00000000005" = "gene2"
        )
    )
    ## geneName
    expect_identical(
        object =
            mapGenesToRownames(
                object = object,
                genes = c(
                    "DPM1",
                    "TSPAN6",
                    "TNMD"
                )
            ),
        expected = c(
            "DPM1" = "gene3",
            "TSPAN6" = "gene1",
            "TNMD" = "gene2"
        )
    )
    ## geneSynonyms
    expect_identical(
        object = mapGenesToRownames(
            object = object,
            genes = c(
                "TSPAN-6",
                "BRICD4",
                "MPDS"
            )
        ),
        expected = c(
            "TSPAN-6" = "gene1",
            "BRICD4" = "gene2",
            "MPDS" = "gene3"
        )
    )
    ## We're currently supported mixed input of gene identifiers.
    expect_identical(
        object = mapGenesToRownames(
            object = object,
            genes = c(
                "T245",
                "ENSG00000000005.6",
                "DPM1"
            )
        ),
        expected = c(
            "T245" = "gene1",
            "ENSG00000000005.6" = "gene2",
            "DPM1" = "gene3"
        )
    )
    ## Check error handling on mismatch.
    expect_error(
        object = mapGenesToRownames(
            object = object,
            genes = c("TSPAN-6", "BRICD4", "XXX"),
            strict = TRUE
        ),
        regexp = "XXX"
    )
    ## Consider returning with NA instead in a future release.
    expect_identical(
        object = mapGenesToRownames(
            object = object,
            genes = c("TSPAN-6", "BRICD4", "XXX"),
            strict = FALSE
        ),
        c(
            "TSPAN-6" = "gene1",
            "BRICD4" = "gene2"
        )
    )
})

test_that("Direct matching against rownames, for a minimal object", {
    object <- rse
    rowData(object) <- NULL
    expect_identical(
        object = mapGenesToRownames(
            object = object,
            genes = head(rownames(object))
        ),
        expected = head(rownames(object))
    )
    expect_error(
        object = mapGenesToRownames(
            object = object,
            genes = c(head(rownames(object)), "XXX"),
            strict = TRUE
        ),
        regexp = "XXX"
    )
    expect_identical(
        object = mapGenesToRownames(
            object = object,
            genes = c(head(rownames(object)), "XXX"),
            strict = FALSE
        ),
        expected = head(rownames(object))
    )
})



test_that("SummarizedExperiment", {
    expect_identical(
        object = mapGenesToIDs(rse, genes = rownames),
        expected = c(
            "gene001" = "ENSG00000000003.15",
            "gene002" = "ENSG00000000005.6"
        )
    )
    expect_identical(
        object = mapGenesToIDs(rse, genes = geneIds),
        expected = c(
            "ENSG00000000003.15" = "ENSG00000000003.15",
            "ENSG00000000005.6" = "ENSG00000000005.6"
        )
    )
    expect_identical(
        object = mapGenesToIDs(rse, genes = geneNames),
        expected = c(
            "TSPAN6" = "ENSG00000000003.15",
            "TNMD" = "ENSG00000000005.6"
        )
    )
})



test_that("SummarizedExperiment", {
    expect_identical(
        object = mapGenesToSymbols(rse, genes = rownames),
        expected = c(
            "gene001" = "TSPAN6",
            "gene002" = "TNMD"
        )
    )
    expect_identical(
        object = mapGenesToSymbols(rse, genes = geneIds),
        expected = c(
            "ENSG00000000003.15" = "TSPAN6",
            "ENSG00000000005.6" = "TNMD"
        )
    )
})
