object <- head(rse, n = 2L)
rownames <- rownames(object)
g2s <- Gene2Symbol(object)
geneIds <- g2s[["geneId"]]
geneNames <- g2s[["geneName"]]



context("mapGenesToRownames")

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



context("mapGenesToIDs")

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



context("mapGenesToSymbols")

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
