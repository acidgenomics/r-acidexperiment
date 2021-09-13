context("convertGenesToSymbols")

skip_if_not(hasInternet())

## NOTE Consider using a presaved object, to speed up tests.
gene2symbol <- AcidGenomes::makeGene2SymbolFromEnsembl(
    organism = "Homo sapiens",
    genomeBuild = "GRCh38",
    release = 87L,
    ignoreVersion = TRUE,
    format = "makeUnique"
)

test_that("character", {
    ## Ensure that function supports remapping of column names.
    g2s <- gene2symbol
    colnames(g2s) <- c("x1", "x2")
    expect_identical(
        object = convertGenesToSymbols(
            object = c("ENSG00000000003", "ENSG00000000005"),
            gene2symbol = g2s
        ),
        expected = c(
            "ENSG00000000003" = "TSPAN6",
            "ENSG00000000005" = "TNMD"
        )
    )
})

## Specify organism (to handle FASTA spike-ins (e.g. EGFP).
test_that("FASTA spike-in support", {
    expect_identical(
        object = convertGenesToSymbols(
            object = c(
                "ENSG00000000003",
                "ENSG00000000005",
                "EGFP",
                "mCherry"
            ),
            gene2symbol = gene2symbol,
            strict = FALSE
        ),
        expected = c(
            "ENSG00000000003" = "TSPAN6",
            "ENSG00000000005" = "TNMD",
            "EGFP" = "EGFP",
            "mCherry" = "mCherry"
        )
    )
    expect_error(
        object = convertGenesToSymbols(
            object = c(
                "ENSG00000000003",
                "ENSG00000000005",
                "EGFP",
                "mCherry"
            ),
            gene2symbol = gene2symbol,
            strict = TRUE
        ),
        regexp = "Failed to match all genes to symbols."
    )
})

test_that("matrix", {
    object <- matrix(
        data = seq_len(4L),
        byrow = TRUE,
        nrow = 2L,
        ncol = 2L,
        dimnames = list(
            c("ENSG00000000003", "ENSG00000000005"),
            c("sample1", "sample2")
        )
    )
    object <- convertGenesToSymbols(
        object = object,
        gene2symbol = gene2symbol,
        strict = TRUE
    )
    expect_identical(
        object = rownames(object),
        expected = c("TSPAN6", "TNMD")
    )
})

test_that("GRanges", {
    object <- gr
    object <- convertGenesToSymbols(object)
    expect_identical(
        object = names(object),
        expected = as.character(Gene2Symbol(gr)[["geneName"]])
    )

})

test_that("SummarizedExperiment", {
    object <- convertGenesToSymbols(rse)
    expect_identical(
        object = rownames(object),
        expected = as.character(mcols(rowRanges(object))[["geneName"]])
    )
})



context("convertSymbolsToGenes")

test_that("SummarizedExperiment", {
    object <- convertGenesToSymbols(rse)
    object <- convertSymbolsToGenes(object)
    expect_identical(
        object = rownames(object),
        expected = as.character(mcols(rowRanges(object))[["geneId"]])
    )
})
