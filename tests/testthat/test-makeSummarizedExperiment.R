context("makeSummarizedExperiment")

genes <- paste0("gene", seq_len(4L))
samples <- paste0("sample", seq_len(4L))

counts <- matrix(
    data = seq_len(16L),
    nrow = 4L,
    ncol = 4L,
    byrow = FALSE,
    dimnames = list(genes, samples)
)

assays <- SimpleList("counts" = counts)

rowRanges <- GRanges(
    seqnames = replicate(n = 4L, expr = "1"),
    ranges = IRanges(
        start = seq(from = 1L, to = 301L, by = 100L),
        end = seq(from = 100L, to = 401L, by = 100L)
    )
)
names(rowRanges) <- genes

rowData <- as(as.data.frame(rowRanges), "DataFrame")

colData <- DataFrame(
    "genotype" = rep(c("wildtype", "knockout"), each = 2L),
    "age" = rep(c(3L, 6L), times = 2L),
    row.names = samples
)

test_that("RangedSummarizedExperiment", {
    object <- makeSummarizedExperiment(
        assays = assays,
        rowRanges = rowRanges,
        colData = colData
    )
    expect_s4_class(object, "RangedSummarizedExperiment")
    expect_identical(dim(object), c(4L, 4L))
    expect_identical(names(object), genes)
    expect_identical(
        object = lapply(metadata(object), class),
        expected = list(
            date = "Date",
            sessionInfo = "session_info",
            wd = "character"
        )
    )
})

## Allowing legacy support of 'rowData' pass-in.
test_that("SummarizedExperiment", {
    object <- makeSummarizedExperiment(
        assays = assays,
        rowData = rowData,
        colData = colData
    )
    expect_identical(simpleClass(object), "SummarizedExperiment")
})

test_that("Minimal input", {
    assays <- list(matrix(nrow = 0L, ncol = 0L))
    object <- makeSummarizedExperiment(assays = assays)
    expect_identical(simpleClass(object), "SummarizedExperiment")
    object <- makeSummarizedExperiment(
        assays = assays,
        rowRanges = NULL,
        rowData = NULL,
        colData = NULL,
        metadata = NULL
    )
    expect_identical(simpleClass(object), "SummarizedExperiment")
    expect_identical(assayNames(object), "counts")
})

test_that("Error on multiple unnamed assays", {
    expect_error(
        object = makeSummarizedExperiment(
            assays = list(matrix(), matrix())
        ),
        regexp = "names"
    )
})

test_that("Inform instead of error on invalid row and/or column names", {
    rownames(counts) <- paste0(rownames(counts), "-XXX")
    colnames(counts) <- paste0(colnames(counts), "-XXX")
    expect_message(
        object = makeSummarizedExperiment(
            assays = SimpleList("counts" = counts)
        ),
        regexp = "make.names"
    )
})

test_that("Duplicate names", {
    countsDupeRows <- counts
    rownames(countsDupeRows) <- paste0("gene", rep(seq_len(2L), each = 2L))
    expect_error(
        object = makeSummarizedExperiment(
            assays = SimpleList("counts" = countsDupeRows),
            rowRanges = rowRanges,
            colData = colData
        ),
        regexp = "hasNoDuplicates"
    )
    countsDupeCols <- counts
    colnames(countsDupeCols) <- paste0("sample", rep(seq_len(2L), each = 2L))
    expect_error(
        object = makeSummarizedExperiment(
            assays = SimpleList("counts" = countsDupeCols),
            rowRanges = rowRanges,
            colData = colData
        ),
        regexp = "hasNoDuplicates"
    )
})

test_that("Column data failure", {
    ## Bad pass-in of objects not supporting `dimnames`.
    expect_error(
        object = makeSummarizedExperiment(
            assays = SimpleList("counts" = "yyy"),
            rowRanges = rowRanges,
            colData = colData
        ),
        regexp = "assay"
    )
    expect_error(
        object = makeSummarizedExperiment(
            assays = assays,
            rowRanges = rowRanges,
            colData = c("xxx" = "yyy")
        ),
        regexp = "isAny.*colData"
    )
    expect_error(
        object = makeSummarizedExperiment(
            assays = assays,
            rowRanges = c(xxx = "yyy"),
            colData = colData
        ),
        regexp = "isAny.*rowRanges"
    )
})

test_that("Row annotation mismatch", {
    badRowRanges <- rowRanges
    names(badRowRanges)[seq_len(2L)] <- LETTERS[seq_len(2L)]
    expect_error(
        object = makeSummarizedExperiment(
            assays = assays,
            rowRanges = badRowRanges
        ),
        regexp = "gene1, gene2"
    )
    badRowData <- rowData
    rownames(badRowData)[c(3L, 4L)] <- LETTERS[seq_len(2L)]
    expect_error(
        object = makeSummarizedExperiment(
            assays = assays,
            rowData = badRowData
        ),
        regexp = "gene3, gene4"
    )
})

test_that("Invalid metadata", {
    expect_error(
        object = makeSummarizedExperiment(
            assays = assays,
            rowRanges = rowRanges,
            colData = colData,
            metadata = Sys.Date()
        ),
        regexp = "isAny.*metadata"
    )
})

## This situation can happen when users input the incorrect genome release.
## Currently is a common case for bcbio RNA-seq output, when a user selects
## an incorrect Ensembl release or uses the latest release.
test_that("Mismatched rowRanges support", {
    rowRanges <- gr
    assays <- SimpleList(
        matrix(
            data = 1L,
            nrow = length(rowRanges),
            ncol = 1L,
            dimnames = list(
                names(rowRanges),
                "sample1"
            )
        )
    )
    object <- makeSummarizedExperiment(
        assays = assays,
        rowRanges = rowRanges
    )
    expect_s4_class(object, "RangedSummarizedExperiment")
    object <- makeSummarizedExperiment(
        assays = assays,
        rowRanges = rowRanges[2L:length(rowRanges)]
    )
    expect_true("unknown" %in% levels(seqnames(object)))
    expect_false("transgene" %in% levels(seqnames(object)))
    object <- makeSummarizedExperiment(
        assays = assays,
        rowRanges = rowRanges[2L:length(rowRanges)],
        transgeneNames = names(rowRanges)[[1L]]
    )
    expect_true("transgene" %in% levels(seqnames(object)))
    expect_false("unknown" %in% levels(seqnames(object)))
})

test_that("GRangesList support", {
    rowRanges <- GRangesList(
        list(
            "gene1" = GRanges(
                seqnames = replicate(n = 2L, expr = "1"),
                ranges = IRanges(
                    start = seq(from = 1L, to = 101L, by = 100L),
                    end = seq(from = 100L, to = 201L, by = 100L)
                ),
                geneName = c("aaa", "bbb")
            ),
            "gene2" = GRanges(
                seqnames = replicate(n = 2L, expr = "1"),
                ranges = IRanges(
                    start = seq(from = 201L, to = 301L, by = 100L),
                    end = seq(from = 300L, to = 401L, by = 100L)
                ),
                geneName = c("ccc", "ddd")
            )
        )
    )
    assays <- SimpleList(
        matrix(
            data = 1L,
            nrow = length(rowRanges),
            ncol = 1L,
            dimnames = list(
                names(rowRanges),
                "sample1"
            )
        )
    )
    object <- makeSummarizedExperiment(
        assays = assays,
        rowRanges = rowRanges
    )
    expect_s4_class(object, "RangedSummarizedExperiment")
    expect_error(
        object = makeSummarizedExperiment(
            assays = assays,
            rowRanges = rowRanges[1L]  # nolint
        ),
        regexp = "mismatched"
    )
})
