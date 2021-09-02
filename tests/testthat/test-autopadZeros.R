context("autopadZeros")

test_that("SummarizedExperiment", {
    object <- rse
    colnames(object) <- gsub(
        pattern = "^sample[0]+",
        replacement = "sample",
        x = colnames(object)
    )
    colData(object)[["sampleName"]] <- as.factor(colnames(object))
    rownames(object) <- gsub(
        pattern = "^gene[0]+",
        replacement = "gene",
        x = rownames(object)
    )
    object <- autopadZeros(
        object = object,
        rownames = TRUE,
        colnames = TRUE,
        sampleNames = TRUE,
        sort = TRUE
    )
    expect_s4_class(object, "RangedSummarizedExperiment")
    expect_identical(
        object = rownames(object)[[1L]],
        expected = "gene001"
    )
    expect_identical(
        object = colnames(object)[[1L]],
        expected = "sample01"
    )
    expect_identical(
        object = as.character(colData(object)[["sampleName"]])[[1L]],
        expected = "sample01"
    )
})

test_that("SummarizedExperiment with 'sampleName' colData", {
    object <- rse
    colnames(object) <- gsub(
        pattern = "^sample0",
        replacement = "sample",
        x = colnames(object)
    )
    object <- autopadZeros(
        object = object,
        rownames = TRUE,
        colnames = TRUE,
        sort = TRUE
    )
})
