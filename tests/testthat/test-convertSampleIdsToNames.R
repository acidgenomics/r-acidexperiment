test_that("SE", {
    object <- rse
    newNames <- letters[seq_len(ncol(object))]
    colData(object)[["sampleName"]] <- as.factor(newNames)
    object <- convertSampleIdsToNames(object)
    expect_identical(colnames(object), newNames)
})

test_that("SE : return unmodified", {
    object <- rse
    colData(object)[["sampleName"]] <- NULL
    expect_identical(
        object = convertSampleIdsToNames(object),
        expected = object
    )
})
