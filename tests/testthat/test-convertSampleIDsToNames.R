context("convertSampleIDsToNames")

test_that("SE", {
    object <- rse
    newNames <- letters[seq_len(ncol(object))]
    colData(object)[["sampleName"]] <- as.factor(newNames)
    object <- convertSampleIDsToNames(object)
    expect_identical(colnames(object), newNames)
})

test_that("SE : return unmodified", {
    object <- rse
    colData(object)[["sampleName"]] <- NULL
    expect_message(
        object = convertSampleIDsToNames(object),
        regexp = "unmodified"
    )
})
