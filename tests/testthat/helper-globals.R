data(
    DFrame,
    GRanges,
    RangedSummarizedExperiment,
    package = "AcidTest",
    envir = environment()
)

stopifnot(
    is(RangedSummarizedExperiment, "RangedSummarizedExperiment")
)

df <- DFrame
gr <- GRanges
rse <- RangedSummarizedExperiment

## nolint start
assay <- SummarizedExperiment::assay
assayNames <- SummarizedExperiment::assayNames
assays <- SummarizedExperiment::assays
`assays<-` <- SummarizedExperiment::`assays<-`
colData <- SummarizedExperiment::colData
rowData <- SummarizedExperiment::rowData
rowRanges <- SummarizedExperiment::rowRanges
## nolint end
