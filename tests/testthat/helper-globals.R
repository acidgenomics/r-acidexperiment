data(
    DFrame,
    GRanges,
    RangedSummarizedExperiment,
    matrix,
    package = "AcidTest",
    envir = environment()
)

stopifnot(
    is(RangedSummarizedExperiment, "RangedSummarizedExperiment")
)

df <- DFrame
gr <- GRanges
mat <- matrix
rse <- RangedSummarizedExperiment

## nolint start
`assays<-` <- SummarizedExperiment::`assays<-`
assay <- SummarizedExperiment::assay
assayNames <- SummarizedExperiment::assayNames
assays <- SummarizedExperiment::assays
colData <- SummarizedExperiment::colData
hasInternet <- goalie::hasInternet
rowData <- SummarizedExperiment::rowData
rowRanges <- SummarizedExperiment::rowRanges
## nolint end
