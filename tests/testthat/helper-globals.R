data(
    DFrame,
    GRanges,
    RangedSummarizedExperiment,
    SummarizedExperiment_transcripts,
    matrix,
    sparseMatrix,
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
sparse <- sparseMatrix
txse <- SummarizedExperiment_transcripts

## nolint start
`assays<-` <- SummarizedExperiment::`assays<-`
assay <- SummarizedExperiment::assay
assayNames <- SummarizedExperiment::assayNames
assays <- SummarizedExperiment::assays
cause <- goalie::cause
colData <- SummarizedExperiment::colData
hasInternet <- goalie::hasInternet
methodFormals <- AcidBase::methodFormals
rowData <- SummarizedExperiment::rowData
rowRanges <- SummarizedExperiment::rowRanges
simpleClass <- AcidBase::simpleClass
## nolint end
