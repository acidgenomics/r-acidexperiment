data(
    DataFrame,
    GenomicRanges,
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

df <- DataFrame
gr <- GenomicRanges
mat <- matrix
rse <- RangedSummarizedExperiment
sparse <- sparseMatrix
txse <- SummarizedExperiment_transcripts

## nolint start
CharacterList <- AcidGenerics::CharacterList
GRanges <- pipette::GRanges
GRangesList <- pipette::GRangesList
IRanges <- AcidGenerics::IRanges
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
seqnames <- AcidGenerics::seqnames
simpleClass <- AcidBase::simpleClass
## nolint end
