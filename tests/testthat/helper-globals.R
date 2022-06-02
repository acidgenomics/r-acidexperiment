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
CharacterList <- IRanges::CharacterList
DataFrame <- S4Vectors::DataFrame
GRanges <- GenomicRanges::GRanges
GRangesList <- GenomicRanges::GRangesList
IRanges <- IRanges::IRanges
SummarizedExperiment <- SummarizedExperiment::SummarizedExperiment
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
seqnames <- GenomeInfoDb::seqnames
simpleClass <- AcidBase::simpleClass
tempdir2 <- AcidBase::tempdir2
unlink2 <- AcidBase::unlink2
## nolint end
