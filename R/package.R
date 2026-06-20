#' AcidExperiment
#'
#' Toolkit to extend the functionality of SummarizedExperiment.
#'
#' @keywords internal
"_PACKAGE"


## S4 classes ==================================================================

#' @importClassesFrom AcidBase missingOrNULL
#' @importClassesFrom AcidGenomes EnsemblToNcbi GeneToSymbol NcbiToEnsembl
#' @importClassesFrom AcidGenomes TxToGene
#' @importClassesFrom IRanges SplitDFrameList
#' @importClassesFrom Matrix Matrix
#' @importClassesFrom S4Vectors DFrame SimpleList
#' @importClassesFrom SummarizedExperiment SummarizedExperiment
NULL


## S4 generics and methods =====================================================

#' @importFrom AcidGenerics EnsemblToNcbi GeneToSymbol NcbiToEnsembl TxToGene
#' @importFrom AcidGenerics aggregateCols aggregateRows as.SummarizedExperiment
#' @importFrom AcidGenerics atomize autopadZeros calculateMetrics camelCase
#' @importFrom AcidGenerics convertGenesToSymbols convertSampleIdsToNames
#' @importFrom AcidGenerics convertSymbolsToGenes convertTranscriptsToGenes
#' @importFrom AcidGenerics correlation dottedCase droplevels2 encode export
#' @importFrom AcidGenerics factorize geneNames headtail humanize import
#' @importFrom AcidGenerics integerCounts interestingGroups interestingGroups<-
#' @importFrom AcidGenerics leftJoin makeNames makeSampleData mapGenesToIds
#' @importFrom AcidGenerics mapGenesToRownames mapGenesToSymbols
#' @importFrom AcidGenerics matchSampleColumn melt metrics mutateAt
#' @importFrom AcidGenerics nonzeroRowsAndCols removeNa sampleData sampleData<-
#' @importFrom AcidGenerics selectSamples snakeCase stripGeneVersions
#' @importFrom AcidGenerics stripTranscriptVersions tpm uniteInterestingGroups
#' @importFrom AcidGenerics upperCamelCase
#' @importFrom BiocGenerics Map %in% as.data.frame cbind combine counts counts<-
#' @importFrom BiocGenerics do.call estimateSizeFactors lapply match organism
#' @importFrom BiocGenerics organism<- setdiff sizeFactors sizeFactors<- t
#' @importFrom BiocGenerics unlist unsplit
#' @importFrom Biobase sampleNames sampleNames<-
#' @importFrom Matrix colSums rowSums
#' @importFrom S4Vectors aggregate complete.cases cor decode droplevels head
#' @importFrom S4Vectors mcols mcols<- metadata metadata<- na.omit split summary
#' @importFrom SummarizedExperiment assay assay<- assayNames assayNames<- assays
#' @importFrom SummarizedExperiment assays<- colData colData<- rowData rowData<-
#' @importFrom SummarizedExperiment rowRanges rowRanges<-
NULL

#' @importMethodsFrom AcidBase headtail
#' @importMethodsFrom AcidGenomes EnsemblToNcbi GeneToSymbol NcbiToEnsembl
#' @importMethodsFrom AcidGenomes TxToGene stripGeneVersions
#' @importMethodsFrom AcidGenomes stripTranscriptVersions
#' @importMethodsFrom AcidPlyr leftJoin mutateAt
#' @importMethodsFrom pipette atomize droplevels2 export factorize import
#' @importMethodsFrom pipette removeNa
#' @importMethodsFrom syntactic camelCase dottedCase makeNames snakeCase
#' @importMethodsFrom syntactic upperCamelCase
NULL


# Standard functions ===========================================================

#' @importFrom AcidBase geometricMean initDir lanePattern metadataDenylist
#' @importFrom AcidBase methodFunction realpath standardizeCall strMatch strPad
#' @importFrom AcidCLI abort alert alertInfo alertWarning toInlineString
#' @importFrom AcidGenomes detectOrganism emptyRanges
#' @importFrom GenomicRanges GRanges
#' @importFrom IRanges SplitDataFrameList
#' @importFrom S4Vectors DataFrame Rle SimpleList
#' @importFrom SummarizedExperiment SummarizedExperiment
#' @importFrom goalie allAreAtomic allAreMatchingRegex areDisjointSets
#' @importFrom goalie areIntersectingSets areSetEqual assert bapply cause false
#' @importFrom goalie hasColnames hasCols hasDimnames hasDuplicates hasLength
#' @importFrom goalie hasNames hasNoDuplicates hasRownames hasRows
#' @importFrom goalie hasValidDimnames hasValidNames isADirectory isAFile
#' @importFrom goalie isAUrl isAny isCharacter isFlag isInRange isInstalled
#' @importFrom goalie isInt isNonNegative isNumber isPositive isScalar isString
#' @importFrom goalie isSubset matchesUniqueGeneNames requireNamespaces
#' @importFrom goalie validNames
#' @importFrom methods as is new setMethod setReplaceMethod signature
#' @importFrom methods validObject
#' @importFrom utils sessionInfo
NULL
