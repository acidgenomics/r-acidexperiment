## Classes =====================================================================

#' @importClassesFrom AcidBase missingOrNULL
#' @importClassesFrom AcidGenomes Ensembl2Ncbi Gene2Symbol Ncbi2Ensembl Tx2Gene
#' @importClassesFrom IRanges SplitDFrameList
#' @importClassesFrom Matrix Matrix
#' @importClassesFrom S4Vectors DFrame SimpleList
#' @importClassesFrom SummarizedExperiment SummarizedExperiment
NULL



## S4 generics and methods =====================================================

#' @importFrom AcidGenerics Ensembl2Ncbi Gene2Symbol Ncbi2Ensembl Tx2Gene
#' aggregateCols aggregateRows as.SummarizedExperiment atomize autopadZeros
#' calculateMetrics camelCase convertGenesToSymbols convertSampleIDsToNames
#' convertSymbolsToGenes convertTranscriptsToGenes correlation dottedCase
#' droplevels2 encode export factorize geneNames headtail humanize import
#' integerCounts interestingGroups interestingGroups<- leftJoin makeNames
#' makeSampleData mapGenesToIDs mapGenesToRownames mapGenesToSymbols
#' matchSampleColumn melt metrics mutateAt nonzeroRowsAndCols removeNA
#' sampleData sampleData<- selectSamples snakeCase stripGeneVersions
#' stripTranscriptVersions tpm uniteInterestingGroups upperCamelCase
#' @importFrom BiocGenerics Map %in% as.data.frame cbind combine counts counts<-
#' do.call estimateSizeFactors lapply match organism organism<- setdiff
#' sizeFactors sizeFactors<- t unlist unsplit
#' @importFrom Biobase sampleNames sampleNames<-
#' @importFrom Matrix colSums rowSums
#' @importFrom MultiAssayExperiment experiments sampleMap
#' @importFrom S4Vectors aggregate complete.cases cor decode droplevels head
#' mcols mcols<- metadata metadata<- na.omit split summary
#' @importFrom SummarizedExperiment assay assay<- assayNames assayNames<- assays
#' assays<- colData colData<- rowData rowData<- rowRanges rowRanges<-
NULL

#' @importMethodsFrom AcidBase headtail
#' @importMethodsFrom AcidGenomes Ensembl2Ncbi Gene2Symbol Ncbi2Ensembl Tx2Gene
#' stripGeneVersions stripTranscriptVersions
#' @importMethodsFrom AcidPlyr leftJoin mutateAt
#' @importMethodsFrom pipette atomize droplevels2 export factorize import
#' removeNA
#' @importMethodsFrom syntactic camelCase dottedCase makeNames snakeCase
#' upperCamelCase
NULL



# Standard functions ===========================================================

#' @importFrom AcidBase geometricMean initDir lanePattern metadataDenylist
#' methodFunction realpath standardizeCall
#' @importFrom AcidCLI abort alert alertInfo alertWarning toInlineString
#' @importFrom AcidGenomes detectOrganism emptyRanges
#' @importFrom GenomicRanges GRanges
#' @importFrom IRanges SplitDataFrameList
#' @importFrom S4Vectors DataFrame Rle SimpleList
#' @importFrom SummarizedExperiment SummarizedExperiment
#' @importFrom goalie allAreAtomic allAreMatchingRegex areDisjointSets
#' areIntersectingSets areSetEqual assert bapply cause false hasColnames
#' hasCols hasDimnames hasDuplicates hasLength hasNames hasNoDuplicates
#' hasRownames hasRows hasValidDimnames hasValidNames isADirectory isAFile
#' isAURL isAny isCharacter isFlag isInRange isInt isNonNegative isNumber
#' isPositive isScalar isString isSubset matchesUniqueGeneNames
#' requireNamespaces validNames
#' @importFrom methods as is new setMethod setReplaceMethod signature
#' validObject
#' @importFrom utils packageName packageVersion sessionInfo
NULL
