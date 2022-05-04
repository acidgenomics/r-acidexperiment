## Classes =====================================================================

#' @importClassesFrom AcidBase missingOrNULL
#' @importClassesFrom AcidGenomes Ensembl2Entrez Entrez2Ensembl Gene2Symbol
#' Tx2Gene
#' @importClassesFrom IRanges SplitDataFrameList
#' @importClassesFrom S4Vectors DataFrame SimpleList
#' @importClassesFrom SummarizedExperiment SummarizedExperiment
NULL



## S4 generics and methods =====================================================

#' @importFrom AcidGenerics Ensembl2Entrez Entrez2Ensembl Gene2Symbol Tx2Gene
#' aggregateCols aggregateRows as.SummarizedExperiment atomize autopadZeros
#' calculateMetrics camelCase convertGenesToSymbols convertSampleIDsToNames
#' convertSymbolsToGenes convertTranscriptsToGenes correlation dottedCase
#' droplevels2 encode factorize geneNames headtail humanize integerCounts
#' interestingGroups interestingGroups<- leftJoin makeNames makeSampleData
#' mapGenesToIDs mapGenesToRownames mapGenesToSymbols matchSampleColumn melt
#' metrics mutateAt nonzeroRowsAndCols removeNA sampleData sampleData<-
#' selectSamples snakeCase stripGeneVersions stripTranscriptVersions tpm
#' uniteInterestingGroups upperCamelCase
#' @importFrom BiocGenerics %in% as.data.frame colSums combine counts counts<-
#' do.call estimateSizeFactors lapply match organism organism<- rowSums
#' setdiff sizeFactors sizeFactors<- t unlist unsplit
#' @importFrom Biobase sampleNames sampleNames<-
#' @importFrom S4Vectors aggregate complete.cases cor decode droplevels head
#' mcols mcols<- metadata metadata<- na.omit split summary
#' @importFrom SummarizedExperiment assay assay<- assayNames assayNames<- assays
#' assays<- colData colData<- rowData rowData<- rowRanges rowRanges<-
#' @importFrom pipette export import
#'
#' @importMethodsFrom AcidBase headtail
#' @importMethodsFrom AcidGenomes Ensembl2Entrez Entrez2Ensembl Gene2Symbol
#' Tx2Gene stripGeneVersions stripTranscriptVersions
#' @importMethodsFrom AcidPlyr leftJoin mutateAt
#' @importMethodsFrom SummarizedExperiment coerce
#' @importMethodsFrom pipette atomize droplevels2 export factorize import
#' removeNA
#' @importMethodsFrom syntactic camelCase dottedCase makeNames snakeCase
#' upperCamelCase
NULL



# Standard functions ===========================================================

#' @importFrom AcidBase geometricMean initDir lanePattern metadataDenylist
#' methodFunction realpath requireNamespaces standardizeCall
#' @importFrom AcidCLI abort alert alertInfo alertWarning toInlineString
#' @importFrom AcidGenomes detectOrganism emptyRanges
#' @importFrom GenomicRanges GRanges
#' @importFrom IRanges SplitDataFrameList
#' @importFrom S4Vectors DataFrame Rle SimpleList
#' @importFrom SummarizedExperiment SummarizedExperiment
#' @importFrom goalie allAreAtomic allAreMatchingRegex areDisjointSets
#' areIntersectingSets areSetEqual assert bapply cause false hasColnames
#' hasCols hasDimnames hasLength hasNames hasNoDuplicates hasRownames hasRows
#' hasValidDimnames hasValidNames isAFile isAURL isAny isCharacter isFlag
#' isInRange isInt isNonNegative isNumber isPositive isScalar isString
#' isSubset matchesUniqueGeneNames validNames
#' @importFrom methods as is new setMethod setReplaceMethod signature
#' validObject
#' @importFrom utils packageName packageVersion
NULL
