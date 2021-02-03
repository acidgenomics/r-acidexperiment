#' @importClassesFrom AcidGenerics missingOrNULL
#' @importClassesFrom AcidGenomes Ensembl2Entrez Entrez2Ensembl Gene2Symbol
#'   Tx2Gene
#' @importClassesFrom IRanges SplitDataFrameList
#' @importClassesFrom S4Vectors DataFrame SimpleList
#' @importClassesFrom SummarizedExperiment SummarizedExperiment
#'
#' @importFrom AcidBase appendToBody bapply formalsList getNameInParent
#'   initDir lanePattern metadataBlacklist methodFormals methodFunction realpath
#'   standardizeCall
#' @importFrom AcidCLI alert alertInfo alertWarning
#' @importFrom AcidGenerics %in% aggregate as.data.frame colSums
#'   cor decode do.call head lapply match
#'   mcols mcols<- metadata metadata<- na.omit rowSums setdiff split
#'   summary t unlist
#' @importFrom AcidGenomes detectOrganism
#' @importFrom Biostrings reverseComplement
#' @importFrom IRanges SplitDataFrameList unsplit
#' @importFrom Matrix fac2sparse
#' @importFrom S4Vectors DataFrame SimpleList
#' @importFrom SummarizedExperiment SummarizedExperiment assay assay<-
#'   assayNames assayNames<- assays assays<- colData colData<- rowData rowData<-
#'   rowRanges rowRanges<-
#' @importFrom goalie allAreAtomic allAreMatchingRegex areDisjointSets
#'   areIntersectingSets areSetEqual assert false hasColnames hasCols
#'   hasDimnames hasLength hasNames hasNoDuplicates hasRownames hasRows
#'   hasValidDimnames hasValidNames isAFile isAURL isAny isCharacter isFlag
#'   isInt isNonNegative isNumber isPositive isScalar isString isSubset
#'   matchesUniqueGeneNames validNames
#' @importFrom methods as coerce is new setMethod setReplaceMethod signature
#'   validObject
#' @importFrom pipette atomize import removeNA
#' @importFrom scales percent
#' @importFrom sessioninfo session_info
#' @importFrom stringr str_match str_pad
#' @importFrom syntactic camelCase snakeCase
#' @importFrom utils packageName packageVersion
#'
#' @importMethodsFrom AcidGenomes Ensembl2Entrez Entrez2Ensembl Gene2Symbol
#'   Tx2Gene stripGeneVersions stripTranscriptVersions
#' @importMethodsFrom SummarizedExperiment coerce
#' @importMethodsFrom pipette as.data.frame coerce droplevels export
NULL



## This is needed to properly declare S4 `as()` coercion methods.
#' @importFrom methods coerce
#' @exportMethod coerce
NULL
