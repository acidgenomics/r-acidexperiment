#' AcidExperiment
#'
#' Toolkit to extend the functionality of SummarizedExperiment.
#'
#' @keywords internal
#'
#' @importClassesFrom AcidGenerics DataFrame SimpleList SplitDataFrameList
#'   missingOrNULL
#' @importClassesFrom AcidGenomes Ensembl2Entrez Entrez2Ensembl Gene2Symbol
#'   Tx2Gene
#' @importClassesFrom SummarizedExperiment SummarizedExperiment
#'
#' @importMethodsFrom AcidGenomes Ensembl2Entrez Entrez2Ensembl Gene2Symbol
#'   Tx2Gene stripGeneVersions stripTranscriptVersions
#' @importMethodsFrom SummarizedExperiment coerce
#' @importMethodsFrom pipette as.data.frame coerce droplevels export
#'
#' @importFrom AcidBase bapply formalsList geometricMean getNameInParent initDir
#'   lanePattern metadataDenylist methodFunction packageName
#'   packageVersion realpath requireNamespaces standardizeCall
#' @importFrom AcidCLI alert alertInfo alertWarning
#' @importFrom AcidGenerics DataFrame Rle SimpleList SplitDataFrameList %in%
#'   aggregate as.data.frame complete.cases coerce colSums cor decode do.call
#'   head lapply match mcols mcols<- metadata metadata<- na.omit rowSums setdiff
#'   split summary t unlist unsplit
#' @importFrom AcidGenomes detectOrganism emptyRanges
#' @importFrom AcidPlyr leftJoin mutateAt
#' @importFrom SummarizedExperiment SummarizedExperiment assay assay<-
#'   assayNames assayNames<- assays assays<- colData colData<- rowData rowData<-
#'   rowRanges rowRanges<-
#' @importFrom goalie allAreAtomic allAreMatchingRegex areDisjointSets
#'   areIntersectingSets areSetEqual assert cause false hasColnames hasCols
#'   hasDimnames hasLength hasNames hasNoDuplicates hasRownames hasRows
#'   hasValidDimnames hasValidNames isAFile isAURL isAny isCharacter isFlag
#'   isInRange isInt isNonNegative isNumber isPositive isScalar isString
#'   isSubset matchesUniqueGeneNames validNames
#' @importFrom methods as is new setMethod setReplaceMethod signature
#'   validObject
#' @importFrom pipette as_tibble atomize factorize import removeNA
#' @importFrom scales percent
#' @importFrom sessioninfo session_info
#' @importFrom stringr str_length str_match str_pad
#' @importFrom syntactic camelCase makeNames snakeCase
"_PACKAGE"
