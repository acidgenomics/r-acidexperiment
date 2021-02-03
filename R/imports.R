#' @importClassesFrom AcidGenerics missingOrNULL
#' @importClassesFrom AcidGenomes Ensembl2Entrez Entrez2Ensembl Gene2Symbol
#'   Tx2Gene
#' @importClassesFrom SummarizedExperiment SummarizedExperiment
#'
#' @importFrom AcidBase appendToBody bapply compress formalsList getNameInParent
#'   lanePattern methodFormals methodFunction printString requireNamespaces
#' @importFrom AcidCLI alert alertInfo alertWarning
#' @importFrom AcidGenerics %in% aggregate as.data.frame as.list colSums
#'   complete.cases cor decode do.call expand expand.grid head lapply match
#'   mcols mcols<- merge metadata metadata<- na.omit order rowSums setdiff split
#'   summary t tail unlist
#' @importFrom AcidGenomes detectOrganism
#' @importFrom Biostrings reverseComplement
#' @importFrom IRanges SplitDataFrameList unsplit
#' @importFrom Matrix fac2sparse
#' @importFrom S4Vectors DataFrame List Rle SimpleList
#' @importFrom SummarizedExperiment SummarizedExperiment assay assay<-
#'   assayNames assayNames<- assays assays<- colData colData<- rowData rowData<-
#'   rowRanges rowRanges<-
#' @importFrom goalie allAreAtomic allAreMatchingRegex allAreNotMatchingRegex
#'   areDisjointSets areIntersectingSets areSameLength areSetEqual assert false
#'   hasColnames hasCols hasDimnames hasDims hasDuplicates hasInternet hasLength
#'   hasMetrics hasNames hasNoDuplicates hasNonzeroRowsAndCols hasRows
#'   hasRownames hasSubset hasUniqueCols hasValidDimnames hasValidNames
#'   isADirectory isAFile isAURL isAlpha isAny isCharacter isFlag isGGScale
#'   isGreaterThanOrEqualTo isHeaderLevel isHexColorFunction isInClosedRange
#'   isInLeftOpenRange isInRange isInt isIntegerish isMatchingRegex
#'   isNonNegative isNotMatchingRegex isNumber isPositive isScalar isString
#'   isSubset isSuperset matchesUniqueGeneNames validNames validate
#'   validateClasses
#' @importFrom methods as coerce formalArgs getGeneric getMethod is isGeneric
#'   new selectMethod setAs setClass setClassUnion setGeneric setMethod
#'   setReplaceMethod setOldClass setValidity signature slot slotNames
#'   validObject .hasSlot
#' @importFrom scales percent
#' @importFrom sessioninfo session_info
#' @importFrom stringr boundary str_detect str_dup str_extract str_length
#'   str_match str_pad str_replace_all str_split str_split_fixed str_subset
#'   str_trunc
#' @importFrom utils capture.output data getFromNamespace installed.packages
#'   packageName packageVersion
#'
#' @importMethodsFrom AcidGenomes Tx2Gene stripGeneVersions
#'   stripTranscriptVersions
#' @importMethodsFrom SummarizedExperiment coerce
#' @importMethodsFrom pipette as.data.frame coerce
NULL



## This is needed to properly declare S4 `as()` coercion methods.
#' @importFrom methods coerce
#' @exportMethod coerce
NULL
