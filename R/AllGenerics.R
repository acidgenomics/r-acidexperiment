#' @rdname Ensembl2Entrez
#' @export
setGeneric(
    name = "Ensembl2Entrez",
    def = function(object, ...) {
        standardGeneric("Ensembl2Entrez")
    }
)



#' @rdname Ensembl2Entrez
#' @export
setGeneric(
    name = "Entrez2Ensembl",
    def = function(object, ...) {
        standardGeneric("Entrez2Ensembl")
    }
)



#' @rdname Gene2Symbol
#' @export
setGeneric(
    name = "Gene2Symbol",
    def = function(object, ...) {
        standardGeneric("Gene2Symbol")
    }
)



#' @rdname Tx2Gene
#' @export
setGeneric(
    name = "Tx2Gene",
    def = function(object, ...) {
        standardGeneric("Tx2Gene")
    }
)



#' @rdname aggregate
#' @name aggregate
#' @importFrom AcidGenerics aggregate
#' @usage aggregate(x, ...)
#' @export
NULL



#' @rdname aggregateCols
#' @name aggregateCols
#' @importFrom AcidGenerics aggregateCols
#' @usage aggregateCols(x, ...)
#' @export
NULL



#' @rdname aggregateRows
#' @name aggregateRows
#' @importFrom AcidGenerics aggregateRows
#' @usage aggregateRows(x, ...)
#' @export
NULL



#' @rdname coerce-SummarizedExperiment
#' @name as.SummarizedExperiment
#' @importFrom AcidGenerics as.SummarizedExperiment
#' @usage as.SummarizedExperiment(x, ...)
#' @export
NULL



#' @rdname autopadZeros
#' @name autopadZeros
#' @importFrom AcidGenerics autopadZeros
#' @importMethodsFrom syntactic autopadZeros
#' @usage autopadZeros(object, ...)
#' @export
NULL



#' @rdname combine
#' @name combine
#' @importFrom AcidGenerics combine
#' @usage combine(x, y, ...)
#' @export
NULL



#' @rdname convertGenesToSymbols
#' @name convertGenesToSymbols
#' @importFrom AcidGenerics convertGenesToSymbols
#' @usage convertGenesToSymbols(object, ...)
#' @export
NULL

#' @rdname convertGenesToSymbols
#' @name convertSymbolsToGenes
#' @importFrom AcidGenerics convertSymbolsToGenes
#' @usage convertSymbolsToGenes(object, ...)
#' @export
NULL



#' @rdname convertSampleIDsToNames
#' @name convertSampleIDsToNames
#' @importFrom AcidGenerics convertSampleIDsToNames
#' @usage convertSampleIDsToNames(object, ...)
#' @export
NULL



#' @rdname convertTranscriptsToGenes
#' @name convertTranscriptsToGenes
#' @importFrom AcidGenerics convertTranscriptsToGenes
#' @usage convertTranscriptsToGenes(object, ...)
#' @export
NULL



#' @rdname correlation
#' @name correlation
#' @importFrom AcidGenerics correlation
#' @usage correlation(x, y, ...)
#' @export
NULL



#' @rdname counts
#' @name counts
#' @importFrom AcidGenerics counts
#' @usage counts(object, ...)
#' @export
NULL

#' @rdname counts
#' @name counts<-
#' @importFrom AcidGenerics counts<-
#' @usage counts(object, ...) <- value
#' @export
NULL



#' @rdname decode
#' @name decode
#' @importFrom AcidGenerics decode
#' @usage decode(x, ...)
#' @export
NULL



#' @rdname encode
#' @name encode
#' @importFrom AcidGenerics encode
#' @usage encode(x, ...)
#' @export
NULL



#' @rdname estimateSizeFactors
#' @name estimateSizeFactors
#' @importFrom AcidGenerics estimateSizeFactors
#' @usage estimateSizeFactors(object, ...)
#' @export
NULL



#' @rdname export
#' @name export
#' @importFrom AcidGenerics export
#' @usage export(object, ...)
#' @export
NULL



#' @rdname geneNames
#' @name geneNames
#' @importFrom AcidGenerics geneNames
#' @usage geneNames(object, ...)
#' @export
NULL



#' @rdname humanize
#' @name humanize
#' @importFrom AcidGenerics humanize
#' @usage humanize(object, ...)
#' @export
NULL



#' @rdname integerCounts
#' @name integerCounts
#' @importFrom AcidGenerics integerCounts
#' @usage integerCounts(object, ...)
#' @export
NULL



#' @rdname interestingGroups
#' @name interestingGroups
#' @importFrom AcidGenerics interestingGroups
#' @usage interestingGroups(object, ...)
#' @export
NULL

#' @rdname interestingGroups
#' @name interestingGroups<-
#' @importFrom AcidGenerics interestingGroups<-
#' @usage interestingGroups(object, ...)  <- value
#' @export
NULL



#' @rdname makeSampleData
#' @name makeSampleData
#' @importFrom AcidGenerics makeSampleData
#' @usage makeSampleData(object, ...)
#' @export
NULL



#' @rdname mapGenes
#' @name mapGenesToRownames
#' @importFrom AcidGenerics mapGenesToRownames
#' @usage mapGenesToRownames(object, ...)
#' @export
NULL

#' @rdname mapGenes
#' @name mapGenesToIDs
#' @importFrom AcidGenerics mapGenesToIDs
#' @usage mapGenesToIDs(object, ...)
#' @export
NULL

#' @rdname mapGenes
#' @name mapGenesToSymbols
#' @importFrom AcidGenerics mapGenesToSymbols
#' @usage mapGenesToSymbols(object, ...)
#' @export
NULL



#' @rdname matchSampleColumn
#' @name matchSampleColumn
#' @importFrom AcidGenerics matchSampleColumn
#' @usage matchSampleColumn(object, ...)
#' @export
NULL



#' @rdname melt
#' @name melt
#' @importFrom AcidGenerics melt
#' @usage melt(object, ...)
#' @export
NULL



#' @rdname metrics
#' @name metrics
#' @importFrom AcidGenerics metrics
#' @usage metrics(object, ...)
#' @export
NULL



#' @rdname nonzeroRowsAndCols
#' @name nonzeroRowsAndCols
#' @importFrom AcidGenerics nonzeroRowsAndCols
#' @usage nonzeroRowsAndCols(object, ...)
#' @export
NULL



#' @rdname organism
#' @name organism
#' @importFrom AcidGenerics organism
#' @usage organism(object)
#' @export
NULL

#' @rdname organism
#' @name organism<-
#' @importFrom AcidGenerics organism<-
#' @usage organism(object) <- value
#' @export
NULL



#' @rdname sampleData
#' @name sampleData
#' @importFrom AcidGenerics sampleData
#' @usage sampleData(object, ...)
#' @export
NULL

#' @rdname sampleData
#' @name sampleData<-
#' @importFrom AcidGenerics sampleData<-
#' @usage sampleData(object, ...) <- value
#' @export
NULL



#' @rdname sampleNames
#' @name sampleNames
#' @importFrom AcidGenerics sampleNames
#' @usage sampleNames(object)
#' @export
NULL

#' @rdname sampleNames
#' @name sampleNames<-
#' @importFrom AcidGenerics sampleNames<-
#' @usage sampleNames(object) <- value
#' @export
NULL



#' @rdname selectSamples
#' @name selectSamples
#' @importFrom AcidGenerics selectSamples
#' @usage selectSamples(object, ...)
#' @export
NULL



#' @rdname sizeFactors
#' @name sizeFactors
#' @importFrom AcidGenerics sizeFactors
#' @usage sizeFactors(object, ...)
#' @export
NULL

#' @rdname sizeFactors
#' @name sizeFactors<-
#' @importFrom AcidGenerics sizeFactors<-
#' @usage sizeFactors(object, ...) <- value
#' @export
NULL



#' @rdname stripVersions
#' @name stripGeneVersions
#' @importFrom AcidGenerics stripGeneVersions
#' @usage stripGeneVersions(object, ...)
#' @export
NULL

#' @rdname stripVersions
#' @name stripTranscriptVersions
#' @importFrom AcidGenerics stripTranscriptVersions
#' @usage stripTranscriptVersions(object, ...)
#' @export
NULL



#' @rdname subsetPerSample
#' @name subsetPerSample
#' @importFrom AcidGenerics subsetPerSample
#' @usage subsetPerSample(object, ...)
#' @export
NULL



#' @rdname tpm
#' @name tpm
#' @importFrom AcidGenerics tpm
#' @usage tpm(object, ...)
#' @export
NULL



#' @rdname uniteInterestingGroups
#' @name uniteInterestingGroups
#' @importFrom AcidGenerics uniteInterestingGroups
#' @usage uniteInterestingGroups(object, ...)
#' @export
NULL
