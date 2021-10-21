## FIXME Need to improve handling of NA gene symbols here.
## FIXME Need to add code coverage for this edge case.



#' @name convertGenesToSymbols
#' @inherit AcidGenerics::convertGenesToSymbols
#' @note Updated 2021-08-10.
#'
#' @inheritParams AcidRoxygen::params
#' @param strict `logical(1)`.
#'   Require that all identifiers contain gene name (symbol) metadata stored
#'   in the object. Disabled by default, to support objects containing
#'   custom gene identifiers, such as FASTA spike-ins.
#' @param ... Additional arguments.
#'
#' @return Modified object of same class.
#'
#' @examples
#' data(RangedSummarizedExperiment, package = "AcidTest")
#'
#' ## SummarizedExperiment ====
#' object <- RangedSummarizedExperiment
#' x <- convertGenesToSymbols(object)
#' print(x)
#' ## Interconvert back to gene IDs.
#' y <- convertSymbolsToGenes(x)
#' print(y)
NULL



## Updated 2021-10-21.
.interconvertGenesAndSymbols <-
    function(
        object,
        strict,
        rownamesCol
    ) {
        validObject(object)
        assert(
            is(object, "SummarizedExperiment"),
            hasRownames(object),
            isFlag(strict),
            isString(rownamesCol)
        )
        if (isTRUE(strict)) {
            rowData <- rowData(object)
            cols <- c("geneId", "geneName")
            assert(
                isSubset(cols, colnames(rowData)),
                all(complete.cases(rowData[, cols, drop = FALSE]))
            )
        }
        g2s <- Gene2Symbol(object, format = "makeUnique", quiet = TRUE)
        assert(areSetEqual(rownames(object), rownames(g2s)))
        g2s <- g2s[rownames(object), , drop = FALSE]
        rn <- unname(as.character(g2s[[rownamesCol]]))
        assert(
            isCharacter(rn),
            hasNoDuplicates(rn),
            areDisjointSets(rownames(object), rn)
        )
        rownames(object) <- rn
        object
    }



## Updated 2021-10-21.
`convertGenesToSymbols,SE` <-  # nolint
    function(
        object,
        strict = FALSE
    ) {
        .interconvertGenesAndSymbols(
            object = object,
            strict = strict,
            rownamesCol = "geneName"
        )
    }



## Updated 2021-10-21.
`convertSymbolsToGenes,SE` <-  # nolint
    function(object, strict = FALSE) {
        .interconvertGenesAndSymbols(
            object = object,
            strict = strict,
            rownamesCol = "geneId"
        )
    }



#' @rdname convertGenesToSymbols
#' @export
setMethod(
    f = "convertGenesToSymbols",
    signature = signature(object = "SummarizedExperiment"),
    definition = `convertGenesToSymbols,SE`
)

#' @rdname convertGenesToSymbols
#' @export
setMethod(
    f = "convertSymbolsToGenes",
    signature = signature(object = "SummarizedExperiment"),
    definition = `convertSymbolsToGenes,SE`
)
