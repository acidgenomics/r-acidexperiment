#' @name convertGenesToSymbols
#' @inherit AcidGenerics::convertGenesToSymbols
#' @note Updated 2021-10-21.
#'
#' @inheritParams AcidRoxygen::params
#' @param ... Additional arguments.
#'
#' @param strict `logical(1)`.
#' Require that all identifiers contain gene name (symbol) metadata stored
#' in the object. Disabled by default, to support objects containing
#' custom gene identifiers, such as FASTA spike-ins.
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
    function(object, from, to, strict) {
        validObject(object)
        assert(
            is(object, "SummarizedExperiment"),
            hasRownames(object),
            isString(from),
            isString(to),
            isFlag(strict)
        )
        if (isTRUE(strict)) {
            rowData <- rowData(object)
            cols <- c(from, to)
            assert(
                isSubset(cols, colnames(rowData)),
                all(complete.cases(rowData[, cols, drop = FALSE])),
                identical(
                    x = rownames(object),
                    y = unname(as.character(rowData[[from]]))
                ),
                msg = "Strict mode check failure."
            )
        }
        g2s <- GeneToSymbol(object, format = "makeUnique", quiet = TRUE)
        assert(areSetEqual(rownames(object), rownames(g2s)))
        g2s <- g2s[rownames(object), , drop = FALSE]
        rn <- unname(as.character(g2s[[to]]))
        assert(
            isCharacter(rn),
            hasNoDuplicates(rn),
            areDisjointSets(rownames(object), rn)
        )
        rownames(object) <- rn
        object
    }



## Updated 2021-10-21.
`convertGenesToSymbols,SE` <- # nolint
    function(object, strict = FALSE) {
        .interconvertGenesAndSymbols(
            object = object,
            from = "geneId",
            to = "geneName",
            strict = strict
        )
    }



## Updated 2021-10-21.
`convertSymbolsToGenes,SE` <- # nolint
    function(object, strict = FALSE) {
        .interconvertGenesAndSymbols(
            object = object,
            from = "geneName",
            to = "geneId",
            strict = strict
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
