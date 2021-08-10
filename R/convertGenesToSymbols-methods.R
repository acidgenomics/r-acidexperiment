## FIXME Need to handle NA values here better.
## FIXME This is now causing a row mismatch on NAs...
## FIXME Inform when we're dropping values here...
## FIXME Our methods in GRanges and SummarizedExperiment should NEVER resize?



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
#' rse <- RangedSummarizedExperiment
#' object <- rse
#'
#' g2s <- Gene2Symbol(object)
#' print(g2s)
#' genes <- head(g2s[["geneId"]])
#' print(genes)
#'
#' ## character ====
#' x <- convertGenesToSymbols(genes, gene2symbol = g2s)
#' print(x)
#'
#' ## matrix ====
#' samples <- head(colnames(object))
#' counts <- matrix(
#'     data = seq_len(length(genes) * length(samples)),
#'     byrow = TRUE,
#'     nrow = length(genes),
#'     ncol = length(samples),
#'     dimnames = list(genes, samples)
#' )
#' print(counts)
#' x <- convertGenesToSymbols(counts, gene2symbol = g2s)
#' print(x)
#'
#' ## SummarizedExperiment ====
#' x <- convertGenesToSymbols(rse)
#' print(x)
#' ## Interconvert back to gene IDs.
#' y <- convertSymbolsToGenes(x)
#' print(y)
NULL



## FIXME Consider adding allowMissing argument here?
## FIXME This needs to be `FALSE` by default.
## FIXME Add a quiet argument here?
## FIXME Need to allow passthrough of FASTA spike-ins here, for example...

## Updated 2021-08-10.
`convertGenesToSymbols,character` <-  # nolint
    function(
        object,
        gene2symbol,
        strict = FALSE,
        quiet = FALSE
    ) {
        assert(
            isCharacter(object),
            !any(is.na(object)),
            is(gene2symbol, "Gene2Symbol"),
            isFlag(strict),
            isFlag(quiet)
        )
        cols <- c("geneId", "geneName")
        if (!identical(cols, colnames(gene2symbol))) {
            colnames(gene2symbol) <- cols
        }
        validObject(gene2symbol)
        idx <- match(x = object, table = gene2symbol[["geneId"]])
        if (isTRUE(strict)) {
            assert(
                !any(is.na(idx)),
                identical(length(idx), length(object)),
                msg = "Failed to match all genes to symbols."
            )
        }
        out <- as(gene2symbol, "DataFrame")
        out <- out[idx, "geneName", drop = TRUE]
        assert(identical(length(object), length(out)))
        names(out) <- object
        if (any(is.na(out))) {
            if (isFALSE(quiet)) {
                alertWarning(sprintf(
                    "Failed to match genes: %s.",
                    toString(
                        x = names(out)[which(is.na(out))],
                        width = 100L
                    )
                ))
            }
            out[which(is.na(out))] <- names(out)[which(is.na(out))]
        }
        out
    }



## FIXME Can we pass through to character method better here?
## Updated 2021-08-09.
`convertGenesToSymbols,matrix` <-  # nolint
    function(object, gene2symbol) {
        gene2symbol <- convertGenesToSymbols(
            object = rownames(object),
            gene2symbol = gene2symbol
        )
        assert(
            isCharacter(gene2symbol),
            identical(length(gene2symbol), nrow(object))
        )
        rownames(object) <- unname(gene2symbol)
        object
    }



## Updated 2020-01-30.
`convertGenesToSymbols,Matrix` <-  # nolint
    `convertGenesToSymbols,matrix`



## Updated 2021-08-09.
`convertGenesToSymbols,GRanges` <-  # nolint
    function(object) {
        validObject(object)
        assert(hasNames(object))


        ## FIXME Argh need to rethink the handling here.
        ## FIXME Likely need to update the AcidGenomes package as well.
        gene2symbol1 <- Gene2Symbol(object, format = "makeUnique")
        ## FIXME This mapping isn't 1:1...
        gene2symbol2 <- Gene2Symbol(object, format = "1:1")

        assert(
            hasRownames(gene2symbol),
            all(complete.cases(gene2symbol))
        )



        idx <- match(x = names(object), table = rownames(gene2symbol))
        keep <- !is.na(idx)



        if (!all(keep)) {
            n <- sum(!keep)
            object <- object[keep]
        }
        idx <- na.omit(idx)
        assert(
            hasLength(idx),
            msg = "Failed to map any genes to symbols."
        )

        object <- object[idx]


        xxx <- as.character(gene2symbol[["geneName"]])[idx]




        symbols <- gene2symbol[[2L]]
        assert(hasNoDuplicates(symbols))
        names(object) <- as.character(symbols)
        object
    }



## FIXME This CANNOT RESIZE.
## Updated 2021-01-17.
`convertGenesToSymbols,SE` <-  # nolint
    function(object) {
        validObject(object)

        ## FIXME Use similar approach to GRanges method above...
        gene2symbol <- Gene2Symbol(object, format = "1:1")
        ## FIXME Need to handle NA values here.
        symbols <- gene2symbol[[2L]]
        assert(hasNoDuplicates(symbols))



        rownames(object) <- as.character(symbols)
        if (is(object, "RangedSummarizedExperiment")) {
            assert(identical(rownames(object), names(rowRanges(object))))
        }
        object
    }



## Updated 2021-08-09.
`convertSymbolsToGenes,SE` <-  # nolint
    function(object) {
        validObject(object)
        gene2symbol <- Gene2Symbol(object, format = "makeUnique")
        assert(
            identical(
                x = rownames(object),
                y = as.character(gene2symbol[[2L]])
            ),
            hasNoDuplicates(gene2symbol[[1L]]),
            msg = "Failed to map gene symbols back to identifiers."
        )
        rownames(object) <- as.character(gene2symbol[[1L]])
        object
    }



#' @rdname convertGenesToSymbols
#' @export
setMethod(
    f = "convertGenesToSymbols",
    signature = signature("GRanges"),
    definition = `convertGenesToSymbols,GRanges`
)

#' @rdname convertGenesToSymbols
#' @export
setMethod(
    f = "convertGenesToSymbols",
    signature = signature("Matrix"),
    definition = `convertGenesToSymbols,Matrix`
)

#' @rdname convertGenesToSymbols
#' @export
setMethod(
    f = "convertGenesToSymbols",
    signature = signature("SummarizedExperiment"),
    definition = `convertGenesToSymbols,SE`
)

#' @rdname convertGenesToSymbols
#' @export
setMethod(
    f = "convertGenesToSymbols",
    signature = signature("character"),
    definition = `convertGenesToSymbols,character`
)

#' @rdname convertGenesToSymbols
#' @export
setMethod(
    f = "convertGenesToSymbols",
    signature = signature("matrix"),
    definition = `convertGenesToSymbols,matrix`
)



#' @rdname convertGenesToSymbols
#' @export
setMethod(
    f = "convertSymbolsToGenes",
    signature = signature("SummarizedExperiment"),
    definition = `convertSymbolsToGenes,SE`
)
