## FIXME Need to handle NA values here better.
## FIXME This is now causing a row mismatch on NAs...
## FIXME Inform when we're dropping values here...
## FIXME Our methods in GRanges and SummarizedExperiment should NEVER resize?
## FIXME Consider adding allowMissing argument here?
## FIXME This needs to be `FALSE` by default.
## FIXME Add a quiet argument here?
## FIXME Need to allow passthrough of FASTA spike-ins here, for example...



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
            hasNoDuplicates(object),
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
        assert(hasNoDuplicates(out))
        out
    }



## Updated 2021-08-10.
`convertGenesToSymbols,matrix` <-  # nolint
    function(
        object,
        gene2symbol,
        strict = FALSE
    ) {
        assert(hasRownames(object))
        rn <- convertGenesToSymbols(
            object = rownames(object),
            gene2symbol = gene2symbol,
            strict = strict
        )
        assert(identical(rownames(object), names(rn)))
        rownames(object) <- unname(rn[rownames(object)])
        object
    }



## Updated 2020-01-30.
`convertGenesToSymbols,Matrix` <-  # nolint
    `convertGenesToSymbols,matrix`



## Updated 2021-08-10.
`convertGenesToSymbols,GRanges` <-  # nolint
    function(
        object,
        strict = FALSE
    ) {
        validObject(object)
        assert(hasNames(object))
        gene2symbol <- Gene2Symbol(
            object = object,
            format = "makeUnique",
            quiet = TRUE
        )
        names <- convertGenesToSymbols(
            object = names(object),
            gene2symbol = gene2symbol,
            strict = strict
        )
        assert(identical(names(object), names(names)))
        names(object) <- unname(names[names(object)])
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
