#' Map genes
#'
#' Take a user-defined gene vector and dynamically map the input to either the
#' object rownames or the gene names (symbols). These functions are useful for
#' writing code that needs to handle either gene identifier or gene name input
#' dynamically (e.g. for single-cell RNA-seq marker analysis).
#'
#' @section Ambiguous gene names:
#'
#' Some genomes (e.g. Homo sapiens, Mus musculus) contain duplicated gene names
#' for multiple gene identifiers. Normally we handle these ambiguous gene names
#' by sanitizing them with `make.names`. If a user requests a gene name that
#' is duplicated, these functions will return a warning.
#'
#' @name mapGenes
#' @note Updated 2021-06-10.
#'
#' @inheritParams AcidRoxygen::params
#' @param strict `logical(1)`.
#'   Require all genes to match. Recommended by default.
#'   If set `FALSE`, instead will return a warning to the user, and subset the
#'   genes vector to only include matches.
#' @param ... Additional arguments.
#'
#' @return `character`.
#'
#' @examples
#' data(RangedSummarizedExperiment, package = "AcidTest")
#'
#' ## SummarizedExperiment ====
#' object <- RangedSummarizedExperiment
#' rownames <- head(rownames(object))
#' print(rownames)
#' g2s <- Gene2Symbol(object)
#' geneIds <- head(g2s[["geneId"]])
#' print(geneIds)
#' geneNames <- head(g2s[["geneName"]])
#' print(geneNames)
#'
#' ## Row names.
#' mapGenesToRownames(object, genes = rownames)
#' mapGenesToRownames(object, genes = geneIds)
#' mapGenesToRownames(object, genes = geneNames)
#'
#' ## Gene identifiers.
#' mapGenesToIDs(object, genes = rownames)
#' mapGenesToIDs(object, genes = geneIds)
#' mapGenesToIDs(object, genes = geneNames)
#'
#' ## Gene names (symbols).
#' mapGenesToSymbols(object, genes = rownames)
#' mapGenesToSymbols(object, genes = geneIds)
#' mapGenesToSymbols(object, genes = geneNames)
NULL



## Internal functions ==========================================================
#' Make a gene mapping data frame
#'
#' Contains gene identifiers, gene names (symbols), and alternative (legacy)
#' gene synonyms, when possible.
#'
#' @note Updated 2021-06-10.
#' @noRd
.makeGeneMap <- function(object) {
    validObject(object)
    assert(is(object, "SummarizedExperiment"))
    suppressMessages({
        g2s <- Gene2Symbol(object, format = "unmodified")
    })
    assert(identical(rownames(g2s), rownames(object)))
    df <- as(g2s, "DataFrame")
    colnames(df) <- camelCase(colnames(df), strict = TRUE)
    if (isSubset("geneSynonyms", colnames(rowData(object)))) {
        df[["geneSynonyms"]] <- rowData(object)[["geneSynonyms"]]
    }
    df
}



## Updated 2021-06-10.
.mapGenes <- function(
    object,
    genes,
    strict = TRUE
) {
    validObject(object)
    assert(
        is(object, "DataFrame"),
        isSubset(
            x = c("geneId", "geneName"),
            y = colnames(object)
        ),
        isCharacter(genes),
        isFlag(strict)
    )
    object <- as(object, "DataFrame")
    if (isTRUE(strict)) {
        alertFun <- stop
    } else {
        alertFun <- alertWarning
    }
    suppressMessages({
        object[["geneIdNoVersion"]] <-
            stripGeneVersions(object[["geneId"]])
    })
    out <- vapply(
        X = genes,
        FUN = function(x) {
            idx <- match(x = x, table = rownames(object))
            if (isInt(idx)) return(idx)
            idx <- match(x = x, table = object[["geneId"]])
            if (isInt(idx)) return(idx)
            idx <- match(x = x, table = object[["geneIdNoVersion"]])
            if (isInt(idx)) return(idx)
            idx <- match(x = x, table = object[["geneName"]])
            if (isInt(idx)) return(idx)
            if (isSubset("geneSynonyms", colnames(object))) {
                idx <- which(bapply(
                    X = object[["geneSynonyms"]],
                    FUN = function(table) {
                        x %in% table
                    }
                ))
                if (isInt(idx)) return(idx)
            }
            alertFun(sprintf("Failed to map gene: %s.", x))
            -1L
        },
        FUN.VALUE = integer(1L)
    )
    out <- out[out > 0L]
    out
}



# S4 method definitions =======================================================
## Updated 2021-06-09.
`mapGenesToIDs,SE` <-  # nolint
    function(
        object,
        genes,
        strict = TRUE
    ) {
        validObject(object)
        col <- "geneId"
        map <- .makeGeneMap(object)
        assert(isSubset(col, colnames(map)))
        idx <- .mapGenes(object = map, genes = genes, strict = strict)
        out <- map[idx, col, drop = TRUE]
        names(out) <- names(idx)
        assert(hasNoDuplicates(out))
        out
    }



## Updated 2021-06-09.
`mapGenesToRownames,SE` <-  # nolint
    function(
        object,
        genes,
        strict = TRUE
    ) {
        validObject(object)
        assert(
            hasRownames(object),
            isFlag(strict)
        )
        ## Check to see if object contains gene-to-symbol mappings.
        tryCatch(
            expr = {
                map <- .makeGeneMap(object)
            },
            error = function(e) {
                NULL
            }
        )
        if (!is.null(map)) {
            idx <- .mapGenes(object = map, genes = genes, strict = strict)
            map <- map[idx, , drop = FALSE]
            out <- rownames(map)
            names(out) <- names(idx)
        } else {
            ## Otherwise, match directly against the rownames.
            table <- rownames(object)
            match <- match(x = genes, table = table)
            names(match) <- genes
            ## Stop or warn if there are unmapped genes.
            if (isTRUE(strict)) {
                alertFun <- stop
            } else {
                alertFun <- alertWarning
            }
            unmapped <- which(is.na(match))
            if (length(unmapped) > 0L) {
                alertFun(sprintf(
                    "Some genes failed to map: %s.",
                    toString(genes[unmapped], width = 100L)
                ))
            }
            ## Return the identifiers that map to rownames.
            mapped <- na.omit(match)
            assert(hasLength(mapped))
            out <- table[mapped]
        }
        assert(hasNoDuplicates(out))
        out
    }



## Updated 2021-06-09.
`mapGenesToSymbols,SE` <-  # nolint
    function(
        object,
        genes,
        strict = TRUE
    ) {
        validObject(object)
        col <- "geneName"
        map <- .makeGeneMap(object)
        assert(isSubset(col, colnames(map)))
        idx <- .mapGenes(object = map, genes = genes, strict = strict)
        out <- map[idx, col, drop = TRUE]
        names(out) <- names(idx)
        assert(hasNoDuplicates(out))
        out
    }



## S4 method exports ===========================================================
#' @rdname mapGenes
#' @export
setMethod(
    f = "mapGenesToRownames",
    signature = signature("SummarizedExperiment"),
    definition = `mapGenesToRownames,SE`
)

#' @rdname mapGenes
#' @export
setMethod(
    f = "mapGenesToIDs",
    signature = signature("SummarizedExperiment"),
    definition = `mapGenesToIDs,SE`
)

#' @rdname mapGenes
#' @export
setMethod(
    f = "mapGenesToSymbols",
    signature = signature("SummarizedExperiment"),
    definition = `mapGenesToSymbols,SE`
)
