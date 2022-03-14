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
#' @note Updated 2021-10-08.
#'
#' @inheritParams AcidRoxygen::params
#' @param ... Additional arguments.
#'
#' @param strict `logical(1)`.
#' Require all genes to match. Recommended by default.
#' If set `FALSE`, instead will return a warning to the user, and subset the
#' genes vector to only include matches.
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



#' Make a gene mapping data frame
#'
#' Contains gene identifiers, gene names (symbols), and alternative (legacy)
#' gene synonyms, when possible.
#'
#' @note Updated 2021-10-08.
#' @noRd
.makeGeneMap <- function(object) {
    validObject(object)
    assert(is(object, "SummarizedExperiment"))
    df <- rowData(object)
    if (!hasCols(df)) {
        return(NULL)
    }
    colnames(df) <- camelCase(colnames(df), strict = TRUE)
    cols <- c("geneId", "geneName")
    assert(
        identical(rownames(df), rownames(object)),
        isSubset(cols, colnames(df))
    )
    cols <- intersect(
        x = colnames(df),
        y = c(cols, "geneSynonyms")
    )
    df <- df[, cols, drop = FALSE]
    df <- decode(df)
    df
}



## Updated 2021-09-02.
.mapGenes <- function(object,
                      genes,
                      strict = TRUE) {
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
        alertFun <- abort
    } else {
        alertFun <- alertWarning
    }
    if (!isSubset("geneIdNoVersion", colnames(object))) {
        suppressMessages({
            object[["geneIdNoVersion"]] <-
                stripGeneVersions(object[["geneId"]])
        })
    }
    if (isSubset("geneSynonyms", colnames(object))) {
        assert(is(object[["geneSynonyms"]], "CharacterList"))
    }
    out <- vapply(
        X = genes,
        FUN = function(x) {
            idx <- match(x = x, table = rownames(object))
            if (isInt(idx)) {
                return(idx)
            }
            idx <- match(x = x, table = object[["geneId"]])
            if (isInt(idx)) {
                return(idx)
            }
            idx <- match(x = x, table = object[["geneIdNoVersion"]])
            if (isInt(idx)) {
                return(idx)
            }
            idx <- match(x = x, table = object[["geneName"]])
            if (isInt(idx)) {
                return(idx)
            }
            if (isSubset("geneSynonyms", colnames(object))) {
                idx <- which(bapply(
                    X = object[["geneSynonyms"]],
                    FUN = function(table) {
                        x %in% table
                    }
                ))
                if (isInt(idx)) {
                    return(idx)
                }
            }
            -1L
        },
        FUN.VALUE = integer(1L)
    )
    if (any(out < 0L)) {
        failures <- genes[which(out < 0L)]
        alertFun(sprintf(
            "Failed to map %d %s: %s.",
            length(failures),
            ngettext(
                n = length(failures),
                msg1 = "gene",
                msg2 = "genes"
            ),
            toInlineString(failures, n = 5L, class = "val")
        ))
    }
    out <- out[out > 0L]
    out
}



## Updated 2021-06-09.
`mapGenesToIDs,SE` <- # nolint
    function(object,
             genes,
             strict = TRUE) {
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



## Updated 2021-08-09.
`mapGenesToRownames,SE` <- # nolint
    function(object,
             genes,
             strict = TRUE) {
        validObject(object)
        assert(
            hasRownames(object),
            isFlag(strict)
        )
        ## Check to see if object contains gene-to-symbol mappings.
        map <- .makeGeneMap(object)
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
                alertFun <- abort
            } else {
                alertFun <- alertWarning
            }
            unmapped <- which(is.na(match))
            if (length(unmapped) > 0L) {
                alertFun(sprintf(
                    "Some genes failed to map: %s.",
                    toInlineString(genes[unmapped], n = 10L, class = "val")
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
`mapGenesToSymbols,SE` <- # nolint
    function(object,
             genes,
             strict = TRUE) {
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



#' @rdname mapGenes
#' @export
setMethod(
    f = "mapGenesToRownames",
    signature = signature(object = "SummarizedExperiment"),
    definition = `mapGenesToRownames,SE`
)

#' @rdname mapGenes
#' @export
setMethod(
    f = "mapGenesToIDs",
    signature = signature(object = "SummarizedExperiment"),
    definition = `mapGenesToIDs,SE`
)

#' @rdname mapGenes
#' @export
setMethod(
    f = "mapGenesToSymbols",
    signature = signature(object = "SummarizedExperiment"),
    definition = `mapGenesToSymbols,SE`
)
