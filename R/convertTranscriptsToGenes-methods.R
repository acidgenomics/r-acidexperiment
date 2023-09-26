#' @name convertTranscriptsToGenes
#' @inherit AcidGenerics::convertTranscriptsToGenes
#'
#' @note For objects containing a count matrix, the object rows will be
#' collapsed to gene level using `aggregate()`. This applies to our
#' `SummarizedExperiment` method.
#' @note Updated 2021-09-13.
#'
#' @inheritParams AcidRoxygen::params
#' @param ... Additional arguments.
#'
#' @param aggregate `logical(1)`.
#' For objects supporting `dim()`, aggregate counts to gene level and
#' collapse the matrix.
#'
#' @return
#' - `character`: `factor`.
#' Genes in the values, transcripts in the names.
#' - `matrix`, `Matrix`, `SummarizedExperiment`:
#' Object containing counts collapsed to gene level by default
#' (see `aggregate` argument).
#'
#' @seealso
#' - `aggregate()`.
#'
#' @examples
#' data(SummarizedExperiment_transcripts, package = "AcidTest")
#' txse <- SummarizedExperiment_transcripts
#' object <- txse
#'
#' t2g <- TxToGene(object)
#' print(t2g)
#' transcripts <- rownames(object)
#' print(transcripts)
#'
#' ## character ====
#' ## Returns as factor.
#' x <- convertTranscriptsToGenes(transcripts, tx2gene = t2g)
#' print(x)
#' str(x)
#'
#' ## matrix ====
#' ## Note that transcript IDs currently must be in the rows.
#' counts <- counts(object)
#' print(counts)
#' ## Aggregate to gene level.
#' x <- convertTranscriptsToGenes(counts, tx2gene = t2g, aggregate = TRUE)
#' print(x)
#' colSums(x)
#' ## Simply map to rownames.
#' x <- convertTranscriptsToGenes(counts, tx2gene = t2g, aggregate = FALSE)
#' print(x)
#' colSums(x)
#'
#' ## SummarizedExperiment ====
#' x <- convertTranscriptsToGenes(object)
#' print(x)
NULL



## Updated 2021-09-01.
`convertTranscriptsToGenes,character` <- # nolint
    function(object, tx2gene) {
        assert(
            isCharacter(object),
            hasNoDuplicates(object),
            is(tx2gene, "TxToGene")
        )
        ## Arrange the tx2gene to match the input.
        cols <- c("txId", "geneId")
        if (!identical(cols, colnames(tx2gene))) {
            colnames(tx2gene) <- cols
        }
        validObject(tx2gene)
        missing <- setdiff(object, tx2gene[["txId"]])
        if (length(missing) > 0L) {
            abort(sprintf(
                "Failed to match transcripts: %s.",
                toInlineString(missing, n = 10L, class = "val")
            ))
        }
        tx2gene <- tx2gene[
            match(x = object, table = tx2gene[["txId"]]), ,
            drop = FALSE
        ]
        out <- as.factor(tx2gene[["geneId"]])
        names(out) <- tx2gene[["txId"]]
        out
    }



## Consider aggregating the matrix to gene level instead.
## Updated 2020-01-30.
`convertTranscriptsToGenes,matrix` <- # nolint
    function(object, tx2gene, aggregate = TRUE) {
        assert(isFlag(aggregate))
        t2g <- do.call(
            what = convertTranscriptsToGenes,
            args = list(
                "object" = rownames(object),
                "tx2gene" = tx2gene
            )
        )
        if (isTRUE(aggregate)) {
            object <- aggregate(
                x = object,
                by = t2g,
                fun = "sum",
                MARGIN = 1L
            )
        } else {
            rownames(object) <- as.character(t2g)
        }
        object
    }



## Updated 2020-01-30.
`convertTranscriptsToGenes,Matrix` <- # nolint
    `convertTranscriptsToGenes,matrix`



## Updated 2021-09-13.
`convertTranscriptsToGenes,SE` <- # nolint
    function(object) {
        se <- SummarizedExperiment(
            assays = lapply(
                X = assays(object),
                FUN = convertTranscriptsToGenes,
                tx2gene = TxToGene(object)
            ),
            colData = colData(object)
        )
        se
    }



#' @rdname convertTranscriptsToGenes
#' @export
setMethod(
    f = "convertTranscriptsToGenes",
    signature = signature(object = "Matrix"),
    definition = `convertTranscriptsToGenes,Matrix`
)

#' @rdname convertTranscriptsToGenes
#' @export
setMethod(
    f = "convertTranscriptsToGenes",
    signature = signature(object = "SummarizedExperiment"),
    definition = `convertTranscriptsToGenes,SE`
)

#' @rdname convertTranscriptsToGenes
setMethod(
    f = "convertTranscriptsToGenes",
    signature = signature(object = "character"),
    definition = `convertTranscriptsToGenes,character`
)

#' @rdname convertTranscriptsToGenes
#' @export
setMethod(
    f = "convertTranscriptsToGenes",
    signature = signature(object = "matrix"),
    definition = `convertTranscriptsToGenes,matrix`
)
