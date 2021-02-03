#' @name Gene2Symbol
#' @inherit AcidGenomes::Gene2Symbol
#' @note Updated 2021-02-01.
#'
#' @inheritParams AcidRoxygen::params
#'
#' @examples
#' data(RangedSummarizedExperiment, package = "AcidTest")
#'
#' ## SummarizedExperiment ====
#' object <- RangedSummarizedExperiment
#' x <- Gene2Symbol(object)
#' print(x)
NULL



## Updated 2021-02-02.
`Gene2Symbol,SE` <-  # nolint
    function(object, format) {
        object <- as.SummarizedExperiment(object)
        df <- rowData(object)
        rownames(df) <- rownames(object)
        do.call(what = Gene2Symbol, args = list(object = df, format = format))
    }

formals(`Gene2Symbol,SE`) <-
    methodFormals(
        f = "Gene2Symbol",
        signature = "GRanges",
        package = "AcidGenomes"
    )



#' @rdname Gene2Symbol
#' @export
setMethod(
    f = "Gene2Symbol",
    signature = signature("SummarizedExperiment"),
    definition = `Gene2Symbol,SE`
)
