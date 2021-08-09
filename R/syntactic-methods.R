#' Syntactic naming functions
#'
#' @name syntactic
#' @note Updated 2021-08-09.
#'
#' @inheritParams AcidRoxygen::params
#' @inheritParams syntactic::camelCase
#' @param assayNames `logical(1)`.
#'   Sanitize assay names.
#' @param colData `logical(1)`.
#'   Sanitize column names of column data.
#' @param mcols `logical(1)`.
#'   Sanitize names of metadata columns (i.e. `DataFrame`).
#' @param metadata `logical(1)`.
#'   Sanitize metadata names.
#' @param rowData `logical(1)`.
#'   Sanitize the row data names.
#' @param rownames `logical(1)`.
#'   Apply sanitization on row names. This is not generally recommended by
#'   default, since rownames commonly contain gene identifiers that should not
#'   be modified.
#' @param ... Additional arguments.
#'
#' @examples
#' data(syntactic, package = "AcidTest")
#' lapply(syntactic, camelCase)
NULL



`camelCase,atomic` <-  # nolint
    function(object, names = TRUE, strict = TRUE) {
        assert(
            isFlag(names),
            isFlag(strict)
        )
        if (isTRUE(names) && hasNames(object)) {
            names(object) <- camelCase(names(object), strict = strict)
        }
        object
    }



`camelCase,factor` <-  # nolint
    function(object, names = TRUE, strict = TRUE) {
        assert(
            isFlag(names),
            isFlag(strict)
        )
        if (isTRUE(names) && hasNames(object)) {
            names <- camelCase(names(object), strict = strict)
        } else {
            names <- names(object)
        }
        object <- as.character(object)
        object <- camelCase(object, strict = strict)
        object <- as.factor(object)
        names(object) <- names
        object
    }



`camelCase,list` <- `camelCase,atomic`  # nolint



`camelCase,matrix` <-  # nolint
    function(
        object,
        rownames = FALSE,
        colnames = TRUE,
        strict = TRUE
    ) {
        assert(
            hasDimnames(object),
            isFlag(rownames),
            isFlag(colnames),
            isFlag(strict)
        )
        if (isTRUE(rownames) && hasRownames(object)) {
            rownames(object) <- camelCase(rownames(object), strict = strict)
        }
        if (isTRUE(colnames) && hasColnames(object)) {
            colnames(object) <- camelCase(colnames(object), strict = strict)
        }
        object
    }



`camelCase,data.frame` <- `camelCase,matrix`  # nolint



`camelCase,Vector` <-  # nolint
    function(
        object,
        names = TRUE,
        mcols = TRUE,
        metadata = TRUE,
        strict = TRUE
    ) {
        assert(
            isFlag(names),
            isFlag(mcols),
            isFlag(metadata),
            isFlag(strict)
        )
        if (isTRUE(names) && hasNames(object)) {
            names(object) <-
                camelCase(names(object), strict = strict)
        }
        if (isTRUE(mcols) && hasNames(mcols(object))) {
            names(mcols(object)) <-
                camelCase(names(mcols(object)), strict = strict)
        }
        if (isTRUE(metadata) && hasNames(metadata(object))) {
            names(metadata(object)) <-
                camelCase(names(metadata(object)), strict = strict)
        }
        object
    }



`camelCase,DataFrame` <-  # nolint
    function(
        object,
        rownames = FALSE,
        colnames = TRUE,
        mcols = TRUE,
        metadata = TRUE,
        strict = TRUE
    ) {
        assert(
            hasDimnames(object),
            isFlag(rownames),
            isFlag(colnames),
            isFlag(mcols),
            isFlag(metadata),
            isFlag(strict)
        )
        if (isTRUE(rownames) && hasRownames(object)) {
            rownames(object) <- camelCase(rownames(object), strict = strict)
        }
        if (isTRUE(colnames) && hasColnames(object)) {
            colnames(object) <- camelCase(colnames(object), strict = strict)
        }
        if (isTRUE(mcols) && hasNames(mcols(object))) {
            names(mcols(object)) <-
                camelCase(names(mcols(object)), strict = strict)
        }
        if (isTRUE(metadata) && hasNames(metadata(object))) {
            names(metadata(object)) <-
                camelCase(names(metadata(object)), strict = strict)
        }
        object

    }



`camelCase,Ranges` <- `camelCase,Vector`  # nolint
formals(`camelCase,Ranges`)[c("mcols", "names")] <- c(TRUE, FALSE)



`camelCase,Matrix` <- `camelCase,matrix`  # nolint



`camelCase,SummarizedExperiment` <-  # nolint
    function(
        object,
        rownames = FALSE,
        colnames = TRUE,
        assayNames = TRUE,
        rowData = TRUE,
        colData = TRUE,
        metadata = TRUE,
        strict = TRUE
    ) {
        assert(
            isFlag(rownames),
            isFlag(colnames),
            isFlag(assayNames),
            isFlag(rowData),
            isFlag(colData),
            isFlag(metadata),
            isFlag(strict)
        )
        if (isTRUE(rownames) && hasRownames(object)) {
            rownames(object) <- camelCase(rownames(object), strict = strict)
        }
        if (isTRUE(colnames) && hasColnames(object)) {
            colnames(object) <- camelCase(colnames(object), strict = strict)
        }
        if (isTRUE(assayNames) && isCharacter(assayNames(object))) {
            ## `assayNames<-` assignment method doesn't work reliably.
            names(assays(object)) <-
                camelCase(names(assays(object)), strict = strict)
        }
        if (isTRUE(rowData) && hasColnames(rowData(object))) {
            colnames(rowData(object)) <-
                camelCase(colnames(rowData(object)), strict = strict)
        }
        if (isTRUE(colData) && hasColnames(colData(object))) {
            colnames(colData(object)) <-
                camelCase(colnames(colData(object)), strict = strict)
        }
        if (isTRUE(metadata) && hasNames(metadata(object))) {
            names(metadata(object)) <-
                camelCase(names(metadata(object)), strict = strict)
        }
        object
    }



`dottedCase,atomic` <-  # nolint
    function(object, names = TRUE) {
        assert(isFlag(names))
        if (isTRUE(names) && hasNames(object)) {
            names(object) <- dottedCase(names(object))
        }
        object
    }



`dottedCase,factor` <-  # nolint
    function(object, names = TRUE) {
        assert(isFlag(names))
        if (isTRUE(names) && hasNames(object)) {
            names <- dottedCase(names(object))
        } else {
            names <- names(object)
        }
        object <- as.character(object)
        object <- dottedCase(object)
        object <- as.factor(object)
        names(object) <- names
        object
    }



`dottedCase,list` <- `dottedCase,atomic`  # nolint



`dottedCase,matrix` <-  # nolint
    function(
        object,
        rownames = FALSE,
        colnames = TRUE
    ) {
        assert(
            hasDimnames(object),
            isFlag(rownames),
            isFlag(colnames)
        )
        if (isTRUE(rownames) && hasRownames(object)) {
            rownames(object) <- dottedCase(rownames(object))
        }
        if (isTRUE(colnames) && hasColnames(object)) {
            colnames(object) <- dottedCase(colnames(object))
        }
        object
    }



`dottedCase,data.frame` <- `dottedCase,matrix`  # nolint



`dottedCase,Vector` <-  # nolint
    function(
        object,
        names = TRUE,
        mcols = TRUE,
        metadata = TRUE
    ) {
        assert(
            isFlag(names),
            isFlag(mcols),
            isFlag(metadata)
        )
        if (isTRUE(names) && hasNames(object)) {
            names(object) <- dottedCase(names(object))
        }
        if (isTRUE(mcols) && hasNames(mcols(object))) {
            names(mcols(object)) <- dottedCase(names(mcols(object)))
        }
        if (isTRUE(metadata) && hasNames(metadata(object))) {
            names(metadata(object)) <- dottedCase(names(metadata(object)))
        }
        object
    }



`dottedCase,DataFrame` <-  # nolint
    function(
        object,
        rownames = FALSE,
        colnames = TRUE,
        mcols = TRUE,
        metadata = TRUE
    ) {
        assert(
            hasDimnames(object),
            isFlag(rownames),
            isFlag(colnames),
            isFlag(mcols),
            isFlag(metadata)
        )
        if (isTRUE(rownames) && hasRownames(object)) {
            rownames(object) <- dottedCase(rownames(object))
        }
        if (isTRUE(colnames) && hasColnames(object)) {
            colnames(object) <- dottedCase(colnames(object))
        }
        if (isTRUE(mcols) && hasNames(mcols(object))) {
            names(mcols(object)) <- dottedCase(names(mcols(object)))
        }
        if (isTRUE(metadata) && hasNames(metadata(object))) {
            names(metadata(object)) <- dottedCase(names(metadata(object)))
        }
        object

    }



`dottedCase,Ranges` <- `dottedCase,Vector`  # nolint
formals(`dottedCase,Ranges`)[c("mcols", "names")] <- c(TRUE, FALSE)



`dottedCase,Matrix` <- `dottedCase,matrix`  # nolint



`dottedCase,SummarizedExperiment` <-  # nolint
    function(
        object,
        rownames = FALSE,
        colnames = TRUE,
        assayNames = TRUE,
        rowData = TRUE,
        colData = TRUE,
        metadata = TRUE
    ) {
        assert(
            isFlag(rownames),
            isFlag(colnames),
            isFlag(assayNames),
            isFlag(rowData),
            isFlag(colData),
            isFlag(metadata)
        )
        if (isTRUE(rownames) && hasRownames(object)) {
            rownames(object) <- dottedCase(rownames(object))
        }
        if (isTRUE(colnames) && hasColnames(object)) {
            colnames(object) <- dottedCase(colnames(object))
        }
        if (isTRUE(assayNames) && isCharacter(assayNames(object))) {
            ## `assayNames<-` assignment method doesn't work reliably.
            names(assays(object)) <- dottedCase(names(assays(object)))
        }
        if (isTRUE(rowData) && hasColnames(rowData(object))) {
            colnames(rowData(object)) <- dottedCase(colnames(rowData(object)))
        }
        if (isTRUE(colData) && hasColnames(colData(object))) {
            colnames(colData(object)) <- dottedCase(colnames(colData(object)))
        }
        if (isTRUE(metadata) && hasNames(metadata(object))) {
            names(metadata(object)) <- dottedCase(names(metadata(object)))
        }
        object
    }



`snakeCase,atomic` <-  # nolint
    function(object, names = TRUE) {
        assert(isFlag(names))
        if (isTRUE(names) && hasNames(object)) {
            names(object) <- snakeCase(names(object))
        }
        object
    }



`snakeCase,factor` <-  # nolint
    function(object, names = TRUE) {
        assert(isFlag(names))
        if (isTRUE(names) && hasNames(object)) {
            names <- snakeCase(names(object))
        } else {
            names <- names(object)
        }
        object <- as.character(object)
        object <- snakeCase(object)
        object <- as.factor(object)
        names(object) <- names
        object
    }



`snakeCase,list` <- `snakeCase,atomic`  # nolint



`snakeCase,matrix` <-  # nolint
    function(
        object,
        rownames = FALSE,
        colnames = TRUE
    ) {
        assert(
            hasDimnames(object),
            isFlag(rownames),
            isFlag(colnames)
        )
        if (isTRUE(rownames) && hasRownames(object)) {
            rownames(object) <- snakeCase(rownames(object))
        }
        if (isTRUE(colnames) && hasColnames(object)) {
            colnames(object) <- snakeCase(colnames(object))
        }
        object
    }



`snakeCase,data.frame` <- `snakeCase,matrix`  # nolint



`snakeCase,Vector` <-  # nolint
    function(
        object,
        names = TRUE,
        mcols = TRUE,
        metadata = TRUE
    ) {
        assert(
            isFlag(names),
            isFlag(mcols),
            isFlag(metadata)
        )
        if (isTRUE(names) && hasNames(object)) {
            names(object) <- snakeCase(names(object))
        }
        if (isTRUE(mcols) && hasNames(mcols(object))) {
            names(mcols(object)) <- snakeCase(names(mcols(object)))
        }
        if (isTRUE(metadata) && hasNames(metadata(object))) {
            names(metadata(object)) <- snakeCase(names(metadata(object)))
        }
        object
    }



`snakeCase,DataFrame` <-  # nolint
    function(
        object,
        rownames = FALSE,
        colnames = TRUE,
        mcols = TRUE,
        metadata = TRUE
    ) {
        assert(
            hasDimnames(object),
            isFlag(rownames),
            isFlag(colnames),
            isFlag(mcols),
            isFlag(metadata)
        )
        if (isTRUE(rownames) && hasRownames(object)) {
            rownames(object) <- snakeCase(rownames(object))
        }
        if (isTRUE(colnames) && hasColnames(object)) {
            colnames(object) <- snakeCase(colnames(object))
        }
        if (isTRUE(mcols) && hasNames(mcols(object))) {
            names(mcols(object)) <- snakeCase(names(mcols(object)))
        }
        if (isTRUE(metadata) && hasNames(metadata(object))) {
            names(metadata(object)) <- snakeCase(names(metadata(object)))
        }
        object

    }



`snakeCase,Ranges` <- `snakeCase,Vector`  # nolint
formals(`snakeCase,Ranges`)[c("mcols", "names")] <- c(TRUE, FALSE)



`snakeCase,Matrix` <- `snakeCase,matrix`  # nolint



`snakeCase,SummarizedExperiment` <-  # nolint
    function(
        object,
        rownames = FALSE,
        colnames = TRUE,
        assayNames = TRUE,
        rowData = TRUE,
        colData = TRUE,
        metadata = TRUE
    ) {
        assert(
            isFlag(rownames),
            isFlag(colnames),
            isFlag(assayNames),
            isFlag(rowData),
            isFlag(colData),
            isFlag(metadata)
        )
        if (isTRUE(rownames) && hasRownames(object)) {
            rownames(object) <- snakeCase(rownames(object))
        }
        if (isTRUE(colnames) && hasColnames(object)) {
            colnames(object) <- snakeCase(colnames(object))
        }
        if (isTRUE(assayNames) && isCharacter(assayNames(object))) {
            ## `assayNames<-` assignment method doesn't work reliably.
            names(assays(object)) <- snakeCase(names(assays(object)))
        }
        if (isTRUE(rowData) && hasColnames(rowData(object))) {
            colnames(rowData(object)) <- snakeCase(colnames(rowData(object)))
        }
        if (isTRUE(colData) && hasColnames(colData(object))) {
            colnames(colData(object)) <- snakeCase(colnames(colData(object)))
        }
        if (isTRUE(metadata) && hasNames(metadata(object))) {
            names(metadata(object)) <- snakeCase(names(metadata(object)))
        }
        object
    }



`upperCamelCase,atomic` <-  # nolint
    function(object, names = TRUE, strict = TRUE) {
        assert(
            isFlag(names),
            isFlag(strict)
        )
        if (isTRUE(names) && hasNames(object)) {
            names(object) <- upperCamelCase(names(object), strict = strict)
        }
        object
    }



`upperCamelCase,factor` <-  # nolint
    function(object, names = TRUE, strict = TRUE) {
        assert(
            isFlag(names),
            isFlag(strict)
        )
        if (isTRUE(names) && hasNames(object)) {
            names <- upperCamelCase(names(object), strict = strict)
        } else {
            names <- names(object)
        }
        object <- as.character(object)
        object <- upperCamelCase(object, strict = strict)
        object <- as.factor(object)
        names(object) <- names
        object
    }



`upperCamelCase,list` <- `upperCamelCase,atomic`  # nolint



`upperCamelCase,matrix` <-  # nolint
    function(
        object,
        rownames = FALSE,
        colnames = TRUE,
        strict = TRUE
    ) {
        assert(
            hasDimnames(object),
            isFlag(rownames),
            isFlag(colnames),
            isFlag(strict)
        )
        if (isTRUE(rownames) && hasRownames(object)) {
            rownames(object) <-
                upperCamelCase(rownames(object), strict = strict)
        }
        if (isTRUE(colnames) && hasColnames(object)) {
            colnames(object) <-
                upperCamelCase(colnames(object), strict = strict)
        }
        object
    }



`upperCamelCase,data.frame` <- `upperCamelCase,matrix`  # nolint



`upperCamelCase,Vector` <-  # nolint
    function(
        object,
        names = TRUE,
        mcols = TRUE,
        metadata = TRUE,
        strict = TRUE
    ) {
        assert(
            isFlag(names),
            isFlag(mcols),
            isFlag(metadata),
            isFlag(strict)
        )
        if (isTRUE(names) && hasNames(object)) {
            names(object) <-
                upperCamelCase(names(object), strict = strict)
        }
        if (isTRUE(mcols) && hasNames(mcols(object))) {
            names(mcols(object)) <-
                upperCamelCase(names(mcols(object)), strict = strict)
        }
        if (isTRUE(metadata) && hasNames(metadata(object))) {
            names(metadata(object)) <-
                upperCamelCase(names(metadata(object)), strict = strict)
        }
        object
    }



`upperCamelCase,DataFrame` <-  # nolint
    function(
        object,
        rownames = FALSE,
        colnames = TRUE,
        mcols = TRUE,
        metadata = TRUE,
        strict = TRUE
    ) {
        assert(
            hasDimnames(object),
            isFlag(rownames),
            isFlag(colnames),
            isFlag(mcols),
            isFlag(metadata),
            isFlag(strict)
        )
        if (isTRUE(rownames) && hasRownames(object)) {
            rownames(object) <-
                upperCamelCase(rownames(object), strict = strict)
        }
        if (isTRUE(colnames) && hasColnames(object)) {
            colnames(object) <-
                upperCamelCase(colnames(object), strict = strict)
        }
        if (isTRUE(mcols) && hasNames(mcols(object))) {
            names(mcols(object)) <-
                upperCamelCase(names(mcols(object)), strict = strict)
        }
        if (isTRUE(metadata) && hasNames(metadata(object))) {
            names(metadata(object)) <-
                upperCamelCase(names(metadata(object)), strict = strict)
        }
        object

    }



`upperCamelCase,Ranges` <- `upperCamelCase,Vector`  # nolint
formals(`upperCamelCase,Ranges`)[c("mcols", "names")] <- c(TRUE, FALSE)



`upperCamelCase,Matrix` <- `upperCamelCase,matrix`  # nolint



`upperCamelCase,SummarizedExperiment` <-  # nolint
    function(
        object,
        rownames = FALSE,
        colnames = TRUE,
        assayNames = TRUE,
        rowData = TRUE,
        colData = TRUE,
        metadata = TRUE,
        strict = TRUE
    ) {
        assert(
            isFlag(rownames),
            isFlag(colnames),
            isFlag(assayNames),
            isFlag(rowData),
            isFlag(colData),
            isFlag(metadata),
            isFlag(strict)
        )
        if (isTRUE(rownames) && hasRownames(object)) {
            rownames(object) <-
                upperCamelCase(rownames(object), strict = strict)
        }
        if (isTRUE(colnames) && hasColnames(object)) {
            colnames(object) <-
                upperCamelCase(colnames(object), strict = strict)
        }
        if (isTRUE(assayNames) && isCharacter(assayNames(object))) {
            ## `assayNames<-` assignment method doesn't work reliably.
            names(assays(object)) <-
                upperCamelCase(names(assays(object)), strict = strict)
        }
        if (isTRUE(rowData) && hasColnames(rowData(object))) {
            colnames(rowData(object)) <-
                upperCamelCase(colnames(rowData(object)), strict = strict)
        }
        if (isTRUE(colData) && hasColnames(colData(object))) {
            colnames(colData(object)) <-
                upperCamelCase(colnames(colData(object)), strict = strict)
        }
        if (isTRUE(metadata) && hasNames(metadata(object))) {
            names(metadata(object)) <-
                upperCamelCase(names(metadata(object)), strict = strict)
        }
        object
    }



#' @rdname syntactic
#' @export
setMethod(
    f = "camelCase",
    signature = signature("DataFrame"),
    definition = `camelCase,DataFrame`
)

#' @rdname syntactic
#' @export
setMethod(
    f = "camelCase",
    signature = signature("Matrix"),
    definition = `camelCase,Matrix`
)

#' @rdname syntactic
#' @export
setMethod(
    f = "camelCase",
    signature = signature("Ranges"),
    definition = `camelCase,Ranges`
)

#' @rdname syntactic
#' @export
setMethod(
    f = "camelCase",
    signature = signature("SummarizedExperiment"),
    definition = `camelCase,SummarizedExperiment`
)

#' @rdname syntactic
#' @export
setMethod(
    f = "camelCase",
    signature = signature("Vector"),
    definition = `camelCase,Vector`
)

#' @rdname syntactic
#' @export
setMethod(
    f = "camelCase",
    signature = signature("atomic"),
    definition = `camelCase,atomic`
)

#' @rdname syntactic
#' @export
setMethod(
    f = "camelCase",
    signature = signature("data.frame"),
    definition = `camelCase,data.frame`
)

#' @rdname syntactic
#' @export
setMethod(
    f = "camelCase",
    signature = signature("factor"),
    definition = `camelCase,factor`
)

#' @rdname syntactic
#' @export
setMethod(
    f = "camelCase",
    signature = signature("list"),
    definition = `camelCase,list`
)

#' @rdname syntactic
#' @export
setMethod(
    f = "camelCase",
    signature = signature("matrix"),
    definition = `camelCase,matrix`
)



#' @rdname syntactic
#' @export
setMethod(
    f = "dottedCase",
    signature = signature("DataFrame"),
    definition = `dottedCase,DataFrame`
)

#' @rdname syntactic
#' @export
setMethod(
    f = "dottedCase",
    signature = signature("Matrix"),
    definition = `dottedCase,Matrix`
)

#' @rdname syntactic
#' @export
setMethod(
    f = "dottedCase",
    signature = signature("Ranges"),
    definition = `dottedCase,Ranges`
)

#' @rdname syntactic
#' @export
setMethod(
    f = "dottedCase",
    signature = signature("SummarizedExperiment"),
    definition = `dottedCase,SummarizedExperiment`
)

#' @rdname syntactic
#' @export
setMethod(
    f = "dottedCase",
    signature = signature("Vector"),
    definition = `dottedCase,Vector`
)

#' @rdname syntactic
#' @export
setMethod(
    f = "dottedCase",
    signature = signature("atomic"),
    definition = `dottedCase,atomic`
)

#' @rdname syntactic
#' @export
setMethod(
    f = "dottedCase",
    signature = signature("data.frame"),
    definition = `dottedCase,data.frame`
)

#' @rdname syntactic
#' @export
setMethod(
    f = "dottedCase",
    signature = signature("factor"),
    definition = `dottedCase,factor`
)

#' @rdname syntactic
#' @export
setMethod(
    f = "dottedCase",
    signature = signature("list"),
    definition = `dottedCase,list`
)

#' @rdname syntactic
#' @export
setMethod(
    f = "dottedCase",
    signature = signature("matrix"),
    definition = `dottedCase,matrix`
)



#' @rdname syntactic
#' @export
setMethod(
    f = "snakeCase",
    signature = signature("atomic"),
    definition = `snakeCase,atomic`
)

#' @rdname syntactic
#' @export
setMethod(
    f = "snakeCase",
    signature = signature("data.frame"),
    definition = `snakeCase,data.frame`
)

#' @rdname syntactic
#' @export
setMethod(
    f = "snakeCase",
    signature = signature("factor"),
    definition = `snakeCase,factor`
)

#' @rdname syntactic
#' @export
setMethod(
    f = "snakeCase",
    signature = signature("list"),
    definition = `snakeCase,list`
)

#' @rdname syntactic
#' @export
setMethod(
    f = "snakeCase",
    signature = signature("matrix"),
    definition = `snakeCase,matrix`
)

#' @rdname syntactic
#' @export
setMethod(
    f = "snakeCase",
    signature = signature("DataFrame"),
    definition = `snakeCase,DataFrame`
)

#' @rdname syntactic
#' @export
setMethod(
    f = "snakeCase",
    signature = signature("Matrix"),
    definition = `snakeCase,Matrix`
)

#' @rdname syntactic
#' @export
setMethod(
    f = "snakeCase",
    signature = signature("Ranges"),
    definition = `snakeCase,Ranges`
)

#' @rdname syntactic
#' @export
setMethod(
    f = "snakeCase",
    signature = signature("SummarizedExperiment"),
    definition = `snakeCase,SummarizedExperiment`
)

#' @rdname syntactic
#' @export
setMethod(
    f = "snakeCase",
    signature = signature("Vector"),
    definition = `snakeCase,Vector`
)



#' @rdname syntactic
#' @export
setMethod(
    f = "upperCamelCase",
    signature = signature("DataFrame"),
    definition = `upperCamelCase,DataFrame`
)

#' @rdname syntactic
#' @export
setMethod(
    f = "upperCamelCase",
    signature = signature("Matrix"),
    definition = `upperCamelCase,Matrix`
)

#' @rdname syntactic
#' @export
setMethod(
    f = "upperCamelCase",
    signature = signature("Ranges"),
    definition = `upperCamelCase,Ranges`
)

#' @rdname syntactic
#' @export
setMethod(
    f = "upperCamelCase",
    signature = signature("SummarizedExperiment"),
    definition = `upperCamelCase,SummarizedExperiment`
)

#' @rdname syntactic
#' @export
setMethod(
    f = "upperCamelCase",
    signature = signature("Vector"),
    definition = `upperCamelCase,Vector`
)

#' @rdname syntactic
#' @export
setMethod(
    f = "upperCamelCase",
    signature = signature("atomic"),
    definition = `upperCamelCase,atomic`
)

#' @rdname syntactic
#' @export
setMethod(
    f = "upperCamelCase",
    signature = signature("data.frame"),
    definition = `upperCamelCase,data.frame`
)

#' @rdname syntactic
#' @export
setMethod(
    f = "upperCamelCase",
    signature = signature("factor"),
    definition = `upperCamelCase,factor`
)

#' @rdname syntactic
#' @export
setMethod(
    f = "upperCamelCase",
    signature = signature("list"),
    definition = `upperCamelCase,list`
)

#' @rdname syntactic
#' @export
setMethod(
    f = "upperCamelCase",
    signature = signature("matrix"),
    definition = `upperCamelCase,matrix`
)
