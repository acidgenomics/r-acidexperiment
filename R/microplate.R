#' Microtiter plate well identifiers
#'
#' Quickly generate identifiers (with optional prefixes) for 96 and 384 well
#' plates.
#'
#' These plate formats are frequently used for high-throughput screening assays.
#'
#' @note Updated 2023-10-03.
#' @export
#'
#' @param plates `integer(1)`.
#' Number of plates.
#'
#' @param wells `integer(1)`.
#' Number of wells (`96`, `384`).
#'
#' @param controls `integer(1)`.
#' Number of control wells.
#'
#' @param prefix `character(1)` or `NULL`.
#' Plate name prefix.
#'
#' @return `character`.
#' Well identifiers.
#'
#' @examples
#' ## Single 96-well plate.
#' x <- microplate(wells = 96L)
#' print(head(x))
#'
#' ## 2 96-well plates.
#' x <- microplate(plates = 2L, wells = 96L)
#' print(head(x))
#'
#' ## Single 384-well plate.
#' x <- microplate(wells = 384L)
#' print(head(x))
#'
#' ## 2 96-well plates with 6 control wells per plate.
#' x <- microplate(plates = 2L, wells = 96L, controls = 6L)
#' print(head(x))
microplate <-
    function(plates = 1L,
             wells = 96L,
             controls = 0L,
             prefix = NULL) {
        assert(
            isInt(plates),
            isPositive(plates),
            isInt(wells),
            isPositive(wells),
            isSubset(x = wells, y = c(96L, 384L)),
            isInt(controls),
            isInRange(x = controls, lower = 0L, upper = 12L),
            isString(prefix, nullOk = TRUE)
        )
        plates <- as.integer(plates)
        wells <- as.integer(wells)
        controls <- as.integer(controls)
        switch(
            EXPR = as.character(wells),
            "96" = {
                row <- 8L
                col <- 12L
            },
            "384" = {
                row <- 16L
                col <- 24L
            }
        )
        row <- LETTERS[seq_len(row)]
        col <- as.character(seq_len(col))
        col <- strPad(
            x = col,
            width = max(nchar(col)),
            side = "left",
            pad = "0"
        )
        plates <- as.character(seq_len(plates))
        plates <- strPad(
            x = plates,
            width = max(nchar(plates)),
            side = "left",
            pad = "0"
        )
        df <- expand.grid(plates, row, col)
        assert(identical(colnames(df), c("Var1", "Var2", "Var3")))
        vector <- sort(paste0(df[["Var1"]], "-", df[["Var2"]], df[["Var3"]]))
        ## Prepare control wells.
        if (isTRUE(controls > 0L)) {
            ## Create a grep string matching the control wells.
            grep <- strPad(
                x = as.character(seq_len(controls)),
                width = max(nchar(col)),
                side = "left",
                pad = "0"
            )
            grep <- paste(grep, collapse = "|")
            grep <- paste0("A(", grep, ")$")
            ## Remove the control wells using `grepl`.
            keep <- !grepl(grep, vector)
            vector <- vector[keep]
        }
        ## Add a prefix, if desired.
        if (isString(prefix)) {
            vector <- paste0(prefix, "-", vector)
        }
        ## Return.
        vector
    }
