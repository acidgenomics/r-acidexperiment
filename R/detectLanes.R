#' Detect sequencing lanes
#'
#' @note Updated 2022-05-04.
#' @export
#'
#' @param path `character`.
#' Sequencing file paths, e.g. FASTQs.
#'
#' @param pattern `character(1)`.
#' Lane grep pattern. Defaults to [lanePattern] global variable.
#'
#' @return `integer`.
#' Lane numbers.
#'
#' @examples
#' ## Matching lanes 1-4 for paired-end FASTQ files.
#' files <- paste0(
#'     "sample1",
#'     paste0("_R", seq_len(2L)),
#'     paste0("_L00", seq_len(4L)),
#'     ".fastq.gz"
#' )
#' detectLanes(files)
detectLanes <- function(path, pattern) {
    assert(
        isCharacter(path),
        isString(lanePattern)
    )
    basename <- basename(path)
    if (any(grepl(pattern = pattern, x = basename))) {
        assert(requireNamespaces("stringi"))
        out <- stringi::stri_match_first_regex(
            str = basename,
            pattern = pattern
        )[, 2L]
        out <- as.integer(out)
    } else {
        out <- integer()
    }
    out
}

formals(detectLanes)[["pattern"]] <- lanePattern
