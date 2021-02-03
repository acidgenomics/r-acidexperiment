#' Package version
#'
#' @note Updated 2021-02-03.
#' @noRd
.version <- packageVersion(packageName())



#' AcidExperiment test data URL
#'
#' @export
#' @keywords internal
#' @note Updated 2021-02-03.
#'
#' @examples
#' AcidExperimentTestsURL
AcidExperimentTestsURL <-  # nolint
    paste0(
        "https://r.acidgenomics.com/testdata/acidgenomes/",
        "v", .version$major, ".", .version$minor  # nolint
    )
