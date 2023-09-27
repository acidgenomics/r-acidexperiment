.pkgName <- packageName()
.pkgVersion <- packageVersion(.pkgName)



#' AcidExperiment test data URL
#'
#' @export
#' @keywords internal
#' @note Updated 2021-02-03.
#'
#' @examples
#' AcidExperimentTestsUrl
AcidExperimentTestsUrl <- # nolint
    paste0(
        "https://r.acidgenomics.com/testdata/acidexperiment/",
        "v", .pkgVersion$major, ".", .pkgVersion$minor # nolint
    )
