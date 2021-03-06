if (!isTRUE(goalie::hasInternet())) {
    warning("No Internet connection detected.")
    return(invisible(NULL))
}
dir.create("cache", showWarnings = FALSE)
files <- c(
    "bcbio-metadata-demultiplexed-invalid-duplicated.csv",
    "bcbio-metadata-demultiplexed-invalid-legacy-samplename.csv",
    "bcbio-metadata-demultiplexed-invalid-missing-columns.csv",
    "bcbio-metadata-demultiplexed-invalid-sample-id.csv",
    "bcbio-metadata-demultiplexed.csv",
    "bcbio-metadata-invalid-column-name.csv",
    "bcbio-metadata-invalid-description.csv",
    "bcbio-metadata-multiplexed-cellranger.csv",
    "bcbio-metadata-multiplexed-indrops.csv",
    "bcbio-metadata-multiplexed-invalid-duplicated.csv",
    "bcbio-metadata-multiplexed-invalid-missing-columns.csv"
)
mapply(
    FUN = function(remoteDir, file, envir) {
        destfile <- file.path("cache", file)
        if (!file.exists(destfile)) {
            utils::download.file(
                url = paste(remoteDir, file, sep = "/"),
                destfile = destfile
            )
        }
    },
    file = files,
    MoreArgs = list(
        remoteDir = AcidExperimentTestsURL,
        envir = environment()
    )
)
rm(files)
