lst <- AcidDevTools::cacheTestFiles(
    pkg = "AcidExperiment",
    files = c(
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
)
cacheDir <- lst[["cacheDir"]]
rm(lst)
