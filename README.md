# AcidExperiment

Toolkit to extend the functionality of [SummarizedExperiment][].

## Installation

Requirements: [R][] >= 4.0, [Bioconductor][] >= 3.11.

### [R][] method

```r
if (!requireNamespace("BiocManager", quietly = TRUE)) {
    install.packages("BiocManager")
}
install.packages(
    pkgs = "AcidExperiment",
    repos = c(
        "https://r.acidgenomics.com",
        BiocManager::repositories()
    )
)
```

[bioconductor]: https://bioconductor.org/
[r]: https://www.r-project.org/
[summarizedexperiment]: https://bioconductor.org/packages/SummarizedExperiment/
