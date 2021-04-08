# AcidExperiment

Toolkit to extend the functionality of [SummarizedExperiment][].

## Installation

Requirements: [R][] >= 4.0, [Bioconductor][] >= 3.12.

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

### [Conda][] method

Configure [Conda][] to use the [Bioconda][] channels.

```sh
# Don't install recipe into base environment.
name="r-acidexperiment"
conda create --name="$name" "$name"
conda activate "$name"
R
```

### [Docker][] method

```sh
image="acidgenomics/r-acidexperiment"
workdir="/mnt/work"
docker pull "$image"
docker run -it \
    --volume="${PWD}:${workdir}" \
    --workdir="$workdir" \
    "$image" \
    R
```

[bioconda]: https://bioconda.github.io/
[bioconductor]: https://bioconductor.org/
[conda]: https://conda.io/
[docker]: https://www.docker.com/
[r]: https://www.r-project.org/
[summarizedexperiment]: https://bioconductor.org/packages/SummarizedExperiment/
