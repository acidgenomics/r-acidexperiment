## AcidExperiment 0.1.14 (UNRELEASED)

## AcidExperiment 0.1.13 (2021-08-09)

### Minor changes

- Updated package dependency version cutoffs.
- `Gene2Symbol`: Tightened up internal `match.arg` handling for `format`
  argument, in defined `SummarizedExperiment` method.

## AcidExperiment 0.1.12 (2021-06-10)

### Minor changes

- `mapGenesToRownames`, `mapGenesToIDs`, `mapGenesToSymbols`: Improved the
  flexibility of gene identifier matching for `SummarizedExperiment` method.
  Now also attempts to match against `geneSynonyms` when defined in `rowData`,
  which is incredibly useful for easy matching of deprecated gene names.
- Removed unnecessary `Gene2Symbol` methods for `mapGenesToRownames`,
  `mapGenesToIDs`, and `mapGenesToSymbols`.
- `matchesGene2Symbol` no longer attempts to use the now removed `Gene2Symbol`
  method for `mapGenesToRownames` internally. We simplified this check using
  the internal `.mapGenes` function to locate index position instead.

## AcidExperiment 0.1.11 (2021-06-04)

### Minor changes

- Updated the documentation to provide compatibility with R 4.1 and Bioconductor
  3.13 release.
- Updated dependency versions.

## AcidExperiment 0.1.10 (2021-03-10)

### Minor changes

- `makeSummarizedExperiment`: Relaxed the validity check on syntactically
  valid names in rownames and colnames of the object. Now the function
  informs the user with a CLI message instead of erroring.

## AcidExperiment 0.1.9 (2021-02-25)

### Minor changes

- Renamed all internal instances of "blacklist" to "denylist".
- Added `denylist` override option to `makeSummarizedExperiment`
  (recommended to be left enabled by default), which will be used in
  cBioPortalAnalysis package update.

## AcidExperiment 0.1.8 (2021-02-22)

### Minor changes

- Now enforcing lower camel case for `sampleData` return.
- Simplified generic handling for Matrix methods.

## AcidExperiment 0.1.7 (2021-02-13)

### Minor changes

- More internal NAMESPACE reworks. IRanges classes now defined in AcidGenerics.

## AcidExperiment 0.1.6 (2021-02-11)

### Minor changes

- Internal NAMESPACE rework, using pipette and AcidGenerics.

## AcidExperiment 0.1.5 (2021-02-09)

### Minor changes

- Reworked the list of reexported functions. Now reexporting only from
  SummarizedExperiment and sessioninfo.

## AcidExperiment 0.1.4 (2021-02-08)

### Minor changes

- Now reexporting some commonly used functions from SummarizedExperiment.

## AcidExperiment 0.1.3 (2021-02-08)

### Minor changes

- `sampleData`: Bug fix for unwanted early return on minimal colData defined
  in SummarizedExperiment. This handling is required for downstream automatic
  plotting functions defined in AcidPlots, that make use of "sampleName"
  metadata column.

## AcidExperiment 0.1.2 (2021-02-06)

### Minor changes

- Added `ignoreVersion` option to gene mapping functions.

## AcidExperiment 0.1.1 (2021-02-06)

### Major changes

- Ensuring we're enforcing lower camel case naming conventions for metadata
  describing the columns slotted in `colData`.

### Minor changes

- Imported coercion methods from `SummarizedExperiment`.
- Added `package.R`, describing package info.

## AcidExperiment 0.1.0 (2021-02-05)

Initial release, with code migrated from basejump package.
