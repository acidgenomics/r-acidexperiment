## AcidExperiment 0.2.3 (UNRELEASED)

### Minor changes

- `selectSamples`: The function now automatically relevels factors defined
  in `colData`.

## AcidExperiment 0.2.2 (2021-09-13)

### Major changes

- `aggregate`: Added support via `MARGIN` argument to work either row-wise
  (`1`; default) or column-wise (`2`). `SummarizedExperiment` method has been
  updated to support these conventions. Corresponding `aggregateCols` and
  `aggregateRows` functions have been simplified to merely wrap an `aggregate`
  call with `MARGIN` argument predefined.
- `convertTranscriptsToGenes`: Now all assays are aggregated, rather than
  simplify returning primary `counts` assay.

### Minor changes

- `calculateMetrics`: Updated to support `assay` argument.
- `convertGenesToSymbols`: Removed unnecessary `strict` argument for
  `SummarizedExperiment` method, and simplified handling of mismatched
  `Gene2Symbol` internally.
- `estimateSizeFactors` now supports `assay` argument.
- `integerCounts` now supports `assay` argument.
- `aggregate` keeps track of mapping factor used internally for
  `SummarizedExperiment` method. This is defined as `aggregateRows` or
  `aggregateCols` in the object metadata.

## AcidExperiment 0.2.1 (2021-09-08)

### Minor changes

- Now importing `bapply` from goalie instead of AcidBase.
- Updated internal gene mapping code to better handle mismatched Gene2Symbol
  input, which can happen in pointillism package.

## AcidExperiment 0.2.0 (2021-09-03)

### Major changes

- Reworked and improved unit tests to achieve 100% code coverage.
- `calculateMetrics`: Simplifed internal assert checks. The `matrix` and
  `Matrix` methods (handed off from `SummarizedExperiment` method) now work
  primarily on `rowData` instead of `rowRanges`, since we're not using any
  genomic coordinates here in checks. Prefiltering (disabled by default, but
  can be enabled with `prefilter = TRUE`) now removes column elements (e.g.
  cells and/or samples) with all mito counts (`mitoRatio >= 1`) and an
  artifically high `log10FeaturesPerCount >= 1` value. These settings help
  make 100% code coverage possible on minimal test datasets.
- `makeSummarizedExperiment`: Improved and fixed CLI messages on detected of
  mismatched features to be slotted into `rowRanges`. This can happen when
  incorrect values are defined in `ensemblRelease` or `gffFile`. This should
  help resolve the cryptic CLI error message seen in
  [bcbioRNASeq issue #170](https://github.com/hbc/bcbioRNASeq/issues/170).

### Minor changes

- Made unused `matchesGene2Symbol` and `matchesInterestingGroups` functions
  defunct. These are still reexported in basejump and DESeqAnalysis, which will
  be cleaned up in future releases.
- Improved and hardened CLI messages. Now using `abort` instead of `stop`
  internally, to support better stylized error messages.
- Now calling `toStringInline` instead of base `toString` internally to
  properly handle string handoff to cli package.
- `autopadZeros`: `SummarizedExperiment` method now has `sampleNames` argument,
  which allows the user to selectively pad the sample names column `sampleNames`
  defined in `colData`. Enabled by default, matching previous internal behavior.
- `estimateSizeFactors`: Simplified internal assert checks that look for any
  zero `libSizes`. Also improved code coverage to check for specific values
  returned by `type` argument.

## AcidExperiment 0.1.14 (2021-08-11)

### Major changes

- `convertGenesToSymbols`: Added `strict` and `quiet` arguments. Improved and
  hardened internal code, improving the verbosity of calls to `Gene2Symbol`
  generator. Added additional asserts to ensure that mismatches (e.g. row
  flipping) don't occur.
- `Ensembl2Entrez`, `Gene2Symbol`: Updated formals to passthrough to other
  primary methods defined in AcidGenomes package.

### Minor changes

- Hardened and expanded code coverage for expected gene-to-symbol mapping
  behavior in main `Gene2Symbol` generator, focusing on the `format` argument.

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
