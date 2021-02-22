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
