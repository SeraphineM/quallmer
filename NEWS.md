# quallmer 0.4.0

## New features

* New `qlm_segment()` segments a corpus into thematic or conceptual units using
  an LLM, returning a quanteda corpus analogous to `quanteda::corpus_segment()`
  output. Schema fields become docvars; `docid_` and `segid_` track provenance.
  Enables aspect-based sentiment analysis, thematic coding, and other
  applications requiring variable-length segmentation (#96).

* `qlm_compare()` now supports inter-coder reliability for segmentation tasks.
  When all inputs are segmented corpora produced by `qlm_segment()`, it
  automatically computes Krippendorff's alpha for unitizing (Krippendorff, 2019,
  section 12.6), an extension of alpha designed for variable-length text
  segmentation. Three measures are reported (marked experimental):
  - `u_alpha_nominal` and `u_alpha_binary` measure joint boundary and coding
    reliability across the full segmented continuum.
  - `cu_alpha_nominal` measures coding reliability *conditional on* unitization,
    isolating coding disagreement from boundary disagreement.
  - Per-value `(k)u_alpha_nominal` reports reliability and coverage for each
    individual code, enabling diagnosis of which codes are applied consistently.
  Results include both per-document and overall (concatenated continuum) alpha.

* `as_qlm_coded()` gains `qlm_segment` and `source_text` arguments for
  converting gold-standard data frames to segmented corpora with character
  positions, enabling ICR comparison of LLM segmentation against human-coded
  reference data.

* `qlm_segment()` now accepts a `name` argument stored in corpus metadata for
  rater identification when comparing multiple segmenters via `qlm_compare()`.

## Internal changes

* Removed dependencies on `dplyr` and `tidyr` (#109). Data manipulation now
  uses base R, `vctrs`, and `tibble`, reducing the install footprint. No
  user-visible behavior changes.

# quallmer 0.3.0

## CRAN submission

* Expanded DESCRIPTION with supported LLM providers, method details, and DOI references.
* Added `\value` documentation to all exported methods.
* Fixed HTML validation issue in `qlm_validate()` documentation.

## Internal changes

* Refactored corpus methods to use `qlm_corpus` wrapper class pattern instead of conditional `registerS3method()`, eliminating load-order dependencies and runtime checks (#86).

## Accessor functions

* New `qlm_meta()` accessor function provides stratified access to metadata for `qlm_coded`, `qlm_codebook`, `qlm_comparison`, and `qlm_validation` objects. Metadata is organized into three types following the quanteda convention:
  - `type = "user"` (default): User-specified fields (`name`, `notes`) that can be modified via `qlm_meta<-()`.
  - `type = "object"`: Read-only parameters set at creation time (`batch`, `call`, `chat_args`, `execution_args`, `parent`, `n_units`, `input_type`).
  - `type = "system"`: Read-only environment information (`timestamp`, `ellmer_version`, `quallmer_version`, `R_version`).
* New `qlm_meta<-()` replacement function allows modifying user metadata fields only. Attempting to modify object or system metadata produces an informative error (#72).
* New `codebook()` extractor retrieves the codebook component from `qlm_coded`, `qlm_comparison`, and `qlm_validation` objects. This is a core component accessor analogous to `formula()` for `lm` objects (#72).
* New `inputs()` extractor retrieves the original input data (texts or image paths) from `qlm_coded` objects. The function name mirrors the `inputs` argument in `qlm_code()` (#72).
* These accessor functions replace direct `attr(x, "run")$...` access, providing a stable API for extracting and modifying object metadata and components.

## Build system

* Build system: pkgdown articles now built locally via Makefile to enable caching and avoid API key requirements in CI (#68).

## Gold standard handling and validation improvements

* New `as_qlm_coded()` function replaces `qlm_humancoded()` as the primary function for converting human-coded or external data to `qlm_coded` objects. The new function includes an `is_gold` parameter to mark gold standard objects for automatic detection.
* `as_qlm_coded()` now supports quanteda corpus objects directly via S3 method dispatch. Document variables (docvars) are automatically converted to coded variables, with document names used as identifiers by default. This simplifies the workflow for corpus-based gold standards (#81).
* `qlm_validate()` now auto-detects gold standards marked with `as_qlm_coded(data, is_gold = TRUE)`, making the `gold =` parameter optional when using marked objects. Explicit `gold =` still works for backward compatibility.
* `qlm_validate()` signature changed to `qlm_validate(..., gold, by, ...)` to support validating multiple coded objects against a single gold standard in one call. Results include a `rater` column identifying each object.
* `qlm_humancoded()` is now marked `@keywords internal` but remains exported for backward compatibility. New code should use `as_qlm_coded()`.
* Gold standard objects display `# Gold:     Yes` in their print output for easy identification.
* Improved error messages in `qlm_validate()` detect common mistakes like forgetting `gold =` or misspelling parameter names, with helpful suggestions for correction.

## Confidence intervals and reliability metrics

* `ci` parameter added to `qlm_compare()` and `qlm_validate()` with options `"none"` (default), `"analytic"`, or `"bootstrap"`.
* Bootstrap confidence intervals now work for all metrics in both functions via percentile method with configurable `bootstrap_n` parameter (default 1000).
* Analytic confidence intervals available for ICC (via psych package) and Pearson's r (via cor.test).
* Results include `ci_lower` and `ci_upper` columns when `ci != "none"`.

## Rater identification and combinability

* `qlm_compare()` results now include `rater1`, `rater2`, `rater3`, etc. columns containing the names of compared objects (from `name` attribute), enabling easy identification when combining multiple comparisons with `dplyr::bind_rows()`.
* `qlm_validate()` results now include a `rater` column identifying which object is being validated, enabling easy combining of multiple validations.
* Both functions return data frames (class `qlm_comparison` and `qlm_validation`) instead of lists, making them easier to filter, combine, and analyze.
* Results from multiple `qlm_compare()` or `qlm_validate()` calls can be combined with `bind_rows()` for analysis across multiple coders or conditions.

## API refinements

* `qlm_code()` default `name` parameter changed from `"original"` to `NULL` for cleaner output when names aren't specified.
* Auto-conversion messages now recommend `as_qlm_coded()` instead of `qlm_humancoded()`.

## The quallmer audit trail

* New `notes` parameter in `qlm_code()`, `qlm_replicate()`, and `as_qlm_coded()` for documenting the rationale behind each coding run. Notes are displayed in print output and captured in `qlm_trail()`.
* The trail API has been simplified to a single function following Lincoln and Guba's (1985) audit trail concept for establishing trustworthiness in qualitative research.
* `qlm_trail()` now accepts an optional `path` argument. When provided, saves RDS archive and generates Quarto report with full audit trail documentation.
* The Quarto report includes all Lincoln and Guba audit trail components: instrument development (codebooks), process notes (run parameters and timeline), data reconstruction (comparisons and validations), and raw data summary.
* New replication section in generated reports provides environment setup instructions, API credential configuration, and executable R code to replicate each coding run.
* Removed helper functions: `qlm_trail_save()`, `qlm_trail_export()`, `qlm_trail_report()`, and `qlm_archive()`. Use `qlm_trail(..., path = "filename")` instead.
* `qlm_trail()` now generates fallback names for objects with missing `name` attribute.

# quallmer 0.2.0

## The quallmer audit trail

* New `qlm_trail()` function creates complete audit trails following Lincoln and Guba's (1985) concept for establishing trustworthiness in qualitative research.
* Use `qlm_trail(..., path = "filename")` to save RDS archive and generate Quarto report.
* Trail print output shows summaries of comparisons and validations (level, subjects, raters, etc.) for better visibility into workflow assessment steps.
* All `qlm_comparison` and `qlm_validation` objects include run attributes capturing parent relationships, enabling full workflow traceability.
* Audit trail automatically captures branching workflows when multiple coded objects are compared or validated.

## New API

The package introduces a new `qlm_*()` API with richer return objects and clearer terminology for qualitative researchers:

* `qlm_codebook()` defines coding instructions, replacing `task()` (#27).
* `qlm_code()` executes coding tasks and returns a tibble with coded results and metadata as attributes, replacing `annotate()` (#27). The returned `qlm_coded` object prints as a tibble and can be used directly in data manipulation workflows. Now includes `name` parameter for tracking runs and hierarchical attribute structure with provenance support.
* `qlm_compare()` compares multiple `qlm_coded` objects to assess inter-rater reliability. Automatically computes all statistically appropriate measures from the irr package based on the specified measurement level (nominal, ordinal, or interval).
* `qlm_validate()` validates a `qlm_coded` object against a gold standard (human-coded reference data). Automatically computes all statistically appropriate metrics based on the specified measurement level, using measures from the yardstick, irr, and stats packages. For nominal data, supports multiple averaging methods (macro, micro, weighted, or per-class breakdown).
* `qlm_replicate()` re-executes coding with optional overrides (model, codebook, parameters) while tracking provenance chain. Enables systematic assessment of coding reliability and sensitivity to model choices.

The new API uses the `qlm_` prefix to avoid namespace conflicts (e.g., with `ggplot2::annotate()`) and follows the convention of verbs for workflow actions, nouns for accessor functions.

### Restructured qlm_coded objects

* `qlm_coded` objects now use a hierarchical attribute structure with a `run` list containing `name`, `batch`, `call`, `codebook`, `chat_args`, `execution_args`, `metadata`, and `parent` fields. This structure supports provenance tracking across replication chains and provides clearer organization of coding metadata (#26).
  - The `batch` flag indicates whether batch processing was used.
  - `execution_args` replaces `pcs_args` and stores all non-chat execution arguments for both parallel and batch processing. Old objects with `pcs_args` remain compatible.

## Example codebooks

* New example codebook data object `data_codebook_sentiment` provides a ready-to-use codebook for sentiment analysis. 
* All predefined `task_*()` functions are deprecated in favor of using the data objects or creating custom codebooks with `qlm_codebook()`.

## Deprecated and superseded functions

* `task()` is deprecated in favor of `qlm_codebook()` (#27).
* `annotate()` is deprecated in favor of `qlm_code()` (#27).
* `validate()` is superseded by `qlm_compare()` (for inter-rater reliability) and `qlm_validate()` (for gold standard validation). The function remains available but is marked with a lifecycle badge.
* Trail functions (`trail_settings()`, `trail_record()`, `trail_compare()`, `trail_matrix()`, `trail_icr()`) are deprecated. Use `qlm_code()` with model and temperature parameters directly, or `qlm_replicate()` for systematic comparisons across models.

**Backward compatibility**: Old code continues to work with deprecation warnings. New `qlm_codebook` objects work with old `annotate()`, and old `task` objects work with new `qlm_code()`. This is achieved through dual-class inheritance where `qlm_codebook` inherits from both `"qlm_codebook"` and `"task"`.

## Package restructuring

* `validate_app()` has been extracted into the companion package [quallmer.app](https://github.com/quallmer/quallmer.app). This reduces dependencies in the core quallmer package (removing shiny, bslib, and htmltools from Imports). Install quallmer.app separately for interactive validation functionality.

## Other changes

- `qlm_validate()` now uses distinct, statistically appropriate metrics for each measurement level:
  - **Nominal** (`level = "nominal"`): accuracy, precision, recall, F1-score, Cohen's kappa (unweighted)
  - **Ordinal** (`level = "ordinal"`): Spearman's rho, Kendall's tau, MAE (mean absolute error)
  - **Interval/Ratio** (`level = "interval"`): ICC (intraclass correlation), Pearson's r, MAE, RMSE (root mean squared error)

  The `measure` argument has been removed entirely - all appropriate measures are now computed automatically based on the `level` parameter. Function signature changed: `level` now comes before `average`, and `average` only applies to nominal (multiclass) data. Return values renamed for consistency: `spearman` → `rho`, `kendall` → `tau`, `pearson` → `r`. Print output uses "levels" terminology for ordinal data and "classes" for nominal data. This change provides more statistically sound validation that respects the mathematical properties of each measurement scale.

- `qlm_compare()` now computes all statistically appropriate measures for each measurement level:
  - **Nominal** (`level = "nominal"`): Krippendorff's alpha (nominal), Cohen's/Fleiss' kappa, percent agreement
  - **Ordinal** (`level = "ordinal"`): Krippendorff's alpha (ordinal), weighted kappa (2 raters only), Kendall's W, Spearman's rho, percent agreement
  - **Interval/Ratio** (`level = "interval"`): Krippendorff's alpha (interval), ICC (intraclass correlation), Pearson's r, percent agreement

  The `measure` argument has been removed entirely - all appropriate measures are now computed automatically and returned in the result object. The return structure changed from a single value to a list containing all computed measures for the specified level. Percent agreement is now computed for all levels; for ordinal/interval/ratio data, the `tolerance` parameter controls what counts as agreement (e.g., `tolerance = 1` means values within 1 unit are considered in agreement).
- New `qlm_humancoded()` function converts human-coded data frames into `qlm_humancoded` objects (dual inheritance: `qlm_humancoded` + `qlm_coded`), enabling full provenance tracking for human coding alongside LLM results. Supports custom metadata for coder information, training details, and coding instructions (#43).
- `qlm_validate()` and `qlm_compare()` now accept plain data frames and automatically convert them to `qlm_humancoded` objects with an informational message. Users can call `qlm_humancoded()` directly to provide richer metadata (coder names, instructions, etc.) or use plain data frames for quick comparisons (#43).
- `qlm_validate()` and `qlm_compare()` now support non-standard evaluation (NSE) for the `by` argument, allowing both `by = sentiment` (unquoted) and `by = "sentiment"` (quoted) syntax. This provides a more natural, tidyverse-style interface while maintaining backward compatibility (#43).
- Print method for `qlm_coded` objects now distinguishes human from LLM coding, displaying "Source: Human coder" for `qlm_humancoded` objects instead of model information.
- Improved error messages in `qlm_compare()` and `qlm_validate()` now show which objects are missing the requested variable and list available alternatives.
- Adopt tidyverse-style error messaging via `cli::cli_abort()` and `cli::cli_warn()` throughout the package, replacing all `stop()`, `stopifnot()`, and `warning()` calls with structured, informative error messages.
- Documentation and CI notes refreshed.

