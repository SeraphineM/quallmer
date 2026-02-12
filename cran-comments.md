Resubmission (for the first time on CRAN)

## Addressed issues from first submission

The first submission in December 2025 was rejected citing several issues, all of which have now been addressed:

### 1. Description field improvements

* **Model support**: The Description now explicitly lists supported LLM providers (OpenAI, Anthropic, Google, Azure, Ollama).
* **Quantitative analysis methods**: The Description now specifies the implemented methods: Krippendorff's alpha, Fleiss' kappa, and gold-standard validation metrics (accuracy, precision, recall, F1).
* **References with DOIs**: Added DOI references directly in the DESCRIPTION field:
  - Krippendorff (2019) <doi:10.4135/9781071878781>
  - Fleiss (1971) <doi:10.1037/h0031619>
  - Sokolova & Lapalme (2009) <doi:10.1016/j.ipm.2009.03.002>
  - Lincoln & Guba (1985) ISBN:0803924313 (for audit trail methodology)

### 2. Package size

* Package size is now 1.3MB, well under the 5MB limit.

### 3. Missing `\value` in Rd files

* Added `\value` documentation to all exported methods including `print.trail_setting`, `print.qlm_corpus`, and `[.qlm_corpus`.
* Dataset documentation files correctly use `\docType{data}` and do not require `\value` tags.

### 4. Writing to user's home filespace

* `trail_record()`: Uses `cache_dir = NULL` by default (no file writing). Documentation explicitly instructs users to use `tempdir()` for examples and tests.
* `validate_app.R`: This file was removed in a refactoring; the replacement `validate.R` does not write to the filesystem.
* `qlm_trail()`: Uses `path = NULL` by default (no file writing). Examples use `tempfile()` for any saved output.

## R CMD check results

0 errors | 0 warnings | 1 note

* checking CRAN incoming feasibility ... NOTE
  Maintainer: 'Seraphine F. Maerz <seraphine.maerz@unimelb.edu.au>'
  New submission

## Test environments

* Local macOS R 4.5.2: `devtools::check(--as-cran)` — OK
* devtools::check_win_release()
* devtools::check_win_oldrelease()
* devtools::check_win_devel()
* GitHub Actions: Ubuntu, macOS, Windows across R release and devel


