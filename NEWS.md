# quallmer 0.4.0.9000 (development version)

## Bug fixes

* The `.id` column of a `qlm_coded` object must now be a key: unique and
  never missing. Every later operation merges on `.id`, and `qlm_compare()`
  and `qlm_validate()` silently formed a Cartesian product of repeated
  rows, computing their statistics over the wrong pairs, while a missing
  identifier was matched to every other missing one; an unnested table that
  keeps the document identifier rather than a document-item key is how the
  first arises in practice. Both are now errors, naming the offending
  values: at construction, in `qlm_code()` before a request is spent, and
  again in `qlm_compare()` and `qlm_validate()`, since row subsetting keeps
  the class and objects from before the check exist. `as_qlm_coded()` also
  refuses an `id` column alongside an existing `.id`, which left two
  columns of that name with the wrong one read (#156).

* `qlm_code()` no longer returns a response cut off at the provider's
  `max_tokens` limit as a successful empty result. Such a response is billed
  in full, and arrived as a row of `NA` scalars and zero-length arrays with no
  `.error`, indistinguishable from a unit to which nothing applied, so a
  document that overran the limit read as "nothing here", and the documents
  that do so are systematically the longest and richest ones. On the JSON
  path the finish reason that ellmer 0.4.2 attaches to each turn is now read:
  the unit is recorded in `.error` with the token count, listed by
  `qlm_failures()`, and not retried, since the same limit reproduces the cut
  and the retry is billed again. On the structured path ellmer's parallel
  call discards the turns, so the finish reason is out of reach; there, when
  `params(max_tokens = )` is set, a row that used the whole budget and
  returned nothing is recorded in `.error` as cut off. Without a declared
  limit the cap is not known and such a row stays silent, which needs an
  ellmer change to close. Rows whose request failed, or whose response was
  cut off, are also no longer read as evidence that an endpoint ignored the
  schema, so a run whose every unit failed that way is reported as failed
  rather than re-coded in JSON mode; a response ellmer could extract nothing
  from still counts, so `"auto"` still falls back for an endpoint that
  answers in prose (#153).

* `qlm_codebook(levels = )` accepts variables nested inside a `type_array()`
  or a nested `type_object()`. The check matched names against top-level
  schema properties only, so a codebook whose schema returns one array entry
  per rated item could not declare measurement levels for the very variables
  `qlm_compare()` and `qlm_validate()` need them for, and the only workaround
  was to drop `levels` from the codebook and re-attach them by hand after
  unnesting. Property names are now collected at every depth, and for a
  schema whose root is a `type_array()` rather than a `type_object()`, which
  previously skipped the check altogether. A name that occurs at more than
  one place in the schema is an error rather than being resolved silently to
  the first match, since a flat `levels` list cannot say which one it means
  (#131).

* `qlm_trail()` reports which endpoint each run actually used, and points at
  the right ellmer help page for setting it up. The report derived a provider
  by splitting the model string and mapped it through a four-name `if/else`
  chain, which had gone stale three ways. The `google` branch could never fire,
  because ellmer's providers are `google_gemini` and `google_vertex` and a
  `google/` prefix does not resolve at all. Every OpenAI-compatible endpoint --
  Qwen through Alibaba, Kimi through Moonshot, a vLLM box, a laptop -- reported
  the same provider and the same instruction to configure credentials for
  "openai_compatible", which for an audit trail whose purpose is telling runs
  apart was the worst of the three. And roughly twenty providers ellmer ships
  fell through to "configure credentials as needed". Runs are now identified by
  provider *and* `base_url`, the rule `qlm_replicate()` already applies, so
  endpoints differing only by port, path or scheme stay distinct. Credentials
  embedded in a URL, as userinfo or as a query parameter, are stripped from the
  endpoint labels this section prints; note that the Call section still shows
  the call as written and the `.rds` still preserves `chat_args` whole. Ollama
  is told it needs no key only where a loopback endpoint was recorded, since
  ellmer reads `OLLAMA_API_KEY` for one served behind a proxy and resolves an
  unset `base_url` through `OLLAMA_BASE_URL`, which may be remote.
  The section is now "Provider and endpoint setup" rather than "Configure API
  credentials", because several providers use IAM, OAuth or platform
  credentials rather than a key (#130).

* `qlm_compare(level = "interval", tolerance = )` counted a pair differing by
  exactly `tolerance` as disagreement or agreement depending on how the
  subtraction happened to round in binary. `1.1 - 1.0` is
  `0.10000000000000009` and failed at `tolerance = 0.1`, while `1.2 - 1.1` is
  `0.09999999999999987` and passed -- the same nominal difference, opposite
  answers. On decimal-increment scales this is common rather than exotic; one
  reported analysis understated agreement by 23 percentage points, and the
  result stayed plausible enough that nothing looked wrong. The comparison now
  allows a few units in the last place, scaled to the magnitudes being
  compared. Percent agreement can therefore only stay the same or rise: no pair
  that previously agreed becomes a disagreement. Anyone who has reported a
  percent agreement computed on a non-integer scale should recompute it.
  `tolerance = 0` now means numerical equality rather than bit identity, so
  `0.1 + 0.2` counts as equal to `0.3`, while a genuine difference of `1e-9` is
  still a difference. A non-finite rating takes the plain comparison, since an
  infinite magnitude would otherwise scale the allowance to infinity and report
  a finite rating as agreeing with an infinite one (#121).

* `qlm_code()` and `qlm_segment()` now reject a model parameter passed at the
  top level. `max_tokens = 100` used to fall through to `chat_args` and reach
  `ellmer::chat()`, failing with `unused argument (max_tokens = 100)` raised
  from inside ellmer -- naming the argument but neither of the two places it
  could have gone. The error now names both: `params` for the settings ellmer
  standardises, `api_args` for a provider's raw request fields. It deliberately
  does not prescribe one, because the choice is not a property of the name:
  `top_k` is a real `ellmer::params()` field, but ellmer maps it onto
  `top_logprobs` for OpenAI-compatible providers, so a caller wanting a
  provider's raw `top_k` needs `api_args`. `stop` and `response_format` are
  unambiguous and do get a specific destination. The rejected set is read from
  `ellmer::params()` at run time, minus whatever the request path currently
  accepts, so a name a later ellmer gives a real meaning stops being rejected
  without an update here (#139).

* `qlm_code()` and `qlm_segment()` now say what to do when a model's provider
  prefix is not one ellmer can dispatch on. `model = "qwen/qwen3-max"` reached
  `ellmer::chat()` and failed with `Can't find provider ellmer::chat_qwen()`,
  which names an ellmer internal and offers no way forward. The error now names
  every prefix that does work and points at `openai_compatible/<model>` with
  `base_url` and `credentials`, which is how any other OpenAI-compatible
  endpoint is reached. The list of working prefixes is derived from the
  installed ellmer at run time, mirroring both gates `ellmer::chat()` applies,
  so a provider ellmer adds or drops later needs no change here. Checked before
  any request is made, since the answer does not depend on asking the provider
  anything (#129).

* `qlm_trail()` no longer advertises or generates a top-level `temperature`
  argument. That form does not work -- it reaches `ellmer::chat()`, which has no
  such argument -- so the replication script the audit trail generated could not
  run, in the one document meant to show that a run can be reproduced. The
  report now reads the sampling settings a run actually recorded, in
  `chat_args$params`, and emits them as `params = ellmer::params()`. A legacy
  `chat_args$temperature`, which older objects carry from the routing that was
  never implemented, is folded into `params` on read, so an old trail file still
  describes and reproduces itself in the form that works; `params$temperature`
  is canonical and wins when both are present. Values are serialised one at a
  time rather than through `unlist()`, so a vector-valued parameter such as
  `stop = c("END", "STOP")` survives into the generated call (#127).

* `qlm_code()` gains a `structured` argument controlling how the output schema
  is obtained, generalising the local-validation path added in #128 beyond
  DeepSeek. `"structured"` trusts the provider; `"json"` puts the schema in the
  system prompt and validates every response against `codebook$schema` locally;
  `"auto"` (the default) attempts the structured call and falls back to
  `"json"` when it fails. This matters because ellmer sends
  `response_format = {type: "json_schema", strict: true}` to every
  OpenAI-compatible provider and takes the result on trust, and measurement
  shows several do not honour it — Kimi violated a schema it was given on 2 of
  3 identical requests through one gateway. Non-conformance arrives as `NA`,
  indistinguishable from missing data, so `qlm_code()` now also emits a
  one-time note when coding against an endpoint whose enforcement it cannot
  verify, silenced with `options(quallmer.quiet_schema_note = TRUE)`. Whether
  an endpoint is trusted is derived from ellmer's own request path rather than
  a list of vendors, so a provider added to ellmer later defaults to
  unverified. Failure is detected both from an error and from a result in which
  every required field is `NA` in every row, which is what an endpoint that
  accepted the schema and ignored it produces. That check reads required
  scalar properties, since required arrays and nested objects become
  list-columns in which a missing value and a schema-valid empty one are
  indistinguishable -- so for a codebook whose required properties are all
  arrays or nested objects, `"auto"` on an unverified endpoint validates
  locally from the start rather than making a call it could not check, and
  reports why (#134).

* `qlm_code()` now rejects `convert = FALSE` with an explanation. It has never
  worked: `ellmer` returns a bare list, which has no rows to carry an `.id` and
  no columns to reorder, and the call failed later with `incorrect number of
  dimensions` (#134).

* `qlm_code()` can now code with DeepSeek, and no longer trusts providers that
  accept a JSON Schema without enforcing it. The DeepSeek API rejects the
  `response_format` that `ellmer::parallel_chat_structured()` sends
  ("This response_format type is unavailable now"), so every request failed;
  and its JSON mode guarantees JSON syntax, not schema conformance, so simply
  switching to JSON mode would trade a loud failure for a silent one. ellmer
  converts every non-conformance -- wrong field type, missing required field,
  out-of-range enum value -- to `NA` without warning, so a run could come back
  looking plausible but wrong. `qlm_code()` now routes `model = "deepseek/..."`
  to a handler that requests JSON mode, puts the codebook schema in the system
  prompt, validates each response locally against `codebook$schema`, and
  re-prompts the model with the specific validation error
  (`$.claims[2].salience must be a number`) when a response does not conform.
  Repair attempts default to 2 and are configurable with `max_retries`. Units
  that never validate have `NA` coded values and a `.error` list-column
  recording why, and token and cost accounting sums across repair attempts. A
  document the provider rejects as too long is not re-sent, but a content
  refusal is: refusals are not deterministic -- the same document is refused on
  one pass and coded on the next, at more than one provider -- and are rejected
  before generation, so a further attempt is free. No other provider's
  behaviour changes (#128).

* `qlm_code()` no longer retries a request the provider has rejected outright,
  and reports why it was rejected. A wrong model name previously produced three
  rounds of retries and four warnings, none of which mentioned the model; it
  now aborts once, quoting the provider's own message (which for DeepSeek lists
  the valid model names). Failures with a fatal HTTP status (400, 401, 403,
  404, 422) are not retried, unlike rate limits and server errors, and the
  provider's error body is read directly off the response, since httr2 will
  only parse a body served as JSON and several providers do not label theirs
  correctly. A run in which every request is rejected as malformed or
  unauthorised stops after the first pass rather than retrying; a refusal or an
  over-long document does not count towards that, so a single failing unit is
  still retried (#128).

* `qlm_replicate()` now reproduces the settings it claims to. It restored only
  the execution arguments, silently dropping everything the original run passed
  to `ellmer::chat()` -- `params`, `api_args`, `base_url`, `credentials` -- so a
  replication could run at a different temperature than the run it replicated,
  and the resulting `qlm_compare()` would read as a model-stability measurement
  when it was partly a settings-difference measurement. Chat arguments are now
  restored alongside execution arguments, with overrides in `...` taking
  precedence. Provider-specific arguments such as credentials and endpoint
  settings are restored only when the provider is unchanged; when changing
  endpoint, an informational message names any inherited arguments that were
  omitted and not explicitly replaced. An endpoint is identified by both the
  provider prefix and `base_url`, because every provider ellmer has no
  `chat_*()` for is reached as `openai_compatible/<model>` -- so Qwen through
  Alibaba Model Studio and Kimi through Moonshot share a prefix while being
  different services with different credentials, and a prefix-only check would
  send one vendor's credential to the other. The model is still passed as
  `model`, and registered `tools` are never carried over (#125).

* `qlm_replicate()` also reproduces the coding path and `max_retries` of the
  original run. The path is derived from the backend the run actually used
  rather than the mode it requested, so a run that asked for
  `structured = "auto"` and fell back to JSON mode replicates as `"json"`:
  requesting `"auto"` again would let an intermittently conforming endpoint
  take the structured path instead, silently skipping the local validation the
  original relied on and leaving the two runs incomparable (#128, #134).

* `qlm_trail()` no longer emits "unknown column" warnings or crashes with
  `the condition has length > 1` when passed a `qlm_comparison` or
  `qlm_validation` object. The trail now stores these (and `qlm_coded`)
  objects as-is rather than copying selected fields into a parallel
  structure, so they round-trip with their class and metadata intact and
  can be extracted from the trail for replication without modification
  (#93).

* `qlm_validate(..., average = "none")` was reporting per-class
  precision and recall swapped: the helper that derived FP and FN
  from the confusion matrix had its row and column sums transposed
  relative to the orientation produced by `yardstick::conf_mat()`.
  Macro-averaged precision/recall (computed via `yardstick` directly)
  were correct; only the per-class breakdown was affected.

## New features

* New `qlm_failures()` lists the units a coding run failed on, with the
  reason for each, and `print()` of a `qlm_coded` object now reports
  `Units: 251 (211 scored, 40 failed)` rather than the number attempted, so a
  partly failed run cannot look complete. The object already carried this in
  its `.error` column, but nothing surfaced it, and the check people write
  for themselves is wrong for array-valued properties: a failed request
  leaves a zero-row tibble in the list-column, not `NA`, so `!is.na()`
  reports every failed unit as coded. A unit counts as failed when it carries
  an `.error` or when every required scalar property is `NA`, the latter
  because an endpoint can accept a JSON schema and ignore it, returning
  HTTP 200 and nothing usable. Arrays and nested objects are not consulted,
  since after conversion a missing array and a valid empty one are the same
  cell. For that to be enough, `qlm_code()` now also records an `.error` for
  a response ellmer could extract no structured data from (a refusal in
  prose, say): ellmer reports those only by warning and leaves the row with
  no `.error`, which for an array-only schema is indistinguishable from a
  valid empty answer. `print()` also distinguishes rows present from units
  attempted after subsetting (#132).

* `qlm_compare()` gains a `by_category = FALSE` argument that, when
  set to `TRUE`, reports per-category reliability rows for nominal
  data: Krippendorff's alpha (`alpha_per_value[k]`, each category
  dichotomised against all others), kappa (`kappa_per_value[k]`,
  Cohen's κ via dichotomise-and-recompute for two raters or Fleiss'
  Eqs. 20-21 for three or more), and `alpha_u_per_value[k]` for
  unitizing comparisons. The marginal count `n` is reported in the
  `docid` column. Per-category rows are only produced for
  nominal-level data (#112).

## Internal changes

* All reliability statistics are now native R implementations, derived
  directly from their source papers; the package no longer depends on
  `irr`. Each function returns a uniform list shape (`method`, `value`,
  `ci_lower`/`ci_upper`, `per_value`, `n_observers`, `n_units`,
  `n_pairable`) plus measure-specific fields (#112):
  - `reliability_alpha()` — Krippendorff (2019, Ch. 12) for predefined
    units; nominal/ordinal/interval/ratio metrics; per-category α for
    nominal data; verified against book worked examples §12.3.1,
    §12.3.4.1, §12.3.4.4.
  - `reliability_alpha_u()` — Krippendorff's α for unitizing
    continua; one call returns all variants (`value` for `_u_α`,
    `binary` for `|_u_α`, `cu_nominal` for `_cu_α`, plus `per_value`).
  - `reliability_kappa()` — Cohen (1960) with unweighted, linear, and
    quadratic weighted variants; analytic SE/CI for unweighted;
    per-category κ via dichotomisation.
  - `reliability_kappa_fleiss()` — Fleiss (1971) for many raters with
    analytic SE and per-category κⱼ.
  - `reliability_kendall_w()` — Kendall & Smith (1939) with automatic
    tie correction; verified against Kendall & Gibbons (1990) Ch. 6.
  - `reliability_icc()` — all six ICC forms (Shrout & Fleiss 1979;
    McGraw & Wong 1996); verified against Shrout & Fleiss Table 4.

* `qlm_compare()` standardises on `subjects × raters` matrix input
  internally, removing the transpose step previously needed for
  `irr::kripp.alpha`.

* `qlm_validate()` no longer relies on `yardstick`. Accuracy, MAE,
  RMSE, and the confusion matrix are computed inline from base R;
  multi-class precision, recall, and F-measure are now provided by
  internal `metric_precision()`, `metric_recall()`, and
  `metric_f_meas()` supporting all four standard estimators
  (`binary`, `macro`, `macro_weighted`, `micro`). Confusion matrix,
  micro and macro precision/recall follow Sokolova & Lapalme (2009),
  Tables 1-3; macro F-measure is the arithmetic mean of per-class
  F-scores (Manning, Raghavan & Schütze 2008, ch. 13), matching the
  yardstick / scikit-learn convention. Output verified identical to
  `yardstick`'s on both the binary case and a 4-class noisy
  multi-class example. `yardstick` removed from `Imports`.

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
