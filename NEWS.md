# quallmer 0.4.0.9000 (development version)

Everything in this section postdates quallmer 0.4.0, released on CRAN on
2026-05-06.

## Breaking changes

* `qlm_code()` now validates every structured response against the codebook
  schema before ellmer converts it to a row, on every provider and both
  paths. A response that does not conform, whether a required field sent
  as null or left out, a number sent as a string, a value outside its enum,
  an array sent as an object, or an undeclared property, is now a failed
  unit: its row is `NA`, its `.error` names the offending JSON path, and
  `qlm_failures()` lists it for `qlm_backfill()` to re-code. Previously
  such a response was converted regardless, so a missing scalar became a
  silent `NA` with no `.error`, an extra property was dropped, and a
  missing required array became the same empty cell as a valid empty one;
  the "every required field is `NA`" heuristic caught only the wholesale
  case, and only for scalar fields. The whole-run fallback of
  `structured = "auto"` to JSON mode is now driven by the validator: it
  fires when every response the provider completed fails validation, and
  not when some do; where no fallback is possible, under
  `structured = "structured"`, `batch = TRUE` or a file input, such a run
  is returned with every unit failed and its reason recorded, rather than
  stopped. The fallback re-codes only the units JSON mode can help: a
  response cut off at the output limit, or an input rejected as longer
  than the context window, keeps the row, reason and usage the structured
  attempt gave it. The usage of a structured attempt the run fell back
  from is carried into the JSON-mode result, and the usage of a request
  whose outcome is unknown, a timeout or a server error, is recorded as
  `NA` rather than zero, so a total that includes one is unknown rather
  than understated; zero is recorded only for a request never sent or
  refused before generation. The finish reason is read
  from every structured response, so a response cut off at `max_tokens`,
  withheld by a content filter or finished for an unrecognised reason is
  recorded with that reason whether or not it parses, on any provider and
  without a declared limit; the inference from token counts, and the
  `params(max_tokens = )` it needed, are gone. A codebook whose schema
  has a `type_array()` root or an opaque type such as
  `type_from_schema()` is refused before a request is sent, since neither
  can be validated or tabulated. The enforcement note for unverified
  endpoints, and the `quallmer.quiet_schema_note` option that silenced
  it, are gone: there is nothing to warn about now that every response is
  checked. A run validated this way records `validation = "local"` in its
  metadata (#140).

* The `.id` column of a `qlm_coded` object must now be a key: unique and
  never missing. Every later operation merges on `.id`, and `qlm_compare()`
  and `qlm_validate()` silently formed a Cartesian product of repeated
  rows, computing their statistics over the wrong pairs, while a missing
  identifier was matched to every other missing one; an unnested table that
  keeps the document identifier rather than a document-item key is how the
  first arises in practice. Both are now errors, naming the offending
  values: at construction, in `qlm_code()` before a request is spent, when
  rows are subset with `[`, and by every function that takes a `qlm_coded`
  object, which now checks its integrity first: class, run metadata, and
  exactly one `.id` column that is a key. That last check is
  what catches objects that dplyr or base row operations have altered
  (`slice(x, c(1, 1))`, `rbind(x, x)`), which keep the class and
  attributes, and objects saved before the check existed. `as_qlm_coded()` also
  refuses an `id` column alongside an existing `.id`, which left two
  columns of that name with the wrong one read (#156).

* `qlm_code()` now rejects `convert = FALSE` with an explanation. It has never
  worked: `ellmer` returns a bare list, which has no rows to carry an `.id` and
  no columns to reorder, and the call failed later with `incorrect number of
  dimensions` (#134).

* `qlm_code()` now runs with `on_error = "continue"` by default, on the
  structured path as it already did on the JSON path, and gains `on_error`
  as a documented argument taking ellmer's three values. A parallel run
  therefore attempts every unit and leaves the failures for `qlm_failures()`
  and `qlm_backfill()`, rather than stopping at the first and returning the
  rest as `NA` rows with no `.error`, which for a codebook of arrays or
  nested objects could not be told from valid empty answers. Stopping early
  saved money when the only remedy was to run again from the start; with
  backfilling it costs more than it saves. The setting is recorded with the
  run, so a run coded before this change records none and is replicated and
  backfilled with the new default. `on_error` cannot be set with
  `batch = TRUE`, which has no equivalent, and `qlm_replicate()` no longer
  carries a parallel-only or batch-only argument into a replication that
  switches path (#171).

## New features

* `qlm_code()` and `qlm_segment()` accept registered OpenAI-compatible
  provider prefixes. `qlm_register_provider()` adds session-specific endpoints;
  replication and backfill retain the recorded endpoint (#145).

* `qlm_codebook()` accepts `input_type = "audio"`, and `qlm_code()` codes
  recordings in one pass: each file is uploaded to the provider through
  ellmer's file upload and the model receives a reference to it with the
  codebook, so the schema can ask for a transcript alongside any coding of
  the content. Which providers accept audio is not checked in advance: a
  provider that cannot take it refuses with its own message, to which
  `qlm_code()` adds what is known, as of this version that only Google
  Gemini does. Every upload completes before the first request is sent, so
  a failed upload stops the run with the provider's message and nothing
  spent. The run records the SHA-256 of each file, which `qlm_replicate()`
  and `qlm_backfill()` check before uploading again and `qlm_trail()`
  reports. `batch = TRUE` is refused for audio, since an upload gets a new
  reference every time and ellmer's prompt-keyed batch cache could not
  resume the job, and the cost note says that an audio cost computed at the
  text rate is potentially underestimated. Requires ellmer 0.5.0 (#124).

* `qlm_codebook()` accepts `input_type = "video"`, and `qlm_code()` codes
  picture and sound in one pass. Each element of `x` may be the path of a
  local video file, which is uploaded; a YouTube link, which the provider
  fetches itself; or the URL of a video file, which is downloaded and then
  uploaded like a file. Before uploading, `qlm_code()` says how much video
  it is about to send, with a token estimate when the av package is
  installed, and refuses a file over the upload's 2 GB limit. The run
  records the size and SHA-256 of every file, a downloaded URL included,
  and the URL alone for a YouTube link; `qlm_replicate()` and
  `qlm_backfill()` download and upload again after checking the hashes. As
  of this version only Google Gemini accepts video; any other provider is
  tried and its refusal reported with that note (#179).

* A file input (image or audio codebook) no longer falls back to JSON mode
  under `structured = "auto"`: that handler sends text, so a failed
  structured call used to end in the misleading error that the model
  "supports text codebooks only". The provider's own error is now reported,
  and `structured = "json"` is refused up front for a file input. `qlm_code()`
  also checks that every image, audio or video file exists before building a
  request (#124).

* `qlm_codebook()` gains `image_file_resize`, which sets how an image
  codebook's files are resized before they are sent: `"high"` (the new
  default, fitting within 2000x768 or 768x2000 pixels), `"low"` (512x512),
  `"none"`, or a magick geometry string such as `"1024x1024>"`. Until now
  every image was sent at ellmer's default of 512x512, with nothing in
  quallmer saying so, which is a thumbnail for a poster whose small print
  is what the codebook asks about. The setting lives on the codebook
  because the resolution is part of the measurement: `qlm_replicate()` and
  `qlm_backfill()` inherit it, `qlm_trail()` reports it, and a codebook
  saved before the field existed is read as `"low"`, what it was coded at,
  so replicating an old run still measures what the original measured.
  magick, which the resizing needs, is now in Suggests, and `qlm_code()`
  says so if it is missing. `x` may now also hold image URLs alongside
  paths: an element beginning with `http://`, `https://` or `data:` is
  passed to the provider as it is, and every path is checked to exist
  before any request is sent. A second codebook field, `image_url_detail`,
  asks the provider for `"low"` or `"high"` detail on such URLs; OpenAI and
  OpenAI-compatible providers read it, and ellmer forwards it from the
  version that includes tidyverse/ellmer#1133, so `qlm_code()` says before
  the run when a value cannot take effect (#177).

* New `qlm_backfill()` re-codes only the units a run failed on and merges the
  results into the original object, instead of re-running the whole corpus
  to recover a handful of transient failures. Which units to retry is
  decided afresh on each pass by the test `qlm_failures()` uses, so a
  request rejected on length drops out as soon as a pass records it, and a
  response cut off at `max_tokens` is retried only when the backfill raises
  the limit through `params`; content refusals are retried, because they are
  not deterministic. A pass that recovers nothing ends the backfill early.
  Passes run with the run's own model and settings by default, on the path
  the run actually took. A different `model` may be given, for units the
  original model consistently refuses or cannot fit in its context window,
  and the result then records which units came from which model and says so
  when printed, since a coding by two instruments has to be disclosed as
  one. Rows keep their order, a failed retry never overwrites anything, and
  token and cost columns are summed across attempts, a total staying `NA`
  when any attempt's figure is unknown, as it becomes for the units a pass
  that failed outright attempted, since the pass may have been billed. A
  pass costed on other rates than the run records them, and `print()` and
  `qlm_trail()` say so beside the run's own cost note. `qlm_code()` gains
  `backfill` to complete a run in the same call, and `qlm_replicate()`
  gains `backfill`, which by default replays the passes recorded on the
  parent so that a replication of a completed run is complete on the same
  terms. `qlm_trail()` reports the passes and any other model they used, so
  a composite is disclosed in the audit trail as well as when printed (#136).

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
  Repair attempts default to 2 and are configurable with `json_retries`. Units
  that never validate have `NA` coded values and a `.error` list-column
  recording why, and token and cost accounting sums across repair attempts. A
  document the provider rejects as too long is not re-sent, but a content
  refusal is: refusals are not deterministic -- the same document is refused on
  one pass and coded on the next, at more than one provider -- and are rejected
  before generation, so a further attempt is free. No other provider's
  behaviour changes (#128).

* `qlm_code()` gains `prices`, rates in US dollars per million tokens that
  cost a run ellmer cannot price, from the token counts it records. Only the
  rows ellmer leaves `NA` are filled, by the sum ellmer applies to its own
  table; where ellmer prices the model its figure stands. The rates are kept
  in the run's metadata, shown by `print()` and the trail report, and reused
  by `qlm_replicate()` when the model, endpoint, batch setting and service
  tier are unchanged, so a cost resting on entered figures is always
  labelled as such. quallmer bundles no prices of its own
  (#135).

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

* `qlm_code()` now says when a model name is not one its provider lists,
  with the nearest names it does have, instead of reporting only the
  provider's HTTP error. The provider's model list is fetched through
  ellmer's `models_<provider>()`, only after a run has been rejected in its
  entirety and at most once per failed run, and only for providers whose
  listing covers every name they accept; where it cannot decide, the
  provider's own error is reported unchanged (#133).

* `qlm_code()` now says why `cost` will be `NA` when `include_cost = TRUE`
  cannot be honoured, once and before the run, and keeps the reason with the
  object so `print()` shows it. ellmer prices from a table fixed at its
  release, matched exactly on provider and model, and answers `NA` on any
  miss without saying which kind: DeepSeek and six other providers are absent
  from the table altogether, so no model of theirs is ever priced; a model
  newer than the installed ellmer is missed on a provider it otherwise
  prices; and local endpoints have no per-token charge. Each is now named,
  since the remedies differ, and the message says whether the token counts a
  cost could be worked out from are being recorded, which needs
  `include_tokens = TRUE` (#135).

## Documentation

* The "Audio transcription and analysis" example article now shows both
  routes: transcription with Whisper followed by coding of the transcripts,
  and coding the recordings directly with `input_type = "audio"` on
  `gemini-2.5-flash`, ending with `qlm_compare()` of the two runs, the
  model's transcript beside Whisper's, the cost, and the recorded file
  hashes (#124).

## Bug fixes

### Coding runs

* `qlm_segment()` now records token counts and cost when asked, and takes
  `prices` as `qlm_code()` does. It forwarded `include_tokens` and
  `include_cost` to ellmer, but ellmer attaches usage only to a converted
  result that is a data frame, and converts the array a segmentation asks
  for to a plain list, so the counts were lost before quallmer saw them. The
  array is now requested inside an object, which converts to one row per
  source document with the usage beside it. Usage belongs to the document,
  not the segment: it is kept in the corpus metadata as `usage`, one row per
  input document including those that yielded no segments, and repeated on
  each segment as docvars; sum the metadata table for the run's total. The
  four usage names are reserved when usage is requested (#119).

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

* The deprecated `annotate()` and `trail_record()` run again. `annotate()`
  passed its `model_name` to `qlm_code()` under that name, whose argument is
  `model`, so the value fell through to the provider call and every use
  failed, with `model` reported missing or `model_name` reported unused.
  `annotate()` again returns its identifier column as `id`, as documented,
  rather than `qlm_code()`'s `.id`, and `trail_record()` now always restores
  the caller's `id_col` values in place of the sequential ones `annotate()`
  generates. Both functions still warn that `qlm_code()` replaces them
  (#141).

* Printing a `qlm_coded` object, or any other quallmer object built on a
  tibble, in a session that had not yet loaded tibble fell through to
  `print.data.frame()`, since tibble was in Imports but not in the
  NAMESPACE and so loaded only on the first namespaced call. For a run with
  an `.error` column that was an error rather than plain output, since a
  recorded condition has no `format()` method. tibble now loads with
  quallmer (#173).

### Reliability and validation

* `qlm_compare()` and `qlm_validate()` ranked ordinal text categories
  alphabetically, so a scale of low / medium / high was ranked
  high < low < medium, and every statistic that uses the order (ordinal
  alpha, weighted kappa, Kendall's W, Spearman's rho, tau and MAE) was
  computed on the wrong ranks with nothing in the output to show it; factor
  levels were flattened to text before they were read, so ordering the
  categories in advance changed nothing. Text categories at ordinal level
  are now ranked by the levels of an ordered factor, which every coder must
  supply and all must agree on, and a plain factor or character column is
  an error that says how to declare the order, since alphabetical order is
  not a ranking and a plain factor's levels are alphabetical by default. A
  `type_enum()` declared `"ordinal"` in `qlm_codebook()` is that
  declaration: its values, in the order written, are the scale, `qlm_code()`
  stores the column as an ordered factor with those levels, `as_qlm_coded()`
  does the same for human-coded data given the codebook (refusing a value
  outside the enum), and the codebook print shows the order. An enum not
  declared ordinal stays nominal. Because the ranks are now numbers, a
  `tolerance` on ordered text counts rank distance, so `tolerance = 1`
  means adjacent categories agree; the warning that a tolerance on text
  categories was being ignored is gone with the reason for it (#165).

* `qlm_compare()` now honours `tolerance`, and computes its numeric
  statistics on the ratings' values, when a coder stores the ratings as
  text. The ratings were assembled into one matrix before their type was
  examined, and a single character column made the whole matrix character:
  every unit then fell through to exact text equality with `tolerance`
  unused, and Krippendorff's alpha, the ICC and Pearson's r ran on factor
  codes of the sorted strings, where `"10"` falls between `"1"` and `"2"`.
  Neither showed in the output, so an LLM column parsed as text against a
  numeric human column gave a flat agreement curve and a wrong alpha. At
  ordinal, interval and ratio level every column is now read as numbers, as
  the declared level asserts; a value that does not read as a number is an
  error naming the coder and the values; ordinal categories given as text
  are ranked as before, but a positive `tolerance` on them draws a warning;
  and nominal categories agree only when identical, as the code's own
  comment already claimed (#150).

* `qlm_validate()` ranked ordinal ratings by their values sorted as strings,
  so on a 1 to 10 scale `"10"` fell between `"1"` and `"2"` and Spearman's
  rho, Kendall's tau and MAE were wrong whenever a rating reached 10, even
  from numeric input. Ordinal and interval values are now read as numbers
  the same way as in `qlm_compare()` (#150).

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

* `qlm_validate(..., average = "none")` was reporting per-class
  precision and recall swapped: the helper that derived FP and FN
  from the confusion matrix had its row and column sums transposed
  relative to the orientation produced by `yardstick::conf_mat()`.
  Macro-averaged precision/recall (computed via `yardstick` directly)
  were correct; only the per-class breakdown was affected.

### Audit trail and replication

* Backfill treats an endpoint change as a new coding route even when the model
  name is unchanged. Replication preserves the requested model and labels
  endpoint overrides in print and trail output (#185).

* `qlm_trail()` no longer writes credentials into the trail. An `api_key`,
  a credential-named `api_headers` entry, or a `base_url` carrying userinfo
  or a credential-named query parameter, given to `qlm_code()` as a literal,
  previously appeared verbatim in the report's Call section and in the saved
  `.rds`. Their values are now replaced by `"<redacted>"` in each run's
  recorded call and chat arguments, in the returned trail and in both files,
  and a message says which runs were affected. A `credentials` callback is
  kept only as `function() Sys.getenv("NAME")`, rebuilt without its
  environment; any other callback is redacted too. Reading the key where it
  is needed, through an environment variable or that callback form, keeps
  it out of the record entirely and is the recommended form (#154).

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

* `qlm_replicate()` also reproduces the coding path and `json_retries` of the
  original run. The path is derived from the backend the run actually used
  rather than the mode it requested, so a run that asked for
  `structured = "auto"` and fell back to JSON mode replicates as `"json"`:
  requesting `"auto"` again would let an intermittently conforming endpoint
  take the structured path instead, silently skipping the local validation the
  original relied on and leaving the two runs incomparable (#128, #134).

* `qlm_replicate()` no longer carries the coding path across a change of
  endpoint, provider or `base_url`. It reproduced the path the parent took,
  `structured` or `json`, which is right for the same endpoint and wrong for
  another: a JSON-mode DeepSeek run replicated on OpenAI would skip
  provider-side enforcement, a structured OpenAI run replicated on DeepSeek
  would fail outright, and two OpenAI-compatible services behind the same
  prefix enforce a schema quite differently. With a new endpoint the path is
  now chosen as for a fresh run; `json_retries` still carries, as before.

* `qlm_trail()` no longer emits "unknown column" warnings or crashes with
  `the condition has length > 1` when passed a `qlm_comparison` or
  `qlm_validation` object. The trail now stores these (and `qlm_coded`)
  objects as-is rather than copying selected fields into a parallel
  structure, so they round-trip with their class and metadata intact and
  can be extracted from the trail for replication without modification
  (#93).

## Documentation

* `?qlm_code` gains an "Incomplete runs" section: what a failed unit looks
  like in the object, the layers at which it can be tried again, what a
  backfill leaves alone and why, with a pointer to the workflow guide's
  section and its failure-to-mechanism table (#174).

* The package now ships a coded run that came back incomplete and its
  backfilled counterpart, in `inst/extdata/example_objects.rds`, coded once
  with a live model and a deliberately short timeout and low `max_tokens`.
  The workflow guide's "When a run comes back incomplete" section, the audit
  trail tutorial, and the examples of `qlm_failures()` and `qlm_backfill()`
  now show `qlm_failures()`, `print()` and the trail's `Backfill:` line on
  those objects, evaluated, where before they described the output in prose
  or sat in `\dontrun{}` (#173).

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
