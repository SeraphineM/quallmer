#' Code qualitative data with an LLM
#'
#' Applies a codebook to input data using a large language model, returning
#' a rich object that includes the codebook, execution settings, results, and
#' metadata for reproducibility.
#'
#' Arguments in `...` are dynamically routed to either [ellmer::chat()],
#' [ellmer::parallel_chat_structured()], or [ellmer::batch_chat_structured()]
#' based on their names.
#'
#' @param x character; the input data: texts for a text codebook, file
#'   paths or URLs for an image codebook (see the section on image input), or
#'   file paths for an audio codebook (see the section on audio input), or
#'   file paths, YouTube links and URLs of video files for a video codebook
#'   (see the section on video input).
#'   Named vectors will use names
#'   as identifiers in the output; unnamed vectors will use sequential integers.
#'   The identifiers become the `.id` column, on which every later operation
#'   keys, so names must be unique.
#' @param codebook qlm_codebook; a codebook created with [qlm_codebook()]. Also accepts
#'   deprecated [task()] objects for backward compatibility.
#' @param model character; the provider (and optionally model) name in the form
#'   `"provider/model"` or `"provider"` (which will use the default model for
#'   that provider). Native prefixes are passed to [ellmer::chat()].
#'   Registered prefixes, such as `"dashscope/qwen-plus"`, resolve through
#'   [qlm_register_provider()] and require an explicit model name.
#'   Examples: `"openai/gpt-4o-mini"`, `"anthropic/claude-3-5-sonnet-20241022"`,
#'   `"ollama/llama3.2"`, `"openai"` (uses default OpenAI model).
#' @param structured character; how the output schema is obtained.
#'   `"structured"` sends it through the provider's structured-output
#'   mechanism. `"json"` asks for JSON, puts the schema in the system prompt,
#'   and re-prompts a unit whose response does not conform. `"auto"` (the
#'   default) attempts the structured call and falls back to `"json"` if it
#'   fails, or if no response it completed matched the schema. On either
#'   path every response is validated against the codebook locally before it
#'   is tabulated. See Details for which to use.
#' @param json_retries Integer; the number of additional requests \pkg{quallmer}
#'   may make for a unit on the JSON path after an unusable response. Default
#'   is 2, giving at most three JSON-path requests per unit. This is implemented
#'   by \pkg{quallmer} and is not passed to \pkg{ellmer}. Each request separately uses
#'   \pkg{ellmer}'s transport retry policy, controlled by
#'   `options(ellmer_max_tries = )`. Applies on the JSON path only, so setting
#'   it alongside `structured = "structured"` is an error. What is still
#'   unusable after the run is left for `backfill`.
#' @param on_error character; what a failed request does to the rest of a
#'   parallel call, passed to [ellmer::parallel_chat_structured()] or, on the
#'   JSON path, [ellmer::parallel_chat()]. `"continue"` (the default) attempts
#'   every unit and records each failure in the `.error` column, for
#'   [qlm_failures()] to list and [qlm_backfill()] to re-code. `"return"`
#'   stops submitting new requests after the first failure, waits for those in
#'   flight, and returns what the call has. On the structured path that call
#'   is the run: the units never sent are recorded in `.error` as not
#'   completed, for [qlm_backfill()] to send. On
#'   the JSON path the call is one wave: a unit the wave did not reach counts
#'   as unanswered, so `json_retries` sends it again in a later wave, each
#'   stopped in turn at its first failure, and whatever is still unsent at the
#'   end is recorded in `.error`; `json_retries = 0` stops after the first
#'   wave. `"stop"` raises the first failure as an error. Applies to parallel
#'   runs only: the batch API has no equivalent, so it cannot be set with
#'   `batch = TRUE`.
#' @param backfill Logical, integer or `NULL`; whether to complete the run
#'   before it is returned, by re-coding the units still failed with
#'   [qlm_backfill()], using the same model and settings. `FALSE` or `0`
#'   (default) leaves the run as it came back; `TRUE` makes the default
#'   number of passes, currently two; a positive integer makes at most that
#'   many; `NULL` means `FALSE` here, since a fresh run has no parent whose
#'   passes could be replayed, which is what `NULL` asks [qlm_replicate()]
#'   for. Each pass is recorded in the object's metadata, and a pass that
#'   recovers nothing ends the backfill early. See [qlm_backfill()] for what
#'   is retried and what is left alone.
#' @param prices Optional. Rates for costing the run when ellmer cannot: a
#'   named numeric vector or list with `input` and `output`, and optionally
#'   `cached_input`, in US dollars per million tokens, for example
#'   `c(input = 0.435, output = 0.87, cached_input = 0.0036)`. Where ellmer
#'   prices the model itself its figure stands and these are not used. A
#'   `cached_input` rate that is not given is taken as the `input` rate. See
#'   the section on cost. Default is `NULL`.
#' @param batch logical; if `TRUE`, uses [ellmer::batch_chat_structured()]
#'   instead of [ellmer::parallel_chat_structured()]. Batch processing is more
#'   cost-effective for large jobs but may have longer turnaround times.
#'   Default is `FALSE`. See [ellmer::batch_chat_structured()] for details.
#' @param ... Additional arguments passed to [ellmer::chat()],
#'   [ellmer::parallel_chat_structured()], or [ellmer::batch_chat_structured()].
#'   Arguments recognized by [ellmer::parallel_chat_structured()] or
#'   [ellmer::batch_chat_structured()] are routed there; all other arguments
#'   (including provider-specific arguments like `base_url`, `credentials`, or
#'   `api_args` for OpenAI-compatible endpoints) are passed to [ellmer::chat()].
#' @param name character or `NULL`; a name identifying this coding run. Default is `NULL`.
#' @param notes character or `NULL`; descriptive notes about this
#'   coding run. Useful for documenting the purpose or rationale when viewing
#'   results in [qlm_trail()]. Default is `NULL`.
#'
#' @details
#' Progress indicators and error handling are provided by the underlying
#' [ellmer::parallel_chat_structured()] or [ellmer::batch_chat_structured()]
#' function. Set `verbose = TRUE` to see progress messages during coding.
#' Retry logic for API failures should be configured through ellmer's options;
#' what a failure does to the rest of a parallel run is `on_error`.
#'
#' @section Image input:
#'
#' An image codebook codes one image per element of `x`. A file path is read
#' and sent inline, after being resized as the codebook's `image_file_resize`
#' says: `"high"` by default, which fits the image within 2000x768 or 768x2000
#' pixels, `"low"` for 512x512, `"none"` to send the file as it is, or a
#' magick geometry string. Anything but `"none"` needs the \pkg{magick}
#' package, which is checked here before any request is sent. The resolution
#' is part of the codebook because it is part of the measurement: a poster
#' whose small print is legible at one size is not at another, and a
#' replication should read the image the original run read. Codebooks saved
#' before the setting existed are read as `"low"`, which is what they were
#' coded at. See [qlm_codebook()].
#'
#' A URL is passed to the provider as it is, through
#' [ellmer::content_image_url()], so `image_file_resize` does not apply to
#' it; what the provider does with a remote image is its own affair, and not
#' every provider fetches URLs. The codebook's `image_url_detail` asks the
#' provider for `"low"` or `"high"` detail on such an image, where the
#' provider reads that field: OpenAI and OpenAI-compatible providers do,
#' others ignore it, and ellmer forwards it only from the version that
#' includes <https://github.com/tidyverse/ellmer/pull/1133>. When a value
#' other than `"auto"` cannot take effect, `qlm_code()` says so before the
#' run rather than recording a setting that was not applied. A path that
#' does not exist is refused before anything is sent, so a URL typed without
#' its scheme fails here with the path named, not inside the request.
#'
#' @section Provider-specific parameters:
#'
#' `params` and `api_args` are forwarded to [ellmer::chat()] unchanged.
#' quallmer does not inspect or rewrite either, so which of the two a setting
#' belongs in is determined by ellmer and the provider, not here.
#'
#' The distinction matters. [ellmer::params()] carries provider-agnostic
#' settings that ellmer translates per provider; `api_args` goes into the raw
#' request body untouched. A setting placed in the wrong one is not
#' necessarily rejected. For OpenAI-compatible providers ellmer maps `top_k`
#' onto the OpenAI field `top_logprobs`, which asks for log-probabilities per
#' token and has nothing to do with top-k sampling — so
#' `params(top_k = 20)` is rejected by Alibaba Model Studio
#' (`Range of top_logprobs should be [0, 5]`), while `params(top_k = 3)` is
#' accepted and silently applies no sampling setting at all. Non-OpenAI
#' sampling controls therefore belong in `api_args`:
#'
#' ```r
#' # Qwen through Alibaba Model Studio
#' qlm_code(
#'   x, codebook,
#'   model = "openai_compatible/qwen3-max",
#'   base_url = "https://dashscope-intl.aliyuncs.com/compatible-mode/v1",
#'   credentials = function() {
#'     list(Authorization = paste("Bearer", Sys.getenv("DASHSCOPE_API_KEY")))
#'   },
#'   params = ellmer::params(temperature = 0.6, top_p = 0.95),
#'   api_args = list(top_k = 20, min_p = 0, enable_thinking = TRUE)
#' )
#'
#' # Kimi K3 through Moonshot, whose temperature and top_p are fixed by the
#' # provider and documented as needing to be omitted rather than set
#' qlm_code(
#'   x, codebook,
#'   model = "openai_compatible/kimi-k3",
#'   base_url = "https://api.moonshot.ai/v1",
#'   credentials = function() {
#'     list(Authorization = paste("Bearer", Sys.getenv("MOONSHOT_API_KEY")))
#'   },
#'   api_args = list(reasoning_effort = "max")
#' )
#' ```
#'
#' Passing a model parameter such as `temperature` or `max_tokens` at the top
#' level does not work: those reach [ellmer::chat()], which has no such
#' argument. Use `params`.
#'
#' @section Cost:
#'
#' `include_tokens = TRUE` and `include_cost = TRUE` are forwarded to ellmer,
#' which adds per-unit token counts and a `cost` column in US dollars. ellmer
#' prices from a table fixed at its release, matched exactly on provider and
#' model, and returns `NA` on any miss. Some providers are absent from that
#' table altogether, DeepSeek among them, so no model of theirs is ever priced;
#' a model newer than the installed ellmer is missed on a provider it otherwise
#' prices, which upgrading fixes; and local endpoints such as ollama have no
#' per-token charge. In each case `qlm_code()` says so once before the run,
#' and the reason is kept with the object and shown when it is printed. With
#' `include_tokens = TRUE` the token counts are recorded, from which such a
#' run can be costed at the provider's published rates.
#'
#' `prices` does that costing, at rates you supply from the provider's
#' published price list. Supplying them implies `include_tokens = TRUE` and
#' `include_cost = TRUE`. Only rows ellmer left `NA` are filled, by the same
#' sum ellmer applies to its own table: uncached input tokens at the `input`
#' rate, cache hits at the `cached_input` rate, output at the `output` rate,
#' each per million. Where ellmer priced every row itself the rates are not
#' used, and you are told so. The rates are kept in the run's metadata, shown
#' by `print()` and in the trail report, and reused by [qlm_replicate()] when
#' the model, the endpoint, the batch setting and the service tier are
#' unchanged, so a cost that rests on entered figures is always labelled as
#' such. quallmer bundles no prices of its own.
#'
#' @section Schema enforcement and validation:
#'
#' Some providers accept a JSON Schema without enforcing it, so a response
#' can come back parseable but non-conforming: a number as a string, a
#' required property missing, an extra one added. Providers reached through
#' ellmer's generic OpenAI-compatible request path are all in this position:
#' `strict = TRUE` is sent and may simply be ignored. Converted straight to
#' a table, such a response would arrive silently as `NA`, or as an empty
#' list-column cell that a valid empty answer also produces.
#'
#' So every response is validated against `codebook$schema` before it is
#' converted, on either path and whatever the provider: required properties
#' present and not null, scalars of the declared type without coercion,
#' enum values from the declared set, arrays and nested objects of the
#' declared shape, no undeclared properties unless the schema allows them.
#' A response that fails is a failed unit: its row is `NA`, its `.error`
#' names the offending JSON path (`$.claims[2].score must be a number`), and
#' [qlm_failures()] lists it for [qlm_backfill()] to re-code. The other
#' units keep their coded values. The response's usage is recorded with the
#' failure, since the request was billed.
#'
#' `structured` chooses how the schema reaches the model, and what to do
#' when the provider ignores it:
#'
#' \describe{
#'   \item{`"structured"`}{Send the schema through the provider's
#'     structured-output mechanism. Fails loudly if the call fails; a
#'     response that does not conform is a failed unit.}
#'   \item{`"json"`}{Ask for JSON, put the schema in the system prompt, and
#'     re-prompt with the specific validation error when a response does not
#'     conform, up to `json_retries` times. The reliable choice for an
#'     endpoint known not to enforce.}
#'   \item{`"auto"`}{Attempt the structured call; fall back to `"json"` for
#'     the whole run if it errors, or if every response the provider
#'     completed fails validation, which is what an endpoint that ignored
#'     the schema produces. Requests the provider refused, and responses it
#'     cut off or filtered, are left out of that judgement, since neither
#'     says anything about the schema. The fallback re-codes the units JSON
#'     mode can help: a response cut off at the output limit, or an input
#'     rejected as longer than the context window, fails the same way on
#'     any path, so those units keep the row, reason and usage the
#'     structured attempt gave them. A run in which some responses
#'     conform keeps them, and records the rest as failed units: the
#'     intermittent kind of non-enforcement is caught per unit, not by
#'     re-coding the corpus.}
#' }
#'
#' Where every completed response fails validation and nothing can fall
#' back, under `"structured"`, `batch = TRUE` or a file input, the run is
#' returned with every unit failed and its reason recorded, and a warning
#' says why: the failed rows, their reasons and their usage are what the run
#' has, and [qlm_backfill()] can retry them with another model.
#'
#' On either path, [qlm_failures()] lists the units that produced no usable
#' coding, with the reason for each, and `print()` reports how many there
#' were. Most such failures are transient, and [qlm_backfill()] re-codes
#' just those units and merges them back; `backfill` does that before
#' returning. Batch processing and file inputs (image and audio codebooks)
#' are not supported on the JSON path, so `"auto"` will not fall back under
#' `batch = TRUE` or for a file input: a failed structured call then stops
#' with the provider's own error, and `structured = "json"` is refused up
#' front. The path actually taken is recorded in the run metadata as
#' `backend`, and a run validated this way carries `validation = "local"`.
#'
#' The schema itself must be a `type_object()` at the root, whose properties
#' become the columns of the result, built from `type_string()`,
#' `type_boolean()`, `type_integer()`, `type_number()`, `type_enum()`,
#' `type_array()` and `type_object()`. Any other type is refused before a
#' request is sent, since there would be nothing to validate a response
#' against.
#'
#' @section Audio input:
#'
#' A codebook with `input_type = "audio"` codes recordings in one pass: each
#' file in `x` is uploaded to the provider through ellmer's file upload,
#' and the model receives a reference to it with the codebook's
#' instructions, so the schema can ask for a transcript, a language, a
#' summary or any coding of the content. Accepted formats are mp3, wav, ogg,
#' m4a, flac and aac.
#'
#' Which providers accept audio this way is not checked in advance: the
#' recordings are uploaded and the model is asked, and a provider that
#' cannot take them refuses with its own message, to which `qlm_code()` adds
#' what is known. As of this version only Google Gemini (`google_gemini/`)
#' is known to accept an uploaded recording alongside a schema-constrained
#' request; OpenAI's and Anthropic's endpoints refuse, and Vertex has no
#' file upload. For those providers, transcribe the recordings first and
#' code the transcripts with a text codebook.
#'
#' Every upload completes before the first coding request is sent, so either
#' all the inputs are ready or nothing is spent; a failed upload stops the
#' run with the provider's message, which says whether the failure was
#' transient or the file itself. Uploads expire after 48 hours and storage
#' is free, so [qlm_replicate()] and [qlm_backfill()] upload the files again
#' from the paths in `x`. Before they do, they check the files against the
#' SHA-256 hashes the run recorded for each unit, and refuse to continue if
#' a path now points at different bytes. The hashes are taken before
#' anything is uploaded, so they are of the bytes the model received even
#' if a file is replaced while the requests run; they are kept in the run's
#' metadata as `input_files` and reported by [qlm_trail()]. A backfill pass
#' records its own hashes for the units it re-coded, so a run coded by an
#' earlier version that recorded none gains them unit by unit; units still
#' without one are reported as unverifiable, with a notice, rather than as
#' changed.
#'
#' `batch = TRUE` is not supported for audio: ellmer's batch cache is keyed
#' on the prompts, and an upload gets a new reference every time, so a
#' batch run could not be resumed.
#'
#' With `include_cost = TRUE`, or `prices`, the cost of an audio run is
#' potentially underestimated: providers charge more per audio token than
#' per text token, and the figure is computed at the text rate from the
#' total. The run's cost note says so, in `print()`, the trail, and any
#' backfill pass.
#'
#' @section Video input:
#'
#' A codebook with `input_type = "video"` codes picture and sound in one
#' pass. Each element of `x` is one of: the path of a local video file
#' (`mp4`, `mov`, `avi`, `wmv` or `webm`), which is uploaded to the
#' provider; a YouTube link, which the provider fetches itself, so nothing
#' is uploaded; or the URL of a video file, which is downloaded here and then
#' uploaded like a file, and must end in one of those extensions. The model
#' receives a reference to each with the codebook's instructions, so the
#' schema can ask for a transcript, a description of what is shown, or any
#' coding of the content. A video carries its audio track, so speech and
#' picture are coded together.
#'
#' As with audio, which providers accept video is not checked in advance; a
#' provider that cannot take it refuses with its own message, and
#' `qlm_code()` adds what is known. As of this version only Google Gemini
#' (`google_gemini/`) accepts video, and all its chat models do. Gemini
#' samples one frame a second and, at its default resolution, spends
#' roughly 300 input tokens per second of video, so a ten-minute clip is
#' about 175,000 tokens and a model with a million tokens of context takes
#' about an hour of video per request. Before uploading, `qlm_code()` says
#' how much it is about to send: the total duration and a token estimate
#' when the \pkg{av} package is installed, the total size otherwise. A file
#' over 2 GB, the upload's limit, is refused. Video tokens are charged at
#' the text rate, so a cost from `include_cost` or `prices` needs no
#' qualification here, unlike audio. A YouTube video must be public; the
#' free tier caps YouTube input at eight hours of video a day. Gemini's own
#' video settings (frame rate, clip offsets, media resolution) are not
#' exposed by ellmer, so whole clips are coded at the defaults.
#'
#' Provenance and replication work as for audio. The run records each
#' file's size and SHA-256 hash; for a downloaded URL those are of the
#' downloaded bytes, and for a YouTube link the URL alone is recorded, since
#' nothing passed through this machine. [qlm_replicate()] and
#' [qlm_backfill()] download and upload again, checking the hashes first,
#' and `batch = TRUE` is refused as for audio.
#'
#' @section Rejected runs:
#' When the provider rejects every request with a status that will not change
#' on retry (400, 401, 403, 404 or 422), `qlm_code()` stops rather than
#' returning a table of `NA`s. The most common cause is a model name the
#' provider does not have, and providers rarely say so; many answer with a
#' bare "HTTP 400 Bad Request". So before reporting, `qlm_code()` asks the
#' provider for its model list, through ellmer's `models_<provider>()`, and
#' says when the name is not on it, with the nearest names it does have. The
#' lookup runs only after a run has failed, once per failed run, and only for
#' providers whose listing is known to cover every name they will invoke
#' (Bedrock, for one, invokes inference profiles its listing omits). Where
#' it cannot run, or the name is on the list, the provider's own error is
#' reported unchanged.
#'
#' Under the default `on_error = "continue"` the parallel call does not stop
#' at the first refusal, so every unit is sent once before the run comes
#' back and is diagnosed. Each such request is refused before anything is
#' generated, so it is cheap, but on a large corpus there are many of them,
#' paced by `rpm`. `on_error = "return"` stops the structured call after the
#' first wave, at the cost described under that argument; on the JSON path,
#' whose default has always been `"continue"`, `json_retries` sends the
#' units a wave did not reach in later waves, so `"return"` stops after the
#' first wave there only with `json_retries = 0`.
#'
#' @section Incomplete runs:
#'
#' A run over a corpus of any size rarely comes back complete, and an
#' incomplete run is not an error. Under the default `on_error = "continue"`
#' every unit is attempted, and a unit that produced no usable coding is
#' returned as a row of `NA` with the reason in its `.error` column;
#' `on_error` says what a failure does to the rest of the run.
#' [qlm_failures()] lists the failed units with their reasons, and `print()`
#' counts them. Trying again happens in layers. ellmer retries each request
#' at the transport level, on every path. On the JSON path, `json_retries`
#' sends a unit again during the run when its response was unusable. After
#' the run, `backfill`, or [qlm_backfill()] on the returned object, re-codes
#' what is still failed and merges it back, with the same model and
#' settings. A backfill leaves alone the two failures that re-sending the
#' same request cannot fix, a text rejected as longer than the context
#' window and a response cut off at `max_tokens` (see below); those need a
#' different `model` or a higher `params(max_tokens = )`. The section "When
#' a run comes back incomplete" of the workflow guide
#' (<https://quallmer.github.io/quallmer/articles/pkgdown/getting-started/workflow.html>)
#' walks through this on a run that ships with the package, and its table
#' matches each kind of failure to the mechanism that handles it.
#'
#' @section Truncated responses:
#'
#' A response that runs into the provider's output limit (`max_tokens`) is cut
#' off mid-JSON, and the request is billed in full. The affected units are
#' systematically the longest and richest ones, and for a codebook where an
#' empty answer is a legitimate outcome, a cut-off answer that reads as empty
#' is the worst kind of silent failure.
#'
#' The provider's finish reason travels with every response, on either path,
#' and is read before the response is parsed: a unit cut off this way is
#' recorded in `.error` with the token count, whether or not the fragment
#' happens to parse, is listed by [qlm_failures()], and is not retried: a
#' repair prompt cannot supply what the limit withheld, and would only press
#' the model into a shorter answer. A backfill leaves it alone too; raise
#' `params(max_tokens = )` and backfill again. A response the provider
#' withheld under a content filter, or finished for a reason it did not
#' recognise, is recorded the same way with that reason.
#'
#' When `batch = TRUE`, the function uses [ellmer::batch_chat_structured()]
#' which submits jobs to the provider's batch API. This is typically more
#' cost-effective but has longer turnaround times. The `path` argument specifies
#' where batch results are cached, `wait` controls whether to wait for completion,
#' and `ignore_hash` can force reprocessing of cached results. `on_error` does
#' not apply: the batch API has no equivalent.
#'
#' @section Registered providers:
#' OpenAI-compatible endpoints can also be addressed by a registered prefix,
#' for example `model = "dashscope/qwen-plus"`. See
#' [qlm_register_provider()] for built-in endpoints, credential sources,
#' and adding a private gateway. Native ellmer prefixes take precedence.
#'
#' @return A `qlm_coded` object (a tibble with additional attributes):
#'   \describe{
#'     \item{Data columns}{The coded results with a `.id` column for identifiers.
#'       A [type_enum()] declared `"ordinal"` in the codebook is an ordered
#'       factor whose levels are the enum's values in the order written; see
#'       the `levels` argument of [qlm_codebook()].}
#'     \item{Attributes}{`data`, `input_type`, and `run` (list containing name, batch, call, codebook, chat_args, execution_args, metadata, parent).}
#'   }
#'   The object prints as a tibble and can be used directly in data manipulation workflows.
#'   The `batch` flag in the `run` attribute indicates whether batch processing was used.
#'   The `execution_args` contains all non-chat execution arguments (for either parallel or batch processing).
#'
#' @seealso
#' [qlm_codebook()] for creating codebooks, [qlm_replicate()] for replicating
#' coding runs, [qlm_compare()] and [qlm_validate()] for assessing reliability.
#'
#' @examples
#' # Requires API credentials and internet access; not run in package checks.
#' \dontrun{
#' # Basic sentiment analysis
#' texts <- c("I love this product!", "Terrible experience.", "It's okay.")
#' coded <- qlm_code(texts, data_codebook_sentiment, model = "openai/gpt-4o-mini")
#' coded
#'
#' # With named inputs (names become IDs in output)
#' texts_named <- c(review1 = "Great service!", review2 = "Very disappointing.")
#' coded2 <- qlm_code(texts_named, data_codebook_sentiment, model = "openai/gpt-4o-mini")
#' coded2
#'
#' # Audio recordings, coded in one pass by a Gemini model; see the section
#' # "Audio input" for which providers accept audio. The model hears the
#' # recording, so the schema can ask for the transcript as well as the codes
#' speech_codebook <- qlm_codebook(
#'   "Speech", "Transcribe the recording, identify the language and summarise what is said.",
#'   ellmer::type_object(
#'     transcript = ellmer::type_string("Verbatim transcript, in the language spoken"),
#'     language = ellmer::type_string("Language spoken"),
#'     summary = ellmer::type_string("One-sentence summary in English")
#'   ),
#'   input_type = "audio"
#' )
#' coded_audio <- qlm_code(
#'   c(interview1 = "interview1.mp3", interview2 = "interview2.wav"),
#'   speech_codebook, model = "google_gemini/gemini-2.5-flash"
#' )
#'
#' # A video codebook takes local files, YouTube links and URLs of video
#' # files in one vector; see "Video input" for what is uploaded and what
#' # the provider fetches itself
#' codebook_video <- qlm_codebook(
#'   name = "Video description",
#'   instructions = "Describe what is shown and transcribe what is said.",
#'   schema = ellmer::type_object(
#'     transcript = ellmer::type_string("Verbatim transcript of the speech"),
#'     setting = ellmer::type_string("Where the video is filmed, in a few words")
#'   ),
#'   input_type = "video"
#' )
#' coded_video <- qlm_code(
#'   c(clip = "clip.mp4", zoo = "https://www.youtube.com/watch?v=jNQXAC9IVRw"),
#'   codebook = codebook_video,
#'   model = "google_gemini/gemini-2.5-flash"
#' )
#' }
#'
#' @export
qlm_code <- function(x, codebook, model, ...,
                     batch = FALSE,
                     structured = c("auto", "structured", "json"),
                     json_retries = 2L,
                     on_error = c("continue", "return", "stop"),
                     backfill = FALSE, prices = NULL,
                     name = NULL, notes = NULL) {
  # Distinguishes a value the user chose from the default, so that the default
  # never errors but an explicit setting is never silently ignored.
  explicit_retries <- !missing(json_retries)
  explicit_structured <- !missing(structured)
  explicit_on_error <- !missing(on_error)
  structured <- match.arg(structured)
  on_error <- match.arg(on_error)
  # Checked here as well as in the JSON handler: under "auto" the handler is
  # reached only after the structured attempt has been paid for.
  if (!is_count(json_retries)) {
    cli::cli_abort("{.arg json_retries} must be a single non-negative integer.")
  }

  # Accept both qlm_codebook and task objects, converting if needed. A
  # qlm_codebook goes through too: one saved before a field existed is read
  # with that field filled, see fill_codebook_fields()
  if (inherits(codebook, "task")) {
    codebook <- as_qlm_codebook(codebook)
  }

  # Checked before any paid call; NULL means no backfill here, since a fresh
  # run has no parent whose passes could be replayed
  backfill <- backfill_passes(backfill)

  if (!inherits(codebook, "qlm_codebook")) {
    cli::cli_abort(c(
      "{.arg codebook} must be created using {.fn qlm_codebook}.",
      "i" = "Use {.fn qlm_codebook} or one of the predefined codebook functions."
    ))
  }

  # Input validation
  if (codebook$input_type == "text" && !is.character(x)) {
    cli::cli_abort("This codebook expects text input (a character vector).")
  }
  if (codebook$input_type %in% file_input_types()) {
    check_file_inputs(x, codebook$input_type)
  }
  if (codebook$input_type == "image") {
    check_image_resize(codebook, x)
  }
  # Audio and video are uploaded, and an upload gets a new reference every
  # time, so a batch job could never be resumed against ellmer's prompt-keyed
  # cache. Refused here, before any download or upload.
  if (codebook$input_type %in% uploaded_input_types() && isTRUE(batch)) {
    cli::cli_abort(c(
      "{.code batch = TRUE} is not supported for {codebook$input_type} input.",
      "i" = "ellmer's batch cache is keyed on the prompts, and an uploaded file gets a new reference on every upload, so a batch run could not be resumed.",
      "i" = "Re-run with {.code batch = FALSE}."
    ))
  }
  # Names become .id, the key every later operation relies on. Checked here,
  # before any request is spent, rather than in the constructor afterwards.
  if (!is.null(names(x))) {
    check_ids(names(x), what = "{.code names(x)}")
  }

  # Get valid argument names from ellmer functions
  pcs_arg_names <- names(formals(ellmer::parallel_chat_structured))
  batch_arg_names <- names(formals(ellmer::batch_chat_structured))

  # Route ... arguments
  # execution_args go to parallel_chat_structured or batch_chat_structured
  # Everything else (including provider-specific args like base_url) goes to chat()
  dots <- list(...)
  dot_names <- names(dots)

  if (!is.character(model) || length(model) != 1L || is.na(model)) {
    cli::cli_abort(c(
      "{.arg model} must be a single string.",
      "i" = "Use the form {.val provider/model} or {.val provider}, for example {.val openai/gpt-4o-mini}."
    ))
  }

  # A prefix ellmer cannot dispatch on is knowable without asking the provider
  # anything, so say so here rather than letting ellmer::chat() abort with
  # "Can't find provider ellmer::chat_qwen()".
  resolved <- resolve_provider(model, dots)
  model <- resolved$model
  dots <- resolved$args
  dot_names <- names(dots)

  # Checked after the model itself, so a bad model and a stray parameter report
  # the model first. `dot_names` is already in hand from the capture above.
  check_model_params(dot_names, model)

  # A file input never takes the JSON path: that handler sends text, so the
  # provider's own failure would be replaced by "supports text codebooks
  # only". Refused as an explicit request, and never chosen as a default.
  file_input <- codebook$input_type %in% file_input_types()
  if (file_input && explicit_structured && identical(structured, "json")) {
    cli::cli_abort(c(
      "{.code structured = \"json\"} is not supported for {codebook$input_type} input.",
      "i" = "JSON mode sends text and validates locally; a file input needs the provider's structured output.",
      "i" = "Use {.code structured = \"auto\"} or {.code \"structured\"}."
    ))
  }

  # A video URL is downloaded here first, so that it can be hashed and
  # uploaded like a file; the temporary copies go when the run returns (#179)
  resolved_files <- if (file_input) resolve_input_files(x, codebook$input_type)
  if (length(resolved_files$temp)) {
    on.exit(unlink(resolved_files$temp), add = TRUE)
  }

  # Hashed now, before any upload or request, so the record is of the bytes
  # about to be sent: a file replaced while requests run cannot be recorded
  # in their place, and one deleted then cannot stop the results coming back
  input_files <- if (file_input) {
    file_provenance(x, names(x) %||% seq_along(x), local = resolved_files$local)
  }

  # Offered where a provider refuses a file input: what is known to accept
  # the type, as a hint beside the provider's own message rather than as a
  # rule that would refuse a newer model (#179)
  provider_hint <- if (file_input) known_input_providers_hint(codebook$input_type)

  # Providers whose API rejects the schema-constrained request skip straight to
  # JSON mode rather than spending a wasted round trip; see
  # default_structured_mode().
  if (!explicit_structured && !file_input) {
    structured <- default_structured_mode(model)
  }

  # Repairing a response requires validating it, which only the JSON-mode path
  # does. Under `structured = "structured"` that path is never reached, so say
  # so rather than accepting a value that will not be applied.
  if (explicit_retries && identical(structured, "structured")) {
    cli::cli_abort(c(
      "{.arg json_retries} is not supported with {.code structured = \"structured\"}.",
      "i" = "It applies to the JSON-mode path; use {.code structured = \"auto\"} or {.code \"json\"}.",
      "i" = "For transport-level retries on any provider, set {.code options(ellmer_max_tries = )}."
    ))
  }

  # execution_args contains arguments for parallel_chat_structured or batch_chat_structured
  execution_arg_names <- unique(c(pcs_arg_names, batch_arg_names))
  execution_args <- dots[dot_names %in% execution_arg_names]

  # chat_args gets everything NOT destined for execution functions

  # This allows provider-specific args (base_url, credentials, api_args, etc.)
  # to pass through to ellmer::chat() which forwards them to the provider
  chat_args <- dots[!dot_names %in% execution_arg_names]

  # Supplied rates cost the run from its token counts, so both must be asked
  # of ellmer; the caller asked for a cost by supplying them.
  prices <- check_prices(prices)
  if (!is.null(prices)) {
    execution_args$include_tokens <- TRUE
    execution_args$include_cost <- TRUE
  }

  # ellmer returns a bare list under convert = FALSE, which has no rows to
  # merge an .id into and no columns for new_qlm_coded() to reorder. It has
  # never worked here; say so rather than failing later with "incorrect number
  # of dimensions".
  if (identical(execution_args$convert, FALSE)) {
    cli::cli_abort(c(
      "{.code convert = FALSE} is not supported by {.fn qlm_code}.",
      "i" = "A {.cls qlm_coded} object is built from the converted table, one row per unit.",
      "i" = "For the unconverted list, call {.fn ellmer::parallel_chat_structured} directly."
    ))
  }

  # `on_error` belongs to the parallel call; the batch API has no equivalent
  # and would refuse the argument. So under batch the default is simply not
  # sent, and an explicit setting is refused rather than silently ignored.
  # On a parallel run it is recorded with the other execution arguments, so
  # a replication or backfill replays the setting the run actually used.
  if (batch) {
    if (explicit_on_error) {
      cli::cli_abort(c(
        "{.arg on_error} is not supported with {.code batch = TRUE}.",
        "i" = "It controls a parallel run; the batch API has no equivalent.",
        "i" = "Re-run with {.code batch = FALSE} to use it."
      ))
    }
  } else {
    execution_args$on_error <- on_error
  }

  # Metadata contributed by the coding path
  backend_meta <- list()
  results <- NULL

  # Whether the cost will come back NA, and why, is read by each path from
  # the chat it builds, before it sends anything (#135). Kept here so the
  # reason outlives the console: it travels with the object.
  unpriced <- NULL
  attempt <- NULL
  fallback_reason <- NULL
  model_hint <- NULL

  # Every response is checked against the schema before it is tabulated, so
  # a schema the validator cannot check is refused now, in one error, rather
  # than after a paid run in which every unit fails
  check_coding_schema(codebook$schema)

  # ---- schema-constrained structured output -------------------------------
  if (structured %in% c("auto", "structured")) {
    attempt <- try_structured_call(
      x = x, codebook = codebook, model = model,
      chat_args = chat_args, execution_args = execution_args, batch = batch,
      cost_message = is.null(prices), local = resolved_files$local
    )

    unpriced <- attempt$unpriced

    # Every completed response failed validation: the endpoint accepted the
    # schema and ignored it. JSON mode is the cure where it can run; where it
    # cannot, the table of failures, with their reasons and usage, is what
    # the run has, and it is returned rather than thrown away
    can_fall_back <- identical(structured, "auto") && !file_input && !batch

    if (isTRUE(attempt$ok) && isTRUE(attempt$invalid) && can_fall_back) {
      fallback_reason <- attempt$error
      cli::cli_warn(c(
        "Structured output failed; falling back to JSON mode with local validation.",
        set_bullets(attempt$error)
      ))
    } else if (isTRUE(attempt$ok)) {
      results <- attempt$value
      # Every response was checked against the schema before conversion;
      # recorded so a reader of the run can tell it from one coded before
      # that was so (#140)
      backend_meta <- list(backend = "structured", validation = "local")
      if (isTRUE(attempt$invalid)) {
        cli::cli_warn(c(
          "No response from the structured call could be coded: every completed response failed validation against the schema.",
          set_bullets(attempt$error),
          "i" = if (identical(structured, "structured")) {
            "Use {.code structured = \"auto\"} to fall back to JSON mode with local validation."
          } else if (file_input) {
            "A {codebook$input_type} input cannot fall back to JSON mode, which sends text."
          } else {
            "JSON-mode coding has no batch path, so {.code structured = \"auto\"} cannot fall back here."
          },
          "i" = "{.fn qlm_failures} lists the units with the reasons; {.fn qlm_backfill} can retry them."
        ))
      }
    } else if (isTRUE(attempt$pending)) {
      # Not a failure either: the job is running at the provider, and its
      # state is on disk. Nothing to fall back to, and nothing to tabulate.
      cli::cli_abort(c(
        "The batch job has not finished.",
        "i" = "Re-run the same call with the same {.arg path} to resume it; ellmer keeps the job's state there.",
        "i" = "Or set {.code wait = TRUE} to wait for it."
      ))
    } else if (isTRUE(attempt$rejected) &&
               length(model_hint <- model_name_hint(model, chat_args))) {
      # The provider refused every request, and confirms it has no such
      # model. Falling back to JSON mode would only send the same name again,
      # so stop here. A refusal the provider does not explain that way may
      # be its answer to the schema-constrained request itself, which JSON
      # mode is the cure for, so that case falls through as before, carrying
      # the (empty) answer so the JSON path does not ask again.
      cli::cli_abort(c(
        "Every request to model {.val {model}} was rejected by the provider.",
        set_bullets(attempt$error),
        model_hint,
        provider_hint
      ))
    } else if (identical(structured, "structured")) {
      cli::cli_abort(c(
        "Structured output failed for model {.val {model}}.",
        set_bullets(attempt$error),
        "i" = "Use {.code structured = \"auto\"} to fall back to JSON mode with local validation.",
        provider_hint
      ))
    } else if (file_input) {
      # The JSON handler sends text, so a file input has nothing to fall back
      # to. The provider's error is the reason, so it is what is reported,
      # with what is known to accept the input beside it.
      cli::cli_abort(c(
        "Structured output failed for model {.val {model}}.",
        set_bullets(attempt$error),
        "i" = "A {codebook$input_type} input cannot fall back to JSON mode, which sends text.",
        provider_hint
      ))
    } else if (batch) {
      # JSON mode drives its own parallel requests and has no batch API path,
      # so under batch there is nothing to fall back to. Say that, rather than
      # letting the handler abort later with a message about `batch`.
      cli::cli_abort(c(
        "Structured output failed for model {.val {model}}.",
        set_bullets(attempt$error),
        "i" = "JSON-mode coding has no batch path, so {.code structured = \"auto\"} cannot fall back here.",
        "i" = "Re-run with {.code batch = FALSE} to use local validation instead."
      ))
    } else {
      fallback_reason <- attempt$error
      cli::cli_warn(c(
        "Structured output failed; falling back to JSON mode with local validation.",
        set_bullets(attempt$error)
      ))
    }
  }

  # ---- JSON mode with local validation ------------------------------------
  if (is.null(results)) {
    # A fallback from an attempt that came back re-codes only the units JSON
    # mode can help. A response cut off at the output limit, or an input
    # rejected as longer than the context window, fails the same way on any
    # path, so those units keep the row the structured attempt gave them,
    # with its reason and usage, as a backfill would leave them alone
    retry <- rep(TRUE, length(x))
    if (isTRUE(attempt$invalid) && !is.null(attempt$value)) {
      retry <- !is_terminal_failure(recorded_errors(attempt$value))
    }
    coded <- code_handler_json(
      x = x[retry],
      codebook = codebook,
      model = model,
      chat_args = chat_args,
      execution_args = execution_args,
      batch = batch,
      json_retries = json_retries,
      model_hint = model_hint,
      cost_message = is.null(attempt) && is.null(prices),
      # What the structured attempt spent is part of what the run cost
      prior_usage = attempt$usage[retry, , drop = FALSE]
    )
    backend_meta <- attr(coded, "qlm_backend_meta") %||% list()
    attr(coded, "qlm_backend_meta") <- NULL
    if (is.null(unpriced)) {
      unpriced <- backend_meta$unpriced
    }
    backend_meta$unpriced <- NULL
    results <- if (all(retry)) {
      coded
    } else {
      backend_meta$fallback_kept <- sum(!retry)
      merge_fallback_rows(attempt$value, coded, retry)
    }
  }

  backend_meta$structured <- structured
  if (!is.null(fallback_reason)) {
    backend_meta$fallback_reason <- fallback_reason
  }

  # Add ID column from input names or sequence
  results$id <- names(x) %||% seq_along(x)

  # Where the cost came from, when ellmer could not fill it (#135)
  settled <- reconcile_prices(results, prices, unpriced)
  results <- settled$results
  prices <- settled$prices
  cost_note <- settled$cost_note

  # An audio cost is computed at the text rate, whoever supplied the rates,
  # so the qualification joins whatever note the cost already carries rather
  # than replacing it, and is said once here as well as kept with the object
  if (identical(codebook$input_type, "audio") &&
      (isTRUE(execution_args$include_cost) || !is.null(prices))) {
    cost_note <- paste(c(cost_note, audio_cost_note()), collapse = "; ")
    cli::cli_inform(c("i" = paste0("Cost: ", audio_cost_note(), ".")))
  }

  # Build metadata list
  metadata <- list(
    timestamp = Sys.time(),
    n_units = length(x),
    notes = notes,
    ellmer_version = tryCatch(
      as.character(utils::packageVersion("ellmer")),
      error = function(e) NA_character_
    ),
    quallmer_version = tryCatch(
      as.character(utils::packageVersion("quallmer")),
      error = function(e) NA_character_
    ),
    R_version = paste(R.version$major, R.version$minor, sep = ".")
  )

  # Fields contributed by a provider-specific handler (backend, json_retries, ...)
  metadata <- c(metadata, backend_meta)
  metadata$provider_resolution <- resolved$resolution
  if (!is.null(cost_note)) {
    metadata$cost_note <- cost_note
  }
  # What was coded, so a later replication or backfill can check it is
  # uploading the same bytes, and the trail can name the files
  if (!is.null(input_files)) {
    metadata$input_files <- input_files
  }
  if (!is.null(prices)) {
    metadata$prices <- prices
  }

  # Add model to chat_args for easy access
  chat_args$name <- model

  # An ordinal enum is stored as an ordered factor in the enum's order, the
  # shape the reliability statistics read the scale order from (#165)
  results <- apply_ordinal_enums(results, codebook)

  coded <- new_qlm_coded(
    results = results,
    codebook = codebook,
    data = x,
    input_type = codebook$input_type,
    chat_args = chat_args,
    execution_args = execution_args,
    batch = batch,
    metadata = metadata,
    name = name,
    call = match.call(),
    parent = NULL
  )

  # Complete the run in the same call, with the same model and settings
  if (backfill > 0L) {
    coded <- qlm_backfill(coded, passes = backfill)
  }
  coded
}

#' Default structured-output mode for a model
#'
#' Providers whose API rejects the schema-constrained request outright should
#' not spend a guaranteed-wasted round trip discovering that. DeepSeek is the
#' known case: its endpoint answers `response_format` of type `json_schema`
#' with HTTP 400, "This response_format type is unavailable now". Everything
#' else defaults to attempting the structured call.
#'
#' Only a *default*: an explicit `structured =` always wins.
#'
#' @param model Provider (and optionally model) name, as passed to [qlm_code()].
#'
#' @return One of `"auto"`, `"structured"` or `"json"`.
#' @keywords internal
#' @noRd
default_structured_mode <- function(model) {
  provider <- model_provider(model)

  switch(provider,
    deepseek = "json",
    "auto"
  )
}


#' Attempt schema-constrained structured output
#'
#' Runs the structured call through `structured_chat_turns()` and validates
#' every response against the codebook before conversion (#140), so that
#' [qlm_code()] can decide what to do with what came back. A unit whose
#' response failed -- refused by the provider, cut off, without JSON, or
#' with JSON that is not the schema -- keeps its row, its reason in `.error`
#' and its usage; the other units keep their coded values.
#'
#' Two things count as failure of the attempt as a whole: the call throwing,
#' and every response that reached validation failing it, which is what an
#' endpoint that accepted the schema and ignored it produces. Only responses
#' the provider completed are judged: a refused request or a cut-off
#' response says nothing about whether the endpoint honours the schema.
#'
#' @param x,codebook,model,chat_args,execution_args,batch As in [qlm_code()].
#' @param cost_message Whether to say now why the cost will be `NA`.
#' @param local The local path of each element of `x`, from
#'   `resolve_input_files()`, or `NULL` for a text input.
#'
#' @return A list with `ok`, and either `value` (the results, with `usage`
#'   alongside) or `error`. `rejected` marks a provider that refused every
#'   request with a status that will not change. `invalid`, alongside
#'   `ok = TRUE`, marks an endpoint whose every completed response failed
#'   validation, with `error` saying so and the table of failures in
#'   `value`: [qlm_code()] falls back where it can, and keeps the table
#'   where it cannot. `pending` marks a batch job not yet finished.
#' @keywords internal
#' @noRd
try_structured_call <- function(x, codebook, model, chat_args, execution_args, batch,
                                cost_message = TRUE, local = NULL) {
  system_prompt <- if (!is.null(codebook$role)) {
    paste(codebook$role, codebook$instructions, sep = "\n\n")
  } else {
    codebook$instructions
  }

  build_chat <- function(prompt) {
    do.call(ellmer::chat, c(list(name = model, system_prompt = prompt), chat_args))
  }
  chat <- build_chat(system_prompt)

  # From the chat the run will use, before anything is sent (#135)
  unpriced <- cost_diagnosis(chat, model, execution_args, say = cost_message)

  # A URL detail setting that will not reach the provider is said here,
  # before anything is sent. Not tied to `cost_message`: supplying `prices`
  # silences the cost note, and must not silence this (#177)
  if (codebook$input_type == "image") {
    say_image_url_detail(codebook, x, chat)
  }

  # What the run depends on in ellmer, checked before any upload or request
  ellmer_structured_internals()

  # Every upload completes here, before the first request: either all the
  # inputs are ready or nothing is spent. Text and images are built inline.
  prompts <- as_input_content(x, codebook, chat, local = local)

  run <- function(chat) {
    structured_chat_turns(
      chat, prompts, codebook$schema, batch = batch,
      execution_args = execution_args
    )
  }

  # `rejected` records whether the provider refused the run with a status
  # that will not change on retry, so qlm_code() can ask about the model
  # name when it reports; the message alone rarely says.
  attempt <- tryCatch(
    list(ok = TRUE, turns = run(chat)),
    error = function(e) list(
      ok = FALSE, error = strip_ansi(conditionMessage(e)),
      rejected = is_fatal_status(api_error_status(e))
    )
  )

  # Alibaba Model Studio refuses `response_format` unless the word "json"
  # appears in the messages. That is a request-shape requirement, not a coding
  # one, and the endpoint does enforce the schema once the request is accepted
  # -- so satisfy it and retry rather than falling back and losing enforcement.
  # The added sentence concerns output format only and names no coding
  # criterion, so the substantive instrument is unchanged.
  #
  # Defensive: not reachable against ellmer's current request shape, which
  # sends json_schema rather than json_object. See is_json_word_error().
  if (!isTRUE(attempt$ok) && is_json_word_error(attempt$error)) {
    retry_prompt <- paste(
      system_prompt,
      "Return your answer as a single JSON object.",
      sep = "\n\n"
    )
    attempt <- tryCatch(
      list(ok = TRUE, turns = run(build_chat(retry_prompt))),
      error = function(e) list(
        ok = FALSE, error = strip_ansi(conditionMessage(e)),
        rejected = is_fatal_status(api_error_status(e))
      )
    )
  }

  if (isTRUE(attempt$ok)) {
    if (is.null(attempt$turns)) {
      # A batch job told not to wait, and not finished: nothing to tabulate
      attempt <- list(ok = FALSE, pending = TRUE,
                      error = "the batch job has not finished")
    } else {
      provider <- tryCatch(chat$get_provider(), error = function(e) NULL)
      attempt <- structured_attempt(
        attempt$turns, codebook$schema, provider, execution_args
      )
    }
  }

  attempt$unpriced <- unpriced
  attempt
}


#' Put the rows a fallback re-coded back among the rows it kept
#'
#' @param kept The structured attempt's table, one row per unit.
#' @param coded The JSON handler's table for the units in `retry`.
#' @param retry Which rows of `kept` `coded` replaces.
#'
#' @return `kept` with those rows replaced, in ellmer's column order.
#' @keywords internal
#' @noRd
merge_fallback_rows <- function(kept, coded, retry) {
  if (!".error" %in% names(kept)) {
    kept$.error <- vector("list", nrow(kept))
  }
  if (!".error" %in% names(coded)) {
    coded$.error <- vector("list", nrow(coded))
  }
  out <- vctrs::vec_assign(kept, which(retry), coded[names(kept)])
  trailing <- intersect(
    c("input_tokens", "output_tokens", "cached_input_tokens", "cost"), names(out)
  )
  out[c(setdiff(names(out), c(".error", trailing)), ".error", trailing)]
}


#' Validate and tabulate the turns of a structured call
#'
#' @param turns What `structured_chat_turns()` returned.
#' @param schema The codebook schema.
#' @param provider The chat's provider, for ellmer's wrapper rule.
#' @param execution_args As in [qlm_code()]; read for the usage columns.
#'
#' @return An attempt, as `try_structured_call()` documents it.
#' @keywords internal
#' @noRd
structured_attempt <- function(turns, schema, provider, execution_args) {
  records <- add_structured_values(
    turn_records(turns),
    needs_wrapper = structured_needs_wrapper(schema, provider)
  )
  n <- length(turns)
  values <- vector("list", n)
  errors <- vector("list", n)
  stage <- rep(NA_character_, n)
  problems <- rep(NA_character_, n)

  for (i in seq_len(n)) {
    checked <- if (!is.null(records$value[[i]])) {
      validate_structured_value(records$value[[i]], schema)
    }
    settled <- settle_response(
      checked, problem = records$problem[[i]], error = records$error[[i]],
      finish = records$finish[[i]],
      output_tokens = records$usage[i, "output_tokens"]
    )
    stage[[i]] <- settled$stage
    if (isTRUE(settled$ok)) {
      values[i] <- list(settled$value)
    } else {
      problems[[i]] <- settled$error
      errors[i] <- list(unit_error(settled$error, settled$stage, settled$truncated))
    }
  }

  # Every request refused with a status that will not change on retry is
  # misconfiguration, most often a model name the provider does not have.
  # Returned as a failed attempt, with the reasons, so that qlm_code() can ask
  # the provider about the name rather than hand back a table of NAs.
  fatal <- vapply(records$status, is_fatal_status, logical(1))
  if (n && all(stage == "transport" & fatal)) {
    return(list(ok = FALSE, rejected = TRUE, error = unique(problems)))
  }

  # Whether the endpoint honours the schema is judged from the responses it
  # completed: JSON that is not the schema, or no JSON at all, is what an
  # endpoint that accepted the schema and ignored it produces. A refused
  # request or a cut-off response is set aside. If none reached validation,
  # there is nothing to conclude, and the failures stand as recorded.
  assessed <- stage %in% c("ok", "extraction", "schema")
  invalid <- any(assessed) && !any(stage == "ok")

  failed <- !is.na(stage) & stage != "ok"
  if (any(failed) && !invalid) {
    cli::cli_warn(c(
      "{sum(failed)} response{?s} from the structured call could not be coded, out of {n}.",
      set_bullets(unique(problems[failed])),
      "i" = "Their coded values are missing; {.fn qlm_failures} lists them with the reasons."
    ))
  }

  # The table is built either way: where qlm_code() has nothing to fall back
  # to, the failed rows, their reasons and their usage are what the run has
  value <- tabulate_results(
    values, errors, records$usage, schema,
    include_tokens = isTRUE(execution_args$include_tokens),
    include_cost = isTRUE(execution_args$include_cost)
  )
  attempt <- list(ok = TRUE, value = value, usage = records$usage,
                  n_invalid = sum(failed))
  if (invalid) {
    attempt$invalid <- TRUE
    attempt$error <- paste0(
      "the structured call returned no usable values (every completed ",
      "response failed validation against the schema, for example: ",
      problems[assessed][[1]],
      "); the endpoint appears not to honour the JSON schema"
    )
  }
  attempt
}








#' The output limit the caller declared, if any
#'
#' Read from `params(max_tokens = )` and nowhere else. ellmer fills in a
#' provider default when none is set (4096 for Anthropic, at the time of
#' writing), but that default lives in its request builder and is not visible
#' from here, so an undeclared limit is treated as unknown rather than guessed
#' at. `api_args` is not consulted either: quallmer forwards it unchanged and
#' does not inspect it.
#'
#' @param chat_args List of arguments destined for [ellmer::chat()].
#'
#' @return A positive number, or `NULL`.
#' @keywords internal
#' @noRd
declared_max_tokens <- function(chat_args) {
  params <- chat_args$params
  if (!is.list(params)) {
    return(NULL)
  }
  cap <- params$max_tokens
  if (is.numeric(cap) && length(cap) == 1L && is.finite(cap) && cap > 0) {
    as.numeric(cap)
  } else {
    NULL
  }
}








blank_cell <- function(x) {
  if (is.null(x)) {
    return(TRUE)
  }
  if (is.data.frame(x)) {
    return(nrow(x) == 0L)
  }
  length(x) == 0L
}


#' An error recorded for a response that held no structured data at all
#'
#' Prose where JSON was asked for, or JSON that does not parse. Carries a
#' class of its own so that the fallback decision in `structured_attempt()`
#' can tell it from a request failure or a cut-off response. The distinction
#' matters: here the endpoint did answer, and the answer was not the schema,
#' which is precisely the evidence that decision reads; a request that never
#' got an answer, or an answer the provider cut short, is not.
#'
#' @param message What was wrong with the response.
#'
#' @return A condition inheriting from `simpleError`.
#' @keywords internal
#' @noRd
extraction_error <- function(message) {
  structure(
    simpleError(message),
    class = c("quallmer_extraction_error", "simpleError", "error", "condition")
  )
}

is_extraction_error <- function(e) {
  inherits(e, "quallmer_extraction_error")
}




#' Create a qlm_coded object (internal)
#'
#' Low-level constructor for qlm_coded objects. This function is not exported
#' and is intended for internal use by [qlm_code()] and [qlm_replicate()].
#'
#' The object is a tibble with additional qlm_coded class and attributes.
#'
#' @param results Data frame of coded results with id column.
#' @param codebook A qlm_codebook object.
#' @param data The original input data (x from qlm_code).
#' @param input_type Type of input ("text", "image" or "audio").
#' @param chat_args List of arguments passed to ellmer::chat().
#' @param execution_args List of arguments passed to ellmer::parallel_chat_structured()
#'   or ellmer::batch_chat_structured(). For backward compatibility, also accepts
#'   pcs_args as an alias.
#' @param batch Logical indicating whether batch processing was used.
#' @param metadata List of metadata (timestamp, versions, etc.).
#' @param name Character string identifying this run.
#' @param call The call that created this object.
#' @param parent Character string identifying parent run (NULL for originals).
#' @param pcs_args Deprecated. Use execution_args instead.
#'
#' @return A qlm_coded object (tibble with attributes).
#' @importFrom utils head
#' @keywords internal
#' @noRd
new_qlm_coded <- function(results, codebook, data, input_type, chat_args,
                           execution_args = NULL, batch = FALSE, metadata,
                           name, call, parent = NULL, pcs_args = NULL) {
  # Backward compatibility: if pcs_args is provided but execution_args is not
  if (is.null(execution_args) && !is.null(pcs_args)) {
    execution_args <- pcs_args
  }
  # Rename id column to .id
  names(results)[names(results) == "id"] <- ".id"

  # Exactly one identifier column, holding a key. Every merge downstream --
  # comparison, validation, backfill -- keys on .id and would silently pair
  # the wrong rows (#156).
  if (sum(names(results) == ".id") != 1L) {
    cli::cli_abort(c(
      "{.arg results} must have exactly one {.field .id} column; found {sum(names(results) == '.id')}.",
      "i" = "An {.code id} column is renamed to {.field .id}, so the two must not both be present."
    ))
  }
  check_ids(results$.id, what = "{.field .id}")

  # Reorder columns to put .id first
  results <- results[, c(".id", setdiff(names(results), ".id"))]

  # Convert to tibble (always available via ellmer)
  results <- tibble::as_tibble(results)

  # Add qlm_coded class and attributes with new metadata structure
  # Build object metadata - include source and is_gold if present
  object_meta <- list(
    batch = batch,
    call = call,
    chat_args = chat_args,
    execution_args = execution_args,
    parent = parent,
    n_units = metadata$n_units,
    input_type = input_type
  )

  # Add source and is_gold from metadata if present (for human-coded data)
  if (!is.null(metadata$source)) {
    object_meta$source <- metadata$source
  }
  if (!is.null(metadata$is_gold)) {
    object_meta$is_gold <- metadata$is_gold
  }
  if (!is.null(metadata$backend)) {
    object_meta$backend <- metadata$backend
  }
  if (!is.null(metadata$structured)) {
    object_meta$structured <- metadata$structured
  }
  if (!is.null(metadata$validation)) {
    object_meta$validation <- metadata$validation
  }

  object_meta$provider_resolution <- metadata$provider_resolution

  # Build user metadata: start with name and notes, then add custom metadata
  user_meta <- list(
    name = name,
    notes = metadata$notes
  )

  # Add any custom metadata fields (exclude system, object, and user-handled fields)
  system_fields <- c("timestamp", "ellmer_version", "quallmer_version", "R_version")
  object_fields <- c("n_units", "source", "is_gold", "backend", "structured",
                     "validation", "provider_resolution")
  user_handled <- c("name", "notes")
  exclude_fields <- c(system_fields, object_fields, user_handled)

  custom_metadata <- metadata[!names(metadata) %in% exclude_fields]
  if (length(custom_metadata) > 0) {
    user_meta <- c(user_meta, custom_metadata)
  }

  structure(
    results,
    class = c("qlm_coded", class(results)),
    data = data,
    codebook = codebook,
    meta = list(
      user = user_meta,
      object = object_meta,
      system = list(
        timestamp = metadata$timestamp,
        ellmer_version = metadata$ellmer_version,
        quallmer_version = metadata$quallmer_version,
        R_version = metadata$R_version
      )
    )
  )
}


#' Subset a qlm_coded object
#'
#' Tibble subsetting keeps the class and attributes, so a subset is still a
#' `qlm_coded` object; what it must also still be is a table keyed by
#' `.id`. Repeating rows, `x[c(1, 1), ]`, would produce an object with a
#' repeated identifier that never passed through the constructor, and every
#' merge downstream would then pair the wrong rows (#156). So the identifier
#' is checked again here, where the duplicate would be made. Selecting the
#' identifier away, `x["score"]`, leaves nothing for the class to promise,
#' so the result is returned as a plain tibble rather than as a coded object
#' every consumer would have to reject.
#'
#' @param x A qlm_coded object.
#' @param i,j Row and column indices, as for a tibble.
#' @param ... Passed on to the tibble method.
#'
#' @return A qlm_coded object when `.id` is among the columns kept; a plain
#'   tibble when it is not; a vector when the tibble method returns one.
#' @keywords internal
#' @export
`[.qlm_coded` <- function(x, i, j, ...) {
  out <- NextMethod()
  if (!is.data.frame(out)) {
    return(out)
  }
  n_id <- sum(names(out) == ".id")
  if (n_id == 0L) {
    for (a in c("data", "codebook", "meta", "run", "input_type")) {
      attr(out, a) <- NULL
    }
    class(out) <- setdiff(class(out), c("qlm_coded", "qlm_humancoded"))
    return(out)
  }
  # Selecting .id twice would keep the class on a table with two key columns,
  # of which `[[` reads only the first
  if (n_id > 1L) {
    cli::cli_abort(c(
      "A {.cls qlm_coded} object must have exactly one {.field .id} column; the subset would have {n_id}.",
      "i" = "{.field .id} identifies each unit and is the key every operation merges on."
    ))
  }
  check_ids(out[[".id"]], what = "{.field .id} of the subset")
  out
}


#' Print a qlm_coded object
#'
#' @param x A qlm_coded object.
#' @param ... Additional arguments passed to print methods.
#'
#' @return Invisibly returns the input object \code{x}. Called for side effects (printing to console).
#' @keywords internal
#' @export
print.qlm_coded <- function(x, ...) {
  # Auto-upgrade old structure if needed
  x <- upgrade_meta(x)

  meta_attr <- attr(x, "meta")
  codebook_attr <- attr(x, "codebook")

  # Print header
  cat("# quallmer coded object\n")
  cat("# Run:      ", meta_attr$user$name, "\n", sep = "")

  # Distinguish human vs LLM coding
  if (!is.null(meta_attr$object$source) && meta_attr$object$source == "human") {
    cat("# Source:   Human coder\n")
    if (!is.null(codebook_attr$name) && codebook_attr$name != "Human-coded data") {
      cat("# Codebook: ", codebook_attr$name, "\n", sep = "")
    }
  } else {
    cat("# Codebook: ", codebook_attr$name, "\n", sep = "")
    cat("# Model:    ", meta_attr$object$chat_args$name %||% "unknown", "\n", sep = "")
    if (!is.null(meta_attr$object$provider_resolution)) {
      cat("# Requested model: ",
          provider_request_label(meta_attr$object$provider_resolution), "\n", sep = "")
    }
  }

  # Show if this is a gold standard
  if (!is.null(meta_attr$object$is_gold) && isTRUE(meta_attr$object$is_gold)) {
    cat("# Gold:     Yes\n")
  }

  # Units attempted, and how many came back with nothing usable, so that a
  # partly failed run cannot look complete (#132). Failed rows beyond the
  # printed head of the tibble would otherwise go unseen. Row subsetting keeps
  # the class and the original count, so when the rows present differ from
  # the units attempted, say so, and count over the rows present.
  n_units <- meta_attr$object$n_units
  n_rows <- nrow(x)
  n_failed <- sum(failed_units(x))
  units <- if (!is.null(n_units) && n_units != n_rows) {
    paste0(n_units, " attempted, ", n_rows, " present")
  } else {
    as.character(n_units %||% n_rows)
  }
  breakdown <- if (n_failed > 0) {
    paste0(" (", n_rows - n_failed, " scored, ", n_failed, " failed)")
  } else {
    ""
  }
  cat("# Units:    ", units, breakdown, "\n", sep = "")

  # A backfilled object is no longer the product of one call, and one
  # completed by another model is a composite; say so here (#136)
  backfill <- backfill_summary(meta_attr$object$backfill)
  if (!is.null(backfill)) {
    cat("# Backfill: ", backfill, "\n", sep = "")
  }

  if (!is.null(meta_attr$object$parent)) {
    cat("# Parent:   ", meta_attr$object$parent, "\n", sep = "")
  }

  # Where the cost column came from, when ellmer could not fill it: why it
  # is NA, or the supplied rates it was computed from
  if (!is.null(meta_attr$user$cost_note)) {
    cat("# Cost:     ", meta_attr$user$cost_note, "\n", sep = "")
  }
  # A pass costed differently from the run: part of the same column rests
  # on it, so it is disclosed here too (#136)
  pass_notes <- backfill_cost_notes(meta_attr$object$backfill, meta_attr$user$cost_note)
  for (i in seq_along(pass_notes)) {
    cat("# Cost (", names(pass_notes)[i], "): ", pass_notes[[i]], "\n", sep = "")
  }

  # Show notes if present
  if (!is.null(meta_attr$user$notes)) {
    cat("# Notes:    ", meta_attr$user$notes, "\n", sep = "")
  }

  cat("\n")

  # Print data using parent class method
  NextMethod()
}





