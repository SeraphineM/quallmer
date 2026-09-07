# A paid call against the real endpoint, run only when asked for:
#   QUALLMER_LIVE_TESTS=true with OPENAI_API_KEY set.
# The clip is the first Harvard list from the Open Speech Repository, so the
# transcript has a known opening sentence to check against.

test_that("qlm_transcribe transcribes a recording from the web with the default model", {
  skip_on_cran()
  skip_if_offline()
  skip_if(!identical(Sys.getenv("QUALLMER_LIVE_TESTS"), "true"), "QUALLMER_LIVE_TESTS is not 'true'")
  skip_if(!nzchar(Sys.getenv("OPENAI_API_KEY")), "OPENAI_API_KEY is not set")

  url <- c(harvard = "https://www.voiptroubleshooter.com/open_speech/american/OSR_us_000_0010_8k.wav")
  out <- qlm_transcribe(url, language = "en")

  expect_s3_class(out, "qlm_transcript")
  expect_equal(names(out), "harvard")
  expect_match(out[["harvard"]], "birch canoe", ignore.case = TRUE)
  prov <- attr(out, "provenance")
  expect_equal(prov$status, "ok")
  expect_equal(prov$file, "OSR_us_000_0010_8k.wav")
  expect_equal(prov$source_url, unname(url))
  expect_equal(prov$size, 538014)
  expect_equal(prov$sha256, "a4bf9becd046d7aedb6d05b6e12347a6294a44f74d263089c636fb0a2b1e6561")
  expect_equal(prov$usage[[1]]$type, "tokens")
})
