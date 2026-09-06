# The installed quallmer predates video input: load the checkout instead
devtools::load_all(".")

cb <- qlm_codebook(
  name = "Video smoke test",
  instructions = paste(
    "Describe the visible scene and movement.",
    "Transcribe any speech exactly; use an empty string if none."
  ),
  schema = ellmer::type_object(
    scene = ellmer::type_string("Visible scene and movement"),
    transcript = ellmer::type_string("Speech, or an empty string")
  ),
  input_type = "video"
)

# Real files in each container format
clips <- c(
  avi = '/Users/kbenoit/Dropbox/Work/Classes/Essex Summer School Misc/my Provalis Research Projects/Scripts/Earth.avi'
)

# Run separately so one rejected format doesn't block the others.
results <- lapply(names(clips), function(fmt) {
  tryCatch(
    qlm_code(
      setNames(clips[[fmt]], fmt),
      codebook = cb,
      model = "google_gemini/gemini-2.5-flash",
      include_cost = TRUE
    ),
    error = identity
  )
})
names(results) <- names(clips)

results
