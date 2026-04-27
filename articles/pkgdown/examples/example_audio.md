# Example: Audio transcription and analysis

This example demonstrates a two-step process for analyzing audio
content: (1) transcribing audio to text using OpenAI’s Whisper model,
and (2) extracting structured information from the transcripts using
[`qlm_code()`](https://quallmer.github.io/quallmer/reference/qlm_code.md).
This workflow enables large-scale analysis of speeches, interviews, and
other audio content.

## Loading packages and data

``` r

library(quallmer)
library(dplyr)
library(purrr)
library(knitr)
```

First, we identify the audio files to analyze:

``` r

# Get all audio files from the data folder
audio_files <- list.files("data/audio/",
                          pattern = "\\.(wav|mp3)$",
                          full.names = TRUE)

cat("Found", length(audio_files), "audio files:\n")
```

    ## Found 6 audio files:

``` r

for (f in audio_files) {
  size_mb <- file.size(f) / 1024^2
  duration_sec <- NA  # Would require audio package to calculate
  cat(sprintf("  %s (%.2f MB)\n", basename(f), size_mb))
}
```

    ##   F1523643-7930-4FCA-B0E1-1CB5AEBE6BF2.mp3 (0.50 MB)
    ##   harvard.wav (3.10 MB)
    ##   OSR_cn_000_0072_8k.wav (0.30 MB)
    ##   OSR_cn_000_0075_8k.wav (0.38 MB)
    ##   OSR_fr_000_0041_8k.wav (1.22 MB)
    ##   OSR_in_000_0064_8k.wav (0.57 MB)

These audio files contain speech samples in multiple languages from the
Open Speech Repository and other sources.

## Step 1: Transcribing audio with Whisper

OpenAI’s Whisper model provides high-quality, multilingual
transcription. We use the `openai` package to transcribe each audio
file:

``` r

library(openai)

# Transcribe all audio files
transcriptions <- map_chr(audio_files, function(file_path) {
  cat("Transcribing:", basename(file_path), "\n")

  transcription <- create_transcription(
    file = file_path,
    model = "whisper-1"
  )

  transcription$text
})

# Name the transcriptions by filename
names(transcriptions) <- basename(audio_files)

# Save transcriptions
saveRDS(transcriptions, "data/transcriptions_whisper.rds")
```

## Viewing the transcriptions

Let’s examine the transcribed content:

``` r

# Display each transcription (truncated for readability)
for (i in seq_along(transcriptions)) {
  cat("=== File:", names(transcriptions)[i], "===\n")

  # Show first 200 characters
  text_preview <- substr(transcriptions[i], 1, 200)
  if (nchar(transcriptions[i]) > 200) {
    text_preview <- paste0(text_preview, "...")
  }
  cat(text_preview, "\n\n")
}
```

    ## === File: F1523643-7930-4FCA-B0E1-1CB5AEBE6BF2.mp3 ===
    ## And this is such an important topic because I'm sure you've felt it. There's tension in the air, right? Everywhere you go. I don't know if it's the headlines or the fact that everybody is so stressed ... 
    ## 
    ## === File: harvard.wav ===
    ## The stale smell of old beer lingers. It takes heat to bring out the odor. A cold dip restores health and zest. A salt pickle tastes fine with ham. Tacos al pastor are my favorite. A zestful food is th... 
    ## 
    ## === File: OSR_cn_000_0072_8k.wav ===
    ## 院子门口不远处就是一个地铁站,这是一个美丽而神奇的景象,树上长满了又大又甜的桃子,海豚和鲸鱼的表演是很好看的节目。 邮局门前的人行道上有一个蓝色的邮箱。 
    ## 
    ## === File: OSR_cn_000_0075_8k.wav ===
    ## 天文望远镜可以用来观察天空 它到过很多地方观光旅游 山间的小道蜿蜒曲折 春天来了,山上开满了樱花 下雪以后,田野里白矮矮的一片 
    ## 
    ## === File: OSR_fr_000_0041_8k.wav ===
    ## Pourrais-je avoir un verre d'eau ? La SNCF assurera un train sur trois. Les coupoles de l'immense palais s'écroulèrent. On apercevait la voile blanche du petit bateau. Il ne sentit ni douleur ni secou... 
    ## 
    ## === File: OSR_in_000_0064_8k.wav ===
    ## शालिनी के पास सौ रुपए हैं। सीता और सुनील का लड़का बहुत होशयार है। तुम्हारी कविता लिठने का शौक कब से शुरू हुआ। सोते हुए शेर को जगाना उच्चित नहीं है। शोर मत करो नहीं तो सुहासिनी जाग जाएगी। काम शुरू होने...

``` r

# Word counts
word_counts <- map_int(transcriptions, ~length(strsplit(.x, "\\s+")[[1]]))
cat("Word count statistics:\n")
```

    ## Word count statistics:

``` r

cat("  Range:", min(word_counts), "-", max(word_counts), "words\n")
```

    ##   Range: 2 - 119 words

``` r

cat("  Mean:", round(mean(word_counts)), "words\n")
```

    ##   Mean: 49 words

## Step 2: Defining the transcript analysis codebook

Now we create a codebook to extract structured information from the
transcripts. This codebook analyzes language, topics, tone, and content:

``` r

# Define a comprehensive transcript analysis codebook
codebook_transcripts <- qlm_codebook(
  name = "Speech Transcript Analysis",
  instructions = paste(
    "You are a research assistant analyzing transcribed speech content.",
    "Provide structured analysis of the speech based on its content.",
    "Be objective and accurate in your assessments."
  ),
  schema = ellmer::type_object(
    language = ellmer::type_string(
      "Primary language of the speech, in English (e.g., 'English', 'Mandarin', 'French', 'Indonesian')"
    ),
    language_confidence = ellmer::type_enum(
      c("high", "medium", "low"),
      "Confidence in language identification"
    ),
    speech_type = ellmer::type_enum(
      c("conversational", "formal_speech", "reading", "spontaneous", "other"),
      "Type or style of speech"
    ),
    main_topics = ellmer::type_string(
      "Main topics or themes discussed in the speech (comma-separated, max 5 topics)"
    ),
    key_phrases = ellmer::type_string(
      "Important phrases or keywords mentioned (comma-separated, max 5 phrases)"
    ),
    tone = ellmer::type_enum(
      c("formal", "informal", "neutral", "technical", "conversational"),
      "Overall tone of the speech"
    ),
    sentiment = ellmer::type_enum(
      c("positive", "negative", "neutral", "mixed"),
      "Overall sentiment expressed in the speech"
    ),
    summary = ellmer::type_string(
      "Brief 2-3 sentence summary of the speech content"
    )
  ),
  role = "You are an expert linguist and discourse analyst.",
  input_type = "text"
)

# View the codebook structure
codebook_transcripts
```

    ## quallmer codebook: Speech Transcript Analysis 
    ##   Input type:   text
    ##   Role:         You are an expert linguist and discourse analyst.
    ##   Instructions: You are a research assistant analyzing transcribed speech co...
    ##   Output schema:ellmer::TypeObject
    ##   Levels:
    ##     language: nominal
    ##     language_confidence: nominal
    ##     speech_type: nominal
    ##     main_topics: nominal
    ##     key_phrases: nominal
    ##     tone: nominal
    ##     sentiment: nominal
    ##     summary: nominal

## Coding transcripts using Gemini 2.5 Flash

We use Gemini 2.5 Flash to analyze the transcripts. This model is fast
and cost-effective for text analysis:

``` r

# Apply transcript analysis using qlm_code()
coded_transcripts <- qlm_code(
  transcriptions,
  codebook = codebook_transcripts,
  model = "google_gemini/gemini-2.5-flash",
  name = "audio_transcripts_gemini",
  notes = "Analysis of multilingual speech transcripts",
  include_cost = TRUE
)

# Add filenames to results
coded_transcripts$.filename <- names(transcriptions)

# Save results
saveRDS(coded_transcripts, "data/coded_transcripts_gemini.rds")
```

## Examining the results

Let’s view the extracted information:

``` r

# Display key results
coded_transcripts %>%
  select(.filename, language, language_confidence, speech_type,
         tone, sentiment) %>%
  kable(
    col.names = c("File", "Language", "Confidence", "Type", "Tone", "Sentiment"),
    caption = "Transcript Analysis Results"
  )
```

| File | Language | Confidence | Type | Tone | Sentiment |
|:---|:---|:---|:---|:---|:---|
| F1523643-7930-4FCA-B0E1-1CB5AEBE6BF2.mp3 | English | high | conversational | conversational | negative |
| harvard.wav | English | high | other | neutral | neutral |
| OSR_cn_000_0072_8k.wav | Mandarin | high | spontaneous | neutral | positive |
| OSR_cn_000_0075_8k.wav | Mandarin | high | other | neutral | neutral |
| OSR_fr_000_0041_8k.wav | French | high | spontaneous | neutral | mixed |
| OSR_in_000_0064_8k.wav | Hindi | high | spontaneous | neutral | mixed |

Transcript Analysis Results {.table}

Total cost for analyzing 6 transcripts:

``` r

cat("Transcription cost (Whisper): ~$",
    round(sum(word_counts) * 0.006 / 100, 4), " (estimated)\n", sep = "")
```

    ## Transcription cost (Whisper): ~$0.0177 (estimated)

``` r

cat("Analysis cost (Gemini): $",
    round(sum(coded_transcripts$cost, na.rm = TRUE), 4), "\n", sep = "")
```

    ## Analysis cost (Gemini): $0.0128

### Language distribution

``` r

cat("Languages detected:\n")
```

    ## Languages detected:

``` r

language_table <- table(coded_transcripts$language)
print(language_table)
```

    ## 
    ##  English   French    Hindi Mandarin 
    ##        2        1        1        2

``` r

cat("\nLanguage confidence levels:\n")
```

    ## 
    ## Language confidence levels:

``` r

print(table(coded_transcripts$language_confidence))
```

    ## 
    ##   high medium    low 
    ##      6      0      0

### Topics and themes

``` r

# Display topics for each transcript
coded_transcripts %>%
  select(.filename, main_topics) %>%
  kable(
    col.names = c("File", "Main Topics"),
    caption = "Topics Identified in Each Transcript"
  )
```

| File | Main Topics |
|:---|:---|
| F1523643-7930-4FCA-B0E1-1CB5AEBE6BF2.mp3 | societal tension, stress, negative news, impatience |
| harvard.wav | Smells, Food flavors, Health benefits, Culinary preferences |
| OSR_cn_000_0072_8k.wav | local observations, scenery, urban features, entertainment, nature |
| OSR_cn_000_0075_8k.wav | astronomy, nature, travel, seasons, landscapes |
| OSR_fr_000_0041_8k.wav | daily observations, human emotions, nature, idiomatic expressions, everyday situations |
| OSR_in_000_0064_8k.wav | personal characteristics, daily observations, advice, interpersonal interactions, hobbies |

Topics Identified in Each Transcript {.table}

### Detailed view of one transcript

Let’s examine the complete analysis for one transcript:

``` r

# Select the first transcript for detailed view
transcript_detail <- coded_transcripts[1, ]

cat("=== Detailed Analysis ===\n\n")
```

    ## === Detailed Analysis ===

``` r

cat("File:", transcript_detail$.filename, "\n\n")
```

    ## File: F1523643-7930-4FCA-B0E1-1CB5AEBE6BF2.mp3

``` r

cat("Language:", transcript_detail$language,
    "(", transcript_detail$language_confidence, "confidence )\n")
```

    ## Language: English ( 1 confidence )

``` r

cat("Speech type:", transcript_detail$speech_type, "\n")
```

    ## Speech type: 1

``` r

cat("Tone:", transcript_detail$tone, "\n")
```

    ## Tone: 5

``` r

cat("Sentiment:", transcript_detail$sentiment, "\n\n")
```

    ## Sentiment: 2

``` r

cat("Main topics:", transcript_detail$main_topics, "\n\n")
```

    ## Main topics: societal tension, stress, negative news, impatience

``` r

cat("Key phrases:", transcript_detail$key_phrases, "\n\n")
```

    ## Key phrases: tension in the air, stressed out, negative news, impatience of people

``` r

cat("Summary:", transcript_detail$summary, "\n\n")
```

    ## Summary: The speaker observes a pervasive tension in society, suggesting it stems from headlines, general stress, or negative news. This tension is evident in the impatience displayed by people.

``` r

cat("=== Original Transcription (first 300 chars) ===\n")
```

    ## === Original Transcription (first 300 chars) ===

``` r

cat(substr(transcriptions[1], 1, 300), "...\n")
```

    ## And this is such an important topic because I'm sure you've felt it. There's tension in the air, right? Everywhere you go. I don't know if it's the headlines or the fact that everybody is so stressed out or the news is so negative, but the impatience of people when they're waiting. ...

## Summary statistics

``` r

# Speech type distribution
cat("Speech types:\n")
```

    ## Speech types:

``` r

print(table(coded_transcripts$speech_type))
```

    ## 
    ## conversational  formal_speech        reading    spontaneous          other 
    ##              1              0              0              3              2

``` r

# Tone distribution
cat("\nTone distribution:\n")
```

    ## 
    ## Tone distribution:

``` r

print(table(coded_transcripts$tone))
```

    ## 
    ##         formal       informal        neutral      technical conversational 
    ##              0              0              5              0              1

``` r

# Sentiment distribution
cat("\nSentiment distribution:\n")
```

    ## 
    ## Sentiment distribution:

``` r

print(table(coded_transcripts$sentiment))
```

    ## 
    ## positive negative  neutral    mixed 
    ##        1        1        2        2

## Creating an audit trail

Document the complete analysis:

``` r

qlm_trail(coded_transcripts, path = "audio_analysis")
```

This creates two files:

- `audio_analysis.rds`: Complete trail object containing the coding run,
  codebook, and metadata
- `audio_analysis.qmd`: Quarto document with full audit trail
  documentation

## Summary

This example demonstrates:

1.  **Two-step workflow**: Transcription (Whisper) → Analysis (LLM)
2.  **Multilingual capability**: Whisper handles multiple languages
    automatically
3.  **Structured extraction**: Codebooks define what to extract from
    transcripts
4.  **Scalability**: Process multiple audio files in batch
5.  **Cost efficiency**: Whisper is significantly cheaper than human
    transcription
6.  **Reproducibility**: All steps are documented and can be replicated

This workflow enables researchers to analyze audio content at scale,
from political speeches to interviews to podcast episodes. The
combination of Whisper for transcription and LLMs for analysis provides
both accuracy and interpretability.
