# a transcript prints a header and one line per element

    Code
      print(tr, width = 60)
    Output
      <qlm_transcript: 3 transcripts, 1 failed; model openai/gpt-4o-mini-transcribe>
      a: long long long long long long long long long long long...
      b: <NA: HTTP 500 Internal Server Error.>
      c: transcript of c

---

    Code
      print(tr, n = 1)
    Output
      <qlm_transcript: 3 transcripts, 1 failed; model openai/gpt-4o-mini-transcribe>
      a: long long long long long long long long long long long long long long long...
      # ... with 2 more

