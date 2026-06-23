# =============================================================================
# NOT-HEARD SECRETS — COMPLETE INDUCTIVE THEME-CODING WORKFLOW
# -----------------------------------------------------------------------------
# Survey question coded here:
#   "Now, what is a secret that someone told you at work that you wish they
#    didn't tell you?"
#
# This is the consolidated, start-to-finish pipeline. It replaces the earlier
# piecemeal scripts:
#   - Stage 1 uses the IMPROVED v2 open-coding logic (from rerunstage1nothear.R),
#     not the original NothearThemeCode.R (1-3 labels), which is obsolete.
#   - Stage 2 uses the theme-consolidation logic from ClearThemeNothear.R, whose
#     final sanity-check line was truncated in the original; it is completed here
#     (clearly marked below).
#
# Pipeline:
#   Stage 1  Open coding   : one theme label per response  (gemini-2.5-flash)
#               input : secretsnothear_long_form_sorted.csv
#               output: stage1_open_codes_notheard_v2.csv
#   Stage 2  Theme build   : consolidate codes -> themes, map each code to a
#                            theme                          (gemini-2.5-pro)
#               input : stage1_open_codes_notheard_v2.csv
#               output: stage2_notheartheme_summary_v2.csv
#                       stage2_code_to_notheartheme_map_v2.csv
#
# NOTE: Both stages call the Gemini API and cost money/time. Set GEMINI_API_KEY
#       before running. Each stage supports resume, so an interrupted run can be
#       restarted without redoing finished rows.
# =============================================================================

library(readr)
library(dplyr)
library(stringr)
library(httr2)
library(jsonlite)
library(purrr)


# #############################################################################
# STAGE 1 — INDUCTIVE OPEN CODING (one theme per response)
# #############################################################################

# ---- Setup ----
api_key  <- Sys.getenv("GEMINI_API_KEY")
model    <- "gemini-2.5-flash"
endpoint <- paste0("https://generativelanguage.googleapis.com/v1beta/models/",
                   model, ":generateContent")

# ---- Config (adjust filename if your not-heard long-form file is named differently) ----
input_file  <- "secretsnothear_long_form_sorted.csv"
output_file <- "stage1_open_codes_notheard_v2.csv"   # new file; v1 stays untouched

# ---- Load: one row per response, add a stable row id for resume/merge ----
long_df <- read_csv(input_file, locale = locale(encoding = "UTF-8")) %>%
  mutate(row_uid = row_number())

cat("Loaded", nrow(long_df), "responses.\n")

# ---- System prompt: one label per response, content-focused, with examples ----
# (Verbatim — this prompt defines the coding output and must not be altered.)
system_prompt <- paste(
  "You are a qualitative coder. Participants were asked:",
  "\"Now, what is a secret that someone told you at work that you wish they didn't tell you?\"",
  "",
  "For each response, identify the SINGLE most important theme — the core",
  "content of the secret. Output exactly ONE short theme label (2-5 words).",
  "",
  "Rules:",
  "1. Focus on WHAT THE SECRET IS ABOUT. Ignore the participant's emotional",
  "   or evaluative commentary (e.g., \"(I really didn't care)\", \"it was",
  "   awful\", \"ugh so annoying\"), UNLESS the emotion itself is part of",
  "   the secret (e.g., a coworker confessing they hate their boss).",
  "2. Generalize where appropriate — specific roles like \"previous boss\"",
  "   or \"my manager\" can become \"coworker\" when the relationship is",
  "   secondary to the content.",
  "3. For responses that are N/A, blank, gibberish, incomplete, dismissive,",
  "   or protest answers, return the literal string \"NONE\".",
  "",
  "Examples:",
  "- \"my previous boss's personal problems/secrets in life (i really didn't",
  "  care)\" -> \"coworker's personal problem/secrets\"",
  "- \"basically anything personal. i don't care.\" -> \"NONE\"",
  "- \"my coworker told me she's having an affair with our manager\" ->",
  "  \"workplace affair\"",
  "- \"asdf\" -> \"NONE\"",
  "",
  "Return ONLY valid JSON in this exact form:",
  '{"theme": "..."}',
  sep = "\n"
)

# ---- API helper: code a single response, return its one theme string ----
code_one <- function(response_text) {
  body <- list(
    system_instruction = list(parts = list(list(text = system_prompt))),
    contents = list(list(parts = list(list(text = response_text)))),
    generationConfig = list(
      temperature      = 0.2,
      responseMimeType = "application/json"
    )
  )
  resp <- request(endpoint) |>
    req_url_query(key = api_key) |>
    req_headers("Content-Type" = "application/json") |>
    req_body_json(body) |>
    req_retry(max_tries = 4, backoff = ~ 2 ^ .x) |>
    req_perform()
  parsed <- resp |> resp_body_json()
  txt   <- parsed$candidates[[1]]$content$parts[[1]]$text
  theme <- fromJSON(txt)$theme
  if (is.null(theme) || length(theme) == 0 || theme == "") return("NONE")
  theme
}

# ---- Resume support: skip rows already coded in a previous run ----
done_uids <- if (file.exists(output_file)) read_csv(output_file)$row_uid else integer(0)
to_do <- long_df %>% filter(!row_uid %in% done_uids)
cat("Done:", length(done_uids), "| Remaining:", nrow(to_do), "\n")

# ---- Loop with buffered save every 5 rows (limits data loss on interruption) ----
flush_every <- 5
buffer <- list()
flush_buffer <- function(buf, file) {
  if (length(buf) == 0) return(invisible())
  write_csv(bind_rows(buf), file, append = file.exists(file))
}

for (i in seq_len(nrow(to_do))) {
  row <- to_do[i, ]
  clean <- str_trim(str_replace_all(row$response, "[\r\n]+", " "))

  # Pre-filter obvious N/A — no API call needed
  result <- if (is.na(clean) || clean == "" || clean == "N/A") {
    "NONE"
  } else {
    tryCatch(
      code_one(clean),
      error = function(e) paste0("ERROR: ", conditionMessage(e))
    )
  }

  buffer[[length(buffer) + 1]] <- tibble(
    row_uid   = row$row_uid,
    ID        = row$ID,
    response  = row$response,
    open_codes = result   # kept as "open_codes" so Stage 2 below still works
  )

  if (length(buffer) >= flush_every) {
    flush_buffer(buffer, output_file)
    buffer <- list()
    if (i %% 50 == 0) message("Coded ", i, " of ", nrow(to_do))
  }
  Sys.sleep(0.3)
}

flush_buffer(buffer, output_file)
message("Done. Results in ", output_file)

# ---- Quick post-run sanity check ----
result_df <- read_csv(output_file, locale = locale(encoding = "UTF-8"))
cat("\n=== Stage 1 Result summary ===\n")
cat("Total coded:", nrow(result_df), "\n")
cat("Unique codes:", length(unique(result_df$open_codes)), "\n")
cat("NONE count:", sum(result_df$open_codes == "NONE"), "\n")
cat("\nTop 20 codes:\n")
print(result_df %>% count(open_codes, sort = TRUE) %>% head(20))


# #############################################################################
# STAGE 2 — THEME CONSOLIDATION & CODE-TO-THEME MAPPING
# #############################################################################

# ---- Setup (stronger model for consolidation) ----
api_key  <- Sys.getenv("GEMINI_API_KEY")
model    <- "gemini-2.5-pro"            # stronger model for consolidation
endpoint <- paste0("https://generativelanguage.googleapis.com/v1beta/models/",
                   model, ":generateContent")

# ---- Config: read Stage 1 output, write the theme summary + code->theme map ----
stage1_file       <- "stage1_open_codes_notheard_v2.csv"
summary_outfile   <- "stage2_notheartheme_summary_v2.csv"
mapping_outfile   <- "stage2_code_to_notheartheme_map_v2.csv"

# ---- Load Stage 1 and extract the sorted set of unique open codes ----
stage1 <- read_csv(stage1_file, locale = locale(encoding = "UTF-8"))

unique_codes <- stage1 %>%
  filter(!is.na(open_codes), open_codes != "", open_codes != "NONE",
         !str_starts(open_codes, "ERROR")) %>%
  pull(open_codes) %>%
  str_split("; ") %>%                # safe even for single labels
  unlist() %>%
  str_trim() %>%
  .[. != ""] %>%
  unique() %>%
  sort()

cat("Unique codes to consolidate:", length(unique_codes), "\n")

# ---- Gemini helper (returns parsed JSON + finish reason) ----
gemini_json <- function(prompt, max_tokens = 16384) {
  body <- list(
    contents = list(list(parts = list(list(text = prompt)))),
    generationConfig = list(
      temperature      = 0.2,
      responseMimeType = "application/json",
      maxOutputTokens  = max_tokens
    )
  )
  resp <- request(endpoint) |>
    req_url_query(key = api_key) |>
    req_headers("Content-Type" = "application/json") |>
    req_body_json(body) |>
    req_retry(max_tries = 4, backoff = ~ 2 ^ .x) |>
    req_timeout(180) |>
    req_perform()
  parsed <- resp |> resp_body_json()
  txt <- parsed$candidates[[1]]$content$parts[[1]]$text
  list(json   = fromJSON(txt, simplifyDataFrame = FALSE),
       finish = parsed$candidates[[1]]$finishReason)
}

# ---- 2A. Build candidate themes (single pass if <=400 codes, else chunk+merge) ----
context_line <- paste(
  "These codes describe secrets that someone told a participant at work that",
  "the participant wished had not been told to them."
)

if (length(unique_codes) <= 400) {
  # Single-pass theme generation
  cat("Running single-pass theme generation...\n")
  prompt <- paste0(
    "You are a qualitative researcher. ", context_line, " Below is the full ",
    "list of open codes. Propose a coherent set of themes that captures them. ",
    "For each theme give a short name and a one-sentence definition. Aim for ",
    "10 to 18 themes total.\n\n",
    "Return ONLY valid JSON:\n",
    '{"themes": [{"name": "...", "definition": "..."}]}\n\n',
    "Codes:\n", paste(unique_codes, collapse = "\n")
  )
  res <- gemini_json(prompt, 16384)
  candidate_themes <- tibble(
    name       = sapply(res$json$themes, `[[`, "name"),
    definition = sapply(res$json$themes, `[[`, "definition")
  )
  cat("Proposed", nrow(candidate_themes), "themes (finish:", res$finish, ")\n")

} else {
  # Chunked: propose per chunk, then merge near-duplicates into one final set
  chunks <- split(unique_codes, ceiling(seq_along(unique_codes) / 300))
  cat("Chunking into", length(chunks), "batches for theme proposal...\n")

  chunk_themes <- list()
  for (i in seq_along(chunks)) {
    prompt <- paste0(
      "You are a qualitative researcher. ", context_line, " Below is a batch ",
      "of open codes. Propose coherent themes that capture this batch. For ",
      "each, give a short name and a one-sentence definition. Aim for 8 to ",
      "15 themes.\n\n",
      "Return ONLY valid JSON:\n",
      '{"themes": [{"name": "...", "definition": "..."}]}\n\n',
      "Codes:\n", paste(chunks[[i]], collapse = "\n")
    )
    res <- gemini_json(prompt, 8192)
    chunk_themes[[i]] <- res$json$themes
    cat("  Chunk", i, "of", length(chunks), "->",
        length(res$json$themes), "themes\n")
    Sys.sleep(1)
  }

  all_candidates <- bind_rows(lapply(chunk_themes, function(t) {
    tibble(name = sapply(t, `[[`, "name"),
           definition = sapply(t, `[[`, "definition"))
  }))

  cat("Merging", nrow(all_candidates), "candidate themes...\n")
  merge_prompt <- paste0(
    "Below are candidate themes proposed across multiple batches of codes ",
    "from the same study. ", context_line, " Merge near-duplicate themes ",
    "into a single coherent final set. Aim for 10 to 18 final themes. For ",
    "each, give a short name and a one-sentence definition.\n\n",
    "Return ONLY valid JSON:\n",
    '{"themes": [{"name": "...", "definition": "..."}]}\n\n',
    "Candidate themes:\n",
    paste0("- ", all_candidates$name, ": ", all_candidates$definition,
           collapse = "\n")
  )
  final <- gemini_json(merge_prompt, 8192)
  candidate_themes <- tibble(
    name       = sapply(final$json$themes, `[[`, "name"),
    definition = sapply(final$json$themes, `[[`, "definition")
  )
  cat("Final theme count:", nrow(candidate_themes),
      "(finish:", final$finish, ")\n")
}

# Save the final theme summary (one row per theme: theme name + definition).
write_csv(candidate_themes %>% rename(theme = name), summary_outfile)
print(candidate_themes)

# ---- 2B. Map every original code to exactly one final theme (chunked) ----
themes_block <- paste0("- ", candidate_themes$name, ": ",
                       candidate_themes$definition, collapse = "\n")

map_chunks <- split(unique_codes, ceiling(seq_along(unique_codes) / 150))
code_map_list <- list()

for (i in seq_along(map_chunks)) {
  prompt <- paste0(
    "Assign each open code below to exactly one of the themes listed. Use ",
    "theme names EXACTLY as written. If a code does not fit any theme well, ",
    "assign it to 'Other'.\n\n",
    "Themes:\n", themes_block,
    "\n- Other: codes that do not fit any theme above\n\n",
    "Return ONLY valid JSON:\n",
    '{"assignments": [{"code": "...", "theme": "..."}]}\n\n',
    "Codes to assign:\n", paste(map_chunks[[i]], collapse = "\n")
  )
  res <- gemini_json(prompt, 16384)
  code_map_list[[i]] <- bind_rows(lapply(res$json$assignments, function(a) {
    tibble(open_code = a$code, theme = a$theme)
  }))
  cat("Mapped chunk", i, "of", length(map_chunks),
      "(finish:", res$finish, ")\n")
  Sys.sleep(1)
}

code_map <- bind_rows(code_map_list) %>% distinct(open_code, .keep_all = TRUE)

# Guard: if Gemini reworded a theme name, snap it back to 'Other'
valid_themes <- c(candidate_themes$name, "Other")
code_map <- code_map %>%
  mutate(theme = if_else(theme %in% valid_themes, theme, "Other"))

write_csv(code_map, mapping_outfile)

# ---- 2C. Sanity check: confirm every code was mapped ----
missing <- setdiff(unique_codes, code_map$open_code)
cat("\n=== Stage 2 v2 Summary ===\n")
cat("Codes mapped:", nrow(code_map), "of", length(unique_codes), "\n")
cat("Unmapped:    ", length(missing), "\n")
# (Completes the line that was truncated in the original ClearThemeNothear.R.)
if (length(missing) > 0) print(head(missing, 20))
