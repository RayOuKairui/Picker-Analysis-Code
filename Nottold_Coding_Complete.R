# =============================================================================
# NOT-TOLD SECRETS — COMPLETE INDUCTIVE THEME-CODING WORKFLOW
# -----------------------------------------------------------------------------
# Survey question coded here (per the Stage 2 prompt):
#   "What is a secret you told someone at work, even though you weren't
#    supposed to?"
#
# This is the consolidated, start-to-finish pipeline, merging:
#   - Stage 1 from NottoldThemeCode.R   (inductive open coding, 1-3 labels)
#   - Stage 2 from ClearThemeNottold.R  (theme consolidation, mapping, rename)
#
# RECONCILIATION: the original Stage 1 script wrote "stage1_open_codes.csv",
# but Stage 2 (and the actual saved data file) use "stage1_open_codesnottold.csv".
# This consolidated script standardizes on "stage1_open_codesnottold.csv" so the
# pipeline is coherent end-to-end and matches the existing data on disk.
#
# Pipeline:
#   Stage 1  Open coding   : 1-3 thematic labels per response (gemini-2.5-flash)
#               input : secretsnottold_long_form_sorted.csv
#               output: stage1_open_codesnottold.csv
#   Stage 2  Theme build   : consolidate codes -> themes, map each code to a
#                            theme, then apply a manual theme-rename pass
#                                                            (gemini-2.5-pro)
#               input : stage1_open_codesnottold.csv
#               output: stage2_nottoldtheme_summary.csv
#                       stage2_code_to_nottoldtheme_map.csv
#
# NOTE: Both stages call the Gemini API and cost money/time. Set GEMINI_API_KEY
#       before running. Stage 1 supports resume (skips rows already coded).
# =============================================================================

library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(httr2)
library(jsonlite)
library(purrr)


# #############################################################################
# STAGE 1 — INDUCTIVE OPEN CODING (1-3 labels per response)
# #############################################################################

# ---- Setup ----
api_key <- Sys.getenv("GEMINI_API_KEY")   # set with Sys.setenv(GEMINI_API_KEY = "your_key")
model   <- "gemini-2.5-flash"
endpoint <- paste0("https://generativelanguage.googleapis.com/v1beta/models/",
                   model, ":generateContent")

# ---- Load the long-form data (one survey response per row) ----
long_df <- read_csv("secretsnottold_long_form_sorted.csv",
                    locale = locale(encoding = "UTF-8"))

# ---- System / task instruction for inductive open coding ----
# (Verbatim — this prompt defines the coding output and must not be altered.)
system_prompt <- paste(
  "You are a qualitative coder performing inductive thematic analysis.",
  "Read the single survey response and assign 1 to 3 short thematic labels",
  "(2-4 words each) capturing its main ideas. Create labels that fit the",
  "content; do NOT use a fixed list. If the response is empty, 'N/A', or",
  "meaningless, return an empty list.",
  "Return ONLY valid JSON, no markdown, in this exact form:",
  '{"labels": ["label one", "label two"]}'
)

# ---- Function to code one response (returns "; "-joined labels, or NA) ----
code_one <- function(response_text) {
  body <- list(
    system_instruction = list(parts = list(list(text = system_prompt))),
    contents = list(list(parts = list(list(text = response_text)))),
    generationConfig = list(
      temperature = 0.2,
      responseMimeType = "application/json"   # forces clean JSON output
    )
  )

  resp <- request(endpoint) |>
    req_url_query(key = api_key) |>
    req_headers("Content-Type" = "application/json") |>
    req_body_json(body) |>
    req_retry(max_tries = 4, backoff = ~ 2 ^ .x) |>   # exponential backoff
    req_perform()

  parsed <- resp |> resp_body_json()
  txt <- parsed$candidates[[1]]$content$parts[[1]]$text
  labels <- fromJSON(txt)$labels
  if (is.null(labels) || length(labels) == 0) return(NA_character_)
  paste(labels, collapse = "; ")   # store as a single "; "-separated string
}

# ---- Run over all rows, with resume support + per-row error handling ----
out_file <- "stage1_open_codesnottold.csv"   # standardized name (see header note)

# resume support: skip rows already done
done_ids <- if (file.exists(out_file)) read_csv(out_file)$ID else integer(0)
to_do <- long_df %>% filter(!ID %in% done_ids)

for (i in seq_len(nrow(to_do))) {
  row <- to_do[i, ]
  result <- tryCatch(
    code_one(row$response),
    error = function(e) paste0("ERROR: ", conditionMessage(e))
  )

  write_csv(
    tibble(ID = row$ID, response = row$response, open_codes = result),
    out_file,
    append = file.exists(out_file)
  )

  Sys.sleep(0.3)   # gentle pacing for rate limits
  if (i %% 50 == 0) message("Coded ", i, " of ", nrow(to_do))
}

message("Done. Results in ", out_file)


# #############################################################################
# STAGE 2 — THEME CONSOLIDATION, MAPPING & RENAME
# #############################################################################

# ---- Load Stage 1 output and rebuild the sorted set of unique open codes ----
stage1 <- read_csv("stage1_open_codesnottold.csv", locale = locale(encoding = "UTF-8"))

unique_codes <- stage1 %>%
  filter(!is.na(open_codes), open_codes != "",
         !str_starts(open_codes, "ERROR")) %>%
  pull(open_codes) %>%
  str_split("; ") %>%
  unlist() %>%
  str_trim() %>%
  .[. != ""] %>%
  unique() %>%
  sort()

cat("Loaded", length(unique_codes), "unique open codes.\n")

# ---- Setup (stronger model for this stage) ----
api_key  <- Sys.getenv("GEMINI_API_KEY")
model    <- "gemini-2.5-pro"            # stronger model for this stage
endpoint <- paste0("https://generativelanguage.googleapis.com/v1beta/models/",
                   model, ":generateContent")

# ---- Helper: one Gemini call returning parsed JSON + finish reason ----
gemini_json <- function(prompt, max_tokens = 16384) {
  body <- list(
    contents = list(list(parts = list(list(text = prompt)))),
    generationConfig = list(
      temperature = 0.2,
      responseMimeType = "application/json",
      maxOutputTokens = max_tokens
    )
  )
  resp <- request(endpoint) |>
    req_url_query(key = api_key) |>
    req_headers("Content-Type" = "application/json") |>
    req_body_json(body) |>
    req_retry(max_tries = 4, backoff = ~ 2 ^ .x) |>
    req_timeout(120) |>
    req_perform()
  parsed <- resp |> resp_body_json()
  txt <- parsed$candidates[[1]]$content$parts[[1]]$text
  list(json = fromJSON(txt, simplifyDataFrame = FALSE),
       finish = parsed$candidates[[1]]$finishReason)
}

# ---- 2A. Chunk the codes and propose candidate themes per chunk ----
chunks <- split(unique_codes, ceiling(seq_along(unique_codes) / 300))
cat("Split into", length(chunks), "chunks.\n")

chunk_themes <- list()
for (i in seq_along(chunks)) {
  prompt <- paste0(
    "You are a qualitative researcher. Below is a batch of open codes from ",
    "survey responses towards the question: What is a secret you told someone at work, even though you weren't supposed to?. Propose a ",
    "set of coherent themes that capture this batch. For each theme give a ",
    "short name and a one-sentence definition. Aim for 8 to 15 themes towards the coded secrets.\n\n",
    "Return ONLY valid JSON:\n",
    '{"themes": [{"name": "...", "definition": "..."}]}\n\n',
    "Codes:\n", paste(chunks[[i]], collapse = "\n")
  )
  res <- gemini_json(prompt, max_tokens = 8192)
  chunk_themes[[i]] <- res$json$themes
  cat("Chunk", i, "->", length(res$json$themes), "themes (finish:", res$finish, ")\n")
  Sys.sleep(1)
}

# ---- 2B. Merge candidate themes into a final consolidated set ----
all_candidates <- bind_rows(lapply(chunk_themes, function(t) {
  tibble(name = sapply(t, `[[`, "name"),
         definition = sapply(t, `[[`, "definition"))
}))

merge_prompt <- paste0(
  "Below are candidate themes proposed across multiple batches of codes from ",
  "the same study. Merge near-duplicate themes into a single coherent final ",
  "set. Aim for 10 to 20 final themes. For each, give a short name and a ",
  "one-sentence definition.\n\n",
  "Return ONLY valid JSON:\n",
  '{"themes": [{"name": "...", "definition": "..."}]}\n\n',
  "Candidate themes:\n",
  paste0("- ", all_candidates$name, ": ", all_candidates$definition, collapse = "\n")
)

final <- gemini_json(merge_prompt, max_tokens = 8192)
final_themes <- tibble(
  theme      = sapply(final$json$themes, `[[`, "name"),
  definition = sapply(final$json$themes, `[[`, "definition")
)
write_csv(final_themes, "stage2_nottoldtheme_summary.csv")
cat("Final theme count:", nrow(final_themes), "(finish:", final$finish, ")\n")
print(final_themes)

# ---- 2C. Map every original code to one final theme (chunked) ----
themes_block <- paste0("- ", final_themes$theme, ": ", final_themes$definition,
                       collapse = "\n")

map_chunks <- split(unique_codes, ceiling(seq_along(unique_codes) / 150))
code_map_list <- list()

for (i in seq_along(map_chunks)) {
  prompt <- paste0(
    "Assign each open code below to exactly one of the themes listed. Use ",
    "theme names EXACTLY as written. If a code does not fit any theme well, ",
    "assign it to 'Other'.\n\n",
    "Themes:\n", themes_block, "\n- Other: codes that do not fit elsewhere\n\n",
    "Return ONLY valid JSON:\n",
    '{"assignments": [{"code": "...", "theme": "..."}]}\n\n',
    "Codes to assign:\n", paste(map_chunks[[i]], collapse = "\n")
  )
  res <- gemini_json(prompt, max_tokens = 16384)
  code_map_list[[i]] <- bind_rows(lapply(res$json$assignments, function(a) {
    tibble(open_code = a$code, theme = a$theme)
  }))
  cat("Mapped chunk", i, "of", length(map_chunks),
      "(finish:", res$finish, ")\n")
  Sys.sleep(1)
}

code_map <- bind_rows(code_map_list) %>% distinct(open_code, .keep_all = TRUE)
write_csv(code_map, "stage2_code_to_nottoldtheme_map.csv")

# ---- 2D. Sanity check: confirm every code was mapped ----
missing <- setdiff(unique_codes, code_map$open_code)
cat("Codes mapped:", nrow(code_map), "of", length(unique_codes), "\n")
cat("Unmapped:", length(missing), "\n")

# ---- 2E. Manual theme-rename pass ----
# Recode the auto-generated theme names to the researcher's final wording, and
# rewrite both the summary and the code->theme map so they stay consistent.
rename_map_not_told <- c(
  "Strategic & Operational Intelligence" = "Confidential Business Strategy & Operations",
  "Personal Vulnerabilities"             = "Personal Life Struggles",
  "Interpersonal Dynamics & Conflict"    = "Workplace Relationships & Conflicts",
  "Premature Disclosures"                = "Advance Notice of Announcements",
  "Impending Job Loss"                   = "Layoffs & Terminations"
)
# (An analogous rename map exists for the not-heard side in its own script.)

summary_df <- read_csv("stage2_nottoldtheme_summary.csv") %>%
  mutate(theme = recode(theme, !!!rename_map_not_told))
write_csv(summary_df, "stage2_nottoldtheme_summary.csv")

mapping_df <- read_csv("stage2_code_to_nottoldtheme_map.csv") %>%
  mutate(theme = recode(theme, !!!rename_map_not_told))
write_csv(mapping_df, "stage2_code_to_nottoldtheme_map.csv")
if (length(missing) > 0) head(missing, 20)
