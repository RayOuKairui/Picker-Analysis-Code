library(readr)
library(dplyr)
library(stringr)
library(httr2)
library(jsonlite)
library(purrr)

# ---- Setup ----
api_key  <- Sys.getenv("GEMINI_API_KEY")
model    <- "gemini-2.5-pro"            # stronger model for consolidation
endpoint <- paste0("https://generativelanguage.googleapis.com/v1beta/models/",
                   model, ":generateContent")

# ---- Config ----
stage1_file       <- "stage1_open_codes_notheard_v2.csv"
summary_outfile   <- "stage2_notheartheme_summary_v2.csv"
mapping_outfile   <- "stage2_code_to_notheartheme_map_v2.csv"

# ---- Load Stage 1 v2 and extract unique codes ----
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

# ---- Gemini helper ----
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

# ---- 2A. Build candidate themes (chunked if needed) ----
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
  # Chunked: propose per chunk, then merge
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

write_csv(candidate_themes %>% rename(theme = name), summary_outfile)
print(candidate_themes)

# ---- 2B. Map every original code to one final theme (chunked) ----
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

# Guard: any theme name Gemini reworded -> Other
valid_themes <- c(candidate_themes$name, "Other")
code_map <- code_map %>%
  mutate(theme = if_else(theme %in% valid_themes, theme, "Other"))

write_csv(code_map, mapping_outfile)

# ---- 2C. Sanity check ----
missing <- setdiff(unique_codes, code_map$open_code)
cat("\n=== Stage 2 v2 Summary ===\n")
cat("Codes mapped:", nrow(code_map), "of", length(unique_codes), "\n")
cat("Unmapped:    ", length(missing), "\n")
if (length(missing) >