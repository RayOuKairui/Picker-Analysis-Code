library(readr)
library(dplyr)
library(stringr)
library(httr2)
library(jsonlite)
library(purrr)

# ---- Load Stage 1 output and rebuild unique_codes ----
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

api_key  <- Sys.getenv("GEMINI_API_KEY")
model    <- "gemini-2.5-pro"            # stronger model for this stage
endpoint <- paste0("https://generativelanguage.googleapis.com/v1beta/models/",
                   model, ":generateContent")

# Helper: one Gemini call returning parsed JSON
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

# ---- 2D. Sanity check ----
missing <- setdiff(unique_codes, code_map$open_code)
cat("Codes mapped:", nrow(code_map), "of", length(unique_codes), "\n")
cat("Unmapped:", length(missing), "\n")


rename_map_not_told <- c(
  "Strategic & Operational Intelligence" = "Confidential Business Strategy & Operations",
  "Personal Vulnerabilities"             = "Personal Life Struggles",
  "Interpersonal Dynamics & Conflict"    = "Workplace Relationships & Conflicts",
  "Premature Disclosures"                = "Advance Notice of Announcements",
  "Impending Job Loss"                   = "Layoffs & Terminations"
)
# (analogous map for not-heard)l

summary_df <- read_csv("stage2_nottoldtheme_summary.csv") %>%
  mutate(theme = recode(theme, !!!rename_map_not_told))
write_csv(summary_df, "stage2_nottoldtheme_summary.csv")

mapping_df <- read_csv("stage2_code_to_nottoldtheme_map.csv") %>%
  mutate(theme = recode(theme, !!!rename_map_not_told))
write_csv(mapping_df, "stage2_code_to_nottoldtheme_map.csv")
if (length(missing) > 0) head(missing, 20)