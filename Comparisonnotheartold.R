# =============================================================================
# CROSS-CODEBOOK COMPARISON — Not Told vs. Not Heard themes
# -----------------------------------------------------------------------------
# Takes the two inductively-built theme codebooks (one for the "not told" secrets
# and one for the "not heard" secrets) and asks Gemini to decide which themes are
# CONCEPTUALLY COMMON across the two sides and which are UNIQUE to one side, then
# writes the comparison to a multi-sheet Excel workbook.
#
# Inputs : stage2_code_to_nottoldtheme_map.csv      (Not Told codebook)
#          stage2_code_to_notheartheme_map_v2.csv   (Not Heard codebook)
# Output : themes_common_vs_unique.xlsx
#
# NOTE: Comments/section headers were added for readability. The only content
#       change is a correction to the NOT TOLD question wording in the prompt
#       (see Stage "Prompt" below) to the confirmed-correct framing.
# =============================================================================

# ---- Packages ----
library(readr)     # read_csv() — load the two codebook CSVs
library(dplyr)     # bind_rows(), tibble(), %>% for assembling result tables
library(stringr)   # string helpers
library(httr2)     # HTTP client for the Gemini API call
library(jsonlite)  # fromJSON() — parse the model's JSON response
library(openxlsx)  # createWorkbook()/writeData()/saveWorkbook() — Excel output

# ---- Setup ----
api_key  <- Sys.getenv("GEMINI_API_KEY")
model    <- "gemini-2.5-pro"
endpoint <- paste0("https://generativelanguage.googleapis.com/v1beta/models/",
                   model, ":generateContent")

# ---- Config: adjust filenames to your latest versions ----
nottold_summary  <- "stage2_code_to_nottoldtheme_map.csv"
notheard_summary <- "stage2_code_to_notheartheme_map_v2.csv"
output_excel     <- "themes_common_vs_unique.xlsx"

# ---- Load both theme codebooks (each: open_code -> theme, with definitions) ----
nottold  <- read_csv(nottold_summary,  locale = locale(encoding = "UTF-8"))
notheard <- read_csv(notheard_summary, locale = locale(encoding = "UTF-8"))

cat("Not Told themes: ", nrow(nottold),  "\n")
cat("Not Heard themes:", nrow(notheard), "\n")

# Helper: render a codebook as a labeled "- theme: definition" block for the prompt.
fmt <- function(df, label) {
  paste0("=== ", label, " ===\n",
         paste0("- ", df$theme, ": ", df$definition, collapse = "\n"))
}

# ---- Prompt ----
# Instructs the model to match themes by CONTENT (not literal name), classify each
# into common-vs-unique, and return strict JSON. Both codebooks are appended.
prompt <- paste0(
  "You are a qualitative researcher comparing two thematic codebooks from a ",
  "single study. The same participants answered two questions:\n",
  "1. NOT TOLD: \"What is a secret you told someone at work, even though you weren't supposed to?\"\n",
  "2. NOT HEARD: \"What is a secret someone TOLD YOU at work that you wish ",
  "they hadn't?\"\n\n",
  "Each side was inductively themed separately, so equivalent content may ",
  "have slightly different theme names. Your task: identify which themes are ",
  "CONCEPTUALLY COMMON across the two codebooks (same content area, even if ",
  "named differently) and which are UNIQUE to one side.\n\n",
  "Rules:\n",
  "- Match by content, not by literal name. \"Compensation & Financial ",
  "Disclosures\" on one side and \"Salary and Pay\" on the other should be ",
  "matched as common.\n",
  "- A theme is UNIQUE only if no theme on the other side covers the same ",
  "content area.\n",
  "- Every theme from both codebooks must appear exactly once in your ",
  "output, either in the 'common' list or in one of the 'unique' lists.\n",
  "- For each common pair, briefly note whether the framing is similar or ",
  "shifts between the two sides.\n",
  "- For each unique theme, briefly note why it likely appears only on that ",
  "side (one sentence).\n\n",
  "Return ONLY valid JSON in this exact form:\n",
  '{"common": [{"content_area": "...", "not_told_theme": "...", ',
  '"not_heard_theme": "...", "note": "..."}], ',
  '"unique_to_not_told": [{"theme": "...", "note": "..."}], ',
  '"unique_to_not_heard": [{"theme": "...", "note": "..."}]}\n\n',
  fmt(nottold,  "NOT TOLD CODEBOOK"),  "\n\n",
  fmt(notheard, "NOT HEARD CODEBOOK")
)

# ---- Gemini call (single request; low temperature, JSON-only, with retries) ----
body <- list(
  contents = list(list(parts = list(list(text = prompt)))),
  generationConfig = list(
    temperature      = 0.2,
    responseMimeType = "application/json",
    maxOutputTokens  = 8192
  )
)

resp <- request(endpoint) |>
  req_url_query(key = api_key) |>
  req_headers("Content-Type" = "application/json") |>
  req_body_json(body) |>
  req_retry(max_tries = 4, backoff = ~ 2 ^ .x) |>
  req_timeout(180) |>
  req_perform()

# Extract and parse the JSON text the model returned.
parsed <- resp |> resp_body_json()
txt    <- parsed$candidates[[1]]$content$parts[[1]]$text
out    <- fromJSON(txt, simplifyDataFrame = FALSE)
cat("Finish reason:", parsed$candidates[[1]]$finishReason, "\n")

# Replace any NULL JSON field with "" so the tibbles below stay rectangular.
null_safe <- function(x) if (is.null(x)) "" else x

# ---- Assemble result tables from the parsed JSON ----
# Common themes: matched pairs across the two codebooks.
common_df <- bind_rows(lapply(out$common, function(r) tibble(
  content_area    = null_safe(r$content_area),
  not_told_theme  = null_safe(r$not_told_theme),
  not_heard_theme = null_safe(r$not_heard_theme),
  note            = null_safe(r$note)
)))

# Themes unique to the Not Told side.
unique_nottold_df <- bind_rows(lapply(out$unique_to_not_told, function(r) tibble(
  theme = null_safe(r$theme),
  note  = null_safe(r$note)
)))

# Themes unique to the Not Heard side.
unique_notheard_df <- bind_rows(lapply(out$unique_to_not_heard, function(r) tibble(
  theme = null_safe(r$theme),
  note  = null_safe(r$note)
)))

# ---- Coverage check: did every input theme make it into the output? ----
all_returned <- c(common_df$not_told_theme, common_df$not_heard_theme,
                  unique_nottold_df$theme, unique_notheard_df$theme)
all_returned <- all_returned[all_returned != ""]
all_input    <- c(nottold$theme, notheard$theme)
missing      <- setdiff(all_input, all_returned)

cat("\n=== Coverage ===\n")
cat("Common pairs:        ", nrow(common_df), "\n")
cat("Unique to Not Told:  ", nrow(unique_nottold_df), "\n")
cat("Unique to Not Heard: ", nrow(unique_notheard_df), "\n")
if (length(missing) > 0) {
  cat("WARNING - themes missing from output:\n")
  print(missing)
} else {
  cat("All themes accounted for.\n")
}

# Print the three result tables to the console.
cat("\n=== Common ===\n");           print(common_df)
cat("\n=== Unique to Not Told ===\n"); print(unique_nottold_df)
cat("\n=== Unique to Not Heard ===\n"); print(unique_notheard_df)

# ---- Excel output: one sheet per result table, plus both source codebooks ----
wb <- createWorkbook()
addWorksheet(wb, "Common Themes")
writeData(wb, "Common Themes", common_df)
addWorksheet(wb, "Unique to Not Told")
writeData(wb, "Unique to Not Told", unique_nottold_df)
addWorksheet(wb, "Unique to Not Heard")
writeData(wb, "Unique to Not Heard", unique_notheard_df)
addWorksheet(wb, "Not Told Codebook")
writeData(wb, "Not Told Codebook", nottold)
addWorksheet(wb, "Not Heard Codebook")
writeData(wb, "Not Heard Codebook", notheard)
saveWorkbook(wb, output_excel, overwrite = TRUE)
cat("\nSaved:", output_excel, "\n")
