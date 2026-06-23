library(readr)
library(dplyr)
library(stringr)
library(httr2)
library(jsonlite)
library(openxlsx)

# ---- Setup ----
api_key  <- Sys.getenv("GEMINI_API_KEY")
model    <- "gemini-2.5-pro"
endpoint <- paste0("https://generativelanguage.googleapis.com/v1beta/models/",
                   model, ":generateContent")

# ---- Config: adjust filenames to your latest versions ----
nottold_summary  <- "stage2_code_to_nottoldtheme_map.csv"
notheard_summary <- "stage2_code_to_notheartheme_map_v2.csv"
output_excel     <- "themes_common_vs_unique.xlsx"

# ---- Load both theme codebooks ----
nottold  <- read_csv(nottold_summary,  locale = locale(encoding = "UTF-8"))
notheard <- read_csv(notheard_summary, locale = locale(encoding = "UTF-8"))

cat("Not Told themes: ", nrow(nottold),  "\n")
cat("Not Heard themes:", nrow(notheard), "\n")

fmt <- function(df, label) {
  paste0("=== ", label, " ===\n",
         paste0("- ", df$theme, ": ", df$definition, collapse = "\n"))
}

# ---- Prompt ----
prompt <- paste0(
  "You are a qualitative researcher comparing two thematic codebooks from a ",
  "single study. The same participants answered two questions:\n",
  "1. NOT TOLD: \"What is a secret you have NOT TOLD anyone at work?\"\n",
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

# ---- Gemini call ----
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

parsed <- resp |> resp_body_json()
txt    <- parsed$candidates[[1]]$content$parts[[1]]$text
out    <- fromJSON(txt, simplifyDataFrame = FALSE)
cat("Finish reason:", parsed$candidates[[1]]$finishReason, "\n")

null_safe <- function(x) if (is.null(x)) "" else x

common_df <- bind_rows(lapply(out$common, function(r) tibble(
  content_area    = null_safe(r$content_area),
  not_told_theme  = null_safe(r$not_told_theme),
  not_heard_theme = null_safe(r$not_heard_theme),
  note            = null_safe(r$note)
)))

unique_nottold_df <- bind_rows(lapply(out$unique_to_not_told, function(r) tibble(
  theme = null_safe(r$theme),
  note  = null_safe(r$note)
)))

unique_notheard_df <- bind_rows(lapply(out$unique_to_not_heard, function(r) tibble(
  theme = null_safe(r$theme),
  note  = null_safe(r$note)
)))

# ---- Coverage check: did every theme make it into the output? ----
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

cat("\n=== Common ===\n");           print(common_df)
cat("\n=== Unique to Not Told ===\n"); print(unique_nottold_df)
cat("\n=== Unique to Not Heard ===\n"); print(unique_notheard_df)

# ---- Excel output ----
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