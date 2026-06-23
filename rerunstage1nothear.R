library(readr)
library(dplyr)
library(stringr)
library(httr2)
library(jsonlite)

# ---- Setup ----
api_key  <- Sys.getenv("GEMINI_API_KEY")
model    <- "gemini-2.5-flash"
endpoint <- paste0("https://generativelanguage.googleapis.com/v1beta/models/",
                   model, ":generateContent")

# ---- Config (adjust filename if your not-heard long-form file is named differently) ----
input_file  <- "secretsnothear_long_form_sorted.csv"
output_file <- "stage1_open_codes_notheard_v2.csv"   # new file; v1 stays untouched

# ---- Load ----
long_df <- read_csv(input_file, locale = locale(encoding = "UTF-8")) %>%
  mutate(row_uid = row_number())

cat("Loaded", nrow(long_df), "responses.\n")

# ---- System prompt: one label per response, content-focused, with your example ----
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

# ---- API helper ----
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

# ---- Resume support ----
done_uids <- if (file.exists(output_file)) read_csv(output_file)$row_uid else integer(0)
to_do <- long_df %>% filter(!row_uid %in% done_uids)
cat("Done:", length(done_uids), "| Remaining:", nrow(to_do), "\n")

# ---- Loop with buffered save every 5 rows ----
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
    open_codes = result   # kept as "open_codes" so existing Stage 2 still works
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
cat("\n=== Result summary ===\n")
cat("Total coded:", nrow(result_df), "\n")
cat("Unique codes:", length(unique(result_df$open_codes)), "\n")
cat("NONE count:", sum(result_df$open_codes == "NONE"), "\n")
cat("\nTop 20 codes:\n")
print(result_df %>% count(open_codes, sort = TRUE) %>% head(20))