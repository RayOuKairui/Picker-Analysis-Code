library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(httr2)
library(jsonlite)

# ---- Setup ----
api_key <- Sys.getenv("GEMINI_API_KEY")   # set with Sys.setenv(GEMINI_API_KEY = "your_key")
model   <- "gemini-2.5-flash"
endpoint <- paste0("https://generativelanguage.googleapis.com/v1beta/models/",
                   model, ":generateContent")

# ---- Load the long-form data ----
long_df <- read_csv("secretsnothear_long_form_sorted.csv",
                    locale = locale(encoding = "UTF-8"))

# System / task instruction for inductive open coding
system_prompt <- paste(
  "You are a qualitative coder performing inductive thematic analysis.",
  "Read the single survey response and assign 1 to 3 short thematic labels",
  "(2-4 words each) capturing its main ideas. Create labels that fit the",
  "content; do NOT use a fixed list. If the response is empty, 'N/A', or",
  "meaningless, return an empty list.",
  "Return ONLY valid JSON, no markdown, in this exact form:",
  '{"labels": ["label one", "label two"]}'
)

# ---- Function to code one response ----
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

# ---- Run over all rows, with caching + error handling ----
out_file <- "stage1_open_codesnothear.csv"

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