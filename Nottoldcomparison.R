library(readr)
library(dplyr)
library(stringr)
library(httr2)
library(jsonlite)
library(openxlsx)

# ---- Setup ----
api_key  <- Sys.getenv("GEMINI_API_KEY")
model    <- "gemini-2.5-flash"
endpoint <- paste0("https://generativelanguage.googleapis.com/v1beta/models/",
                   model, ":generateContent")

# ---- The 20 categories from Category_wording.docx ----
categories <- tribble(
  ~name, ~definition,
  "Salary and Pay",                                 "Pay levels, salary ranges, or pay differences between employees",
  "Rewards and Advancement",                        "Raises, bonuses, promotions, or who may advance",
  "Work Arrangements and Workload",                 "Benefits, PTO, scheduling, hours, or workload and task assignments",
  "Hiring Decisions",                               "Recruiting, candidate evaluations, or who may or may not be hired",
  "Layoffs and Terminations",                       "Planned layoffs, firings, or role eliminations not yet announced",
  "Employee Movement",                              "Transfers, reassignments, resignations, or private career plans",
  "Organizational and Leadership Changes",          "Restructuring, management changes, or internal leadership discussions",
  "Business Direction and Company Changes",         "Mergers, acquisitions, closures, or long-term strategic plans",
  "Budgets and Financial Health",                   "Budgets, spending limits, or the organization's financial stability",
  "Positive Performance and Talent Assessment",     "Employee evaluations, talent rankings, or positive judgments about employees' potential",
  "Performance Concerns and Corrective Feedback",   "Performance problems, warnings, discipline, negative feedback on work, concerns about employees' potential",
  "Complaints and Investigations",                  "Employee complaints, HR reports, or internal investigations",
  "Projects and Programs",                          "Project status, deadlines, delays, or cancellations not yet announced",
  "Products and Proprietary Information",           "Products, trade secrets, proprietary data, or confidential know-how",
  "Client and Business Information",                "Clients, accounts, investors",
  "Health and Personal Circumstances",              "Employees' health, medical leave, family situations, or private life",
  "Relationships",                                  "Romantic relationships in the office, affairs, or undisclosed workplace relationships",
  "Interpersonal Tensions",                         "Conflict or tensions involving coworkers, distrusting, or disliking each other",
  "Hidden Rules and Workarounds",                   "Informal rules, policy exceptions, misrepresenting time worked, shortcuts, or undocumented practices",
  "Misconduct and Compliance Issues",               "Theft, fraud, misuse of resources, legal, or compliance problems"
)
categories_block <- paste0("- ", categories$name, ": ", categories$definition,
                           collapse = "\n")
valid_labels <- c(categories$name, "NOT-FIT-IN", "N/A")

# ---- Load and prep responses ----
df <- read_csv("secretsnottold_long_form_sorted.csv",
               locale = locale(encoding = "UTF-8")) %>%
  mutate(
    row_uid        = row_number(),
    response_clean = str_trim(str_replace_all(response, "[\r\n]+", " ")),
    # Pre-classify the obvious N/A cases to save API calls
    pre_category   = if_else(
      is.na(response_clean) | response_clean == "" | response_clean == "N/A",
      "N/A", NA_character_
    )
  )

to_code <- df %>% filter(is.na(pre_category))
cat("Total rows:", nrow(df), "| Pre-classified N/A:", sum(!is.na(df$pre_category)),
    "| Sending to API:", nrow(to_code), "\n")

# ---- Gemini helper ----
gemini_json <- function(prompt, max_tokens = 8192) {
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
    req_timeout(120) |>
    req_perform()
  parsed <- resp |> resp_body_json()
  txt <- parsed$candidates[[1]]$content$parts[[1]]$text
  list(json   = fromJSON(txt, simplifyDataFrame = FALSE),
       finish = parsed$candidates[[1]]$finishReason)
}

# ---- Stage A: classify in batches, with resume support ----
progress_file <- "nottold_coded_progress.csv"
done_uids <- if (file.exists(progress_file)) read_csv(progress_file)$row_uid else integer(0)
queue <- to_code %>% filter(!row_uid %in% done_uids)

batch_size <- 20
batches    <- split(queue, ceiling(seq_len(nrow(queue)) / batch_size))

for (b_idx in seq_along(batches)) {
  batch <- batches[[b_idx]]
  items_block <- paste0("[", batch$row_uid, "] ", batch$response_clean,
                        collapse = "\n")
  
  prompt <- paste0(
    "You are a qualitative researcher classifying workplace secrets into ",
    "predefined categories. For each response below, assign EXACTLY ONE label.\n\n",
    "Categories:\n", categories_block, "\n\n",
    "Special rules:\n",
    "- If the response does NOT clearly fit any category above, return the ",
    "literal string \"NOT-FIT-IN\". Do not force-fit.\n",
    "- If the response is blank, gibberish (e.g., \"asdf\"), incomplete, or a ",
    "protest answer (e.g., \"none of your business\", \"I don't want to say\"), ",
    "return the literal string \"N/A\".\n",
    "- Use category names EXACTLY as written.\n",
    "- Each response must receive exactly one label.\n\n",
    "Return ONLY valid JSON in this form:\n",
    '{"results": [{"id": 123, "category": "..."}]}\n\n',
    "Responses:\n", items_block
  )
  
  res <- tryCatch(
    gemini_json(prompt, 8192),
    error = function(e) list(
      json = list(results = list()),
      finish = paste0("ERROR: ", conditionMessage(e))
    )
  )
  
  assignments <- bind_rows(lapply(res$json$results, function(a) {
    tibble(row_uid = as.integer(a$id), category = a$category)
  }))
  
  # Validate against the allowed label set
  assignments <- assignments %>%
    mutate(category = if_else(category %in% valid_labels, category, "NOT-FIT-IN"))
  
  batch_out <- batch %>%
    select(row_uid, ID, response) %>%
    left_join(assignments, by = "row_uid") %>%
    mutate(category = if_else(is.na(category), "NOT-FIT-IN", category))
  
  write_csv(batch_out, progress_file, append = file.exists(progress_file))
  cat("Batch", b_idx, "of", length(batches),
      "(finish:", res$finish, "| n returned:", nrow(assignments), ")\n")
  Sys.sleep(0.5)
}

# ---- Merge API results with pre-classified N/A rows ----
api_results <- read_csv(progress_file, locale = locale(encoding = "UTF-8"))
coded <- df %>%
  select(row_uid, ID, response, pre_category) %>%
  left_join(api_results %>% select(row_uid, category), by = "row_uid") %>%
  mutate(category = coalesce(pre_category, category, "NOT-FIT-IN")) %>%
  select(ID, response, category)

write_csv(coded, "nottold_coded.csv")

cat("\n=== Stage A Summary ===\n")
print(coded %>% count(category, sort = TRUE))

# ---- Stage B: analyze NOT-FIT-IN responses for new categories ----
not_fit <- coded %>% filter(category == "NOT-FIT-IN") %>%
  mutate(idx = row_number())
cat("\nNOT-FIT-IN responses to analyze:", nrow(not_fit), "\n")

suggestions_df <- tibble()
examples_df    <- tibble()

if (nrow(not_fit) > 0) {
  not_fit_block <- paste0("[", not_fit$idx, "] ",
                          str_replace_all(not_fit$response, "[\r\n]+", " "),
                          collapse = "\n")
  existing_names <- paste(categories$name, collapse = ", ")
  
  analysis_prompt <- paste0(
    "Below are workplace-secret responses that could not be classified into ",
    "any of the existing 24 categories. Your task:\n\n",
    "1. Read all responses and identify common patterns or themes.\n",
    "2. Propose 1 to 5 NEW categories that capture these patterns. The new ",
    "categories MUST be conceptually distinct from the existing ones — do ",
    "not duplicate, rename, or trivially split them.\n",
    "3. For each proposed new category provide: name, one-sentence ",
    "definition, rationale (what gap it fills and why this is a meaningful ",
    "pattern), and 2 to 4 example response IDs from the list below.\n\n",
    "Existing categories (DO NOT duplicate):\n", existing_names, "\n\n",
    "Return ONLY valid JSON in this form:\n",
    '{"new_categories": [{"name": "...", "definition": "...", ',
    '"rationale": "...", "example_ids": [1, 5, 12]}]}\n\n',
    "Unfit responses:\n", not_fit_block
  )
  
  analysis_res <- gemini_json(analysis_prompt, max_tokens = 16384)
  new_cats <- analysis_res$json$new_categories
  
  suggestions_df <- bind_rows(lapply(new_cats, function(c) {
    tibble(proposed_name = c$name,
           definition    = c$definition,
           rationale     = c$rationale,
           example_ids   = paste(unlist(c$example_ids), collapse = ", "))
  }))
  
  examples_df <- bind_rows(lapply(new_cats, function(c) {
    tibble(proposed_name = c$name,
           example_id    = as.integer(unlist(c$example_ids)))
  })) %>%
    left_join(not_fit %>% select(idx, ID, response),
              by = c("example_id" = "idx"))
  
  cat("\n=== Proposed New Categories ===\n")
  print(suggestions_df)
}

# ---- Write everything to Excel ----
wb <- createWorkbook()
addWorksheet(wb, "All Coded Responses")
writeData(wb, "All Coded Responses", coded)

addWorksheet(wb, "Category Frequencies")
writeData(wb, "Category Frequencies", coded %>% count(category, sort = TRUE))

addWorksheet(wb, "NOT-FIT-IN Responses")
writeData(wb, "NOT-FIT-IN Responses",
          not_fit %>% select(ID, response))

if (nrow(suggestions_df) > 0) {
  addWorksheet(wb, "Proposed New Categories")
  writeData(wb, "Proposed New Categories", suggestions_df)
  addWorksheet(wb, "New Category Examples")
  writeData(wb, "New Category Examples", examples_df)
}

saveWorkbook(wb, "nottold_classification.xlsx", overwrite = TRUE)
cat("\nSaved: nottold_classification.xlsx\n")