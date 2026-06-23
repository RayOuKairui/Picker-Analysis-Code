# =============================================================================
# Long-form Reshaping — "name 3 things" free responses
# -----------------------------------------------------------------------------
# Utility script that converts the wide "name 3 things" free-response data
# (five response columns per participant) into a long format with one response
# per row, sorted alphabetically by the first letter of the response, and
# writes the result to a new CSV for downstream coding.
#
# NOTE: This file only adds comments/section headers for readability.
#       All code logic, variable names, paths, and outputs are unchanged from
#       the original.
# =============================================================================


# =============================================================================
# 1. SETUP — PACKAGES
# =============================================================================

install.packages("readr")
library(readr)    # read_csv() / write_csv() — fast CSV input/output
library(dplyr)    # select(), mutate(), arrange(), %>% pipe for data wrangling
library(tidyr)    # pivot_longer() — reshape wide to long
library(stringr)  # str_to_lower(), str_sub() — string helpers


# =============================================================================
# 2. READ DATA
# =============================================================================

# Read the CSV
df <- read_csv("Common Secrecy Clean Original File.csv")


# =============================================================================
# 3. WIDE -> LONG TRANSFORMATION
# =============================================================================

# Convert from wide format to long format
long_df <- df %>%
  # Keep the participant ID and the five free-response columns.
  select(
    ID,
    name3things_form_1,
    name3things_form_2,
    name3things_form_3,
    name3things_form_4,
    name3things_form_5,
  ) %>%
  # Stack the five response columns into one row per response (keep NAs for now).
  pivot_longer(
    cols = starts_with("name3things_"),
    names_to = "response_number",
    values_to = "response",
    values_drop_na = FALSE
  ) %>%
  # Replace missing responses with the literal "N/A", and derive a sort key from
  # the lowercase first character of each response.
  mutate(
    response = if_else(is.na(response), "N/A", response),
    initial_character = str_to_lower(str_sub(response, 1, 1))
  ) %>%
  # Sort alphabetically by first letter (then ID) and keep only ID + response.
  arrange(initial_character, ID) %>%
  select(ID, response)


# =============================================================================
# 4. WRITE OUTPUT
# =============================================================================

# Save the cleaned long-form file
write_csv(long_df, "secretsnothear_long_form_sorted.csv", na = "N/A")
