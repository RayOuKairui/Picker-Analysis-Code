install.packages("readr")
library(readr)
library(dplyr)
library(tidyr)
library(stringr)

# Read the CSV
df <- read_csv("Common Secrecy Clean Original File.csv")

# Convert from wide format to long format
long_df <- df %>%
  select(
    ID,
    name3things_form_1,
    name3things_form_2,
    name3things_form_3,
    name3things_form_4,
    name3things_form_5,
  ) %>%
  pivot_longer(
    cols = starts_with("name3things_"),
    names_to = "response_number",
    values_to = "response",
    values_drop_na = FALSE
  ) %>%
  mutate(
    response = if_else(is.na(response), "N/A", response),
    initial_character = str_to_lower(str_sub(response, 1, 1))
  ) %>%
  arrange(initial_character, ID) %>%
  select(ID, response)

# Save the cleaned long-form file
write_csv(long_df, "secretsnothear_long_form_sorted.csv", na = "N/A")
