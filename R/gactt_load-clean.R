################################################################################

# GACTT - Loading and cleaning

################################################################################

# Set up environment -----------------------------------------------------------

packages <- c(
  "tidyverse"
)

lapply(packages, library, character.only = TRUE)

# Load data --------------------------------------------------------------------

# Load raw data

## Automatically download data

# If necessary to download manually, data set can be downloaded from
# https://bit.ly/gacttCSV, and they should be placed in a subdirectory called
# "data". The code below assumes the file name has not been changed. This link
# is available in the video description for Hoffman (2023, Surprising And
# Fascinating Results From The Taste Test,
# https://www.youtube.com/watch?v=bMOOQfeloH0) (2023-12-10)

if (!file.exists("data/GACTT_RESULTS_ANONYMIZED_v2.csv")) {
  
  download.file("https://bit.ly/gacttCSV",
                "data/GACTT_RESULTS_ANONYMIZED_v2.csv")
  
}

## Load data

gactt_raw <- read_csv("data/GACTT_RESULTS_ANONYMIZED_v2.csv")

# Column names

item_text <- colnames(gactt_raw)

colnames(gactt_raw) <- c(
  
  "id", "age", 
  "cups_per_day", 
  
  "where_drink", "at_home", "at_office", "on_the_go", "at_cafe", "at_none",
  
  "how_at_home", "pour_over", "french_press", "espresso", "brewing_machine",
  "pod_machine", "instant", "bean_to_cup", "cold_brew", "coffee_extract", 
  "how_other", "how_else",
  
  "where_purchase", "national_chain", "local_cafe", "drive_thru", "specialty",
  "supermarket", "where_purchase_other", "where_purchase_else",
  
  "favorite_coffee", "favorite_specify",
  
  "add_anything", "just_black", "add_dairy", "add_sugar", "add_flavoring",
  "add_other", "add_other_text",
  
  "dairy", "whole_milk", "skim_milk", "half_and_half", 
  "creamer", "flavored_creamer", "oat_milk", "almond_milk", "soy_milk", 
  "dairy_other",
  
  "sweetener", "granulated", "artificial", "honey", "maple", "stevia", "agave",
  "brown_sugar", "raw_sugar",
  
  "flavoring", "vanilla", "caramel", "hazelnut", "cinnamon", "peppermint",
  "flavor_other", "flavor_other_text",
  
  "prior_preferences", "preference_strong", "preference_roast", 
  "preference_caffeine",
  
  "expertise",
  
  "a_bitter", "a_acidity", "a_preference", "a_notes",
  "b_bitter", "b_acidity", "b_preference", "b_notes",
  "c_bitter", "c_acidity", "c_preference", "c_notes",
  "d_bitter", "d_acidity", "d_preference", "d_notes",
  
  "compare_abc", "compare_ad", "compare_overall",
  
  "work_from_home", "monthly_spending",
  
  "why_coffee", "taste", "caffeine_need", "ritual", "toilet", "why_other",
  "why_other_text",
  
  "like_taste", "know_origin", "most_paid", "most_willing_pay", "value_cafe",
  "equipment_spending", "equipment_value",
  
  "gender", "gender_text",
  "education", "ethnicity", "ethnicity_text",
  "employment", "children", "political_aff"
)

# Create codebook

gactt_codebook <- data.frame(
  variable  = colnames(gactt_raw),
  item_text = item_text
)

gactt_codebook$options <- map(gactt_raw, unique)

# Export cleaned data and codebook ---------------------------------------------

write_csv(gactt_raw, "data/gactt_data-cleaned.csv")
write_rds(gactt_raw, "data/gactt_data-cleaned.rds")

write_csv(gactt_codebook, "data/gactt_codebook.csv")
write_rds(gactt_codebook, "data/gactt_codebook.rds")
