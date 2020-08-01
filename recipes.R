library(jsonlite)
library(purrr)
library(magrittr)
library(tibble)
library(dplyr)
library(readr)
library(stringr)

# Download data from https://eightportions.com/datasets/Recipes/#fn:1 and place in data/recipes/

recipes_ar <- fromJSON("data/recipes/recipes_raw_nosource_ar.json", flatten = TRUE)
recipes_epi <- fromJSON("data/recipes/recipes_raw_nosource_epi.json", flatten = TRUE)
recipes_fn <- fromJSON("data/recipes/recipes_raw_nosource_fn.json", flatten = TRUE)

recipes_raw <- c(recipes_ar, recipes_epi, recipes_fn)
recipes_raw <- head(recipes_raw, 1000) # TEMP

recipe_titles <- map(recipes_raw, ~ .x$title)
recipe_ingredients <- map(recipes_raw, ~ .x$ingredients)

measurements_raw <- read_tsv("data/measurements.tsv")
measurements <- measurements_raw %>%
  filter(type == "measurement") %>%
  use_series(name)
measurements_regex <- measurements %>%
  paste0(., "\\w*") %>%
  paste(collapse = "|") %>%
  paste0("(", ., ")")
abbreviations <- measurements_raw %>%
  filter(type == "abbr") %>%
  use_series(name)
abbreviations_regex <- abbreviations %>%
  paste0("[^\\s]+", ., "[\\s$]+") %>%
  paste(collapse = "|") %>%
  paste0("(", ., ")")

clean_ingredient <- function(ingredient) {
  cleaned_ingredient <- ingredient %>%
    str_remove_all(fixed("ADVERTISEMENT")) %>%
    str_remove_all("\\(.+\\)") %>% # remove everythiing inside of parentheses
    str_remove(",.+") %>% # remove everything after a comma
    str_remove("\\d+\\/\\d+") %>% # remove fractions
    str_remove("\\d+") %>% # remove numbers
    str_remove_all(measurements_regex) %>%
    str_remove_all(abbreviations_regex) %>%
    str_trim() # remove trailing and leading whitespaces

  return(cleaned_ingredient)
}

all_ingredients <- recipe_ingredients %>%
  unlist() %>%
  unname() %>%
  map(clean_ingredient) %>%
  unlist() %>%
  map(~ ifelse(.x == "", NA_character_, .x)) %>%
  unlist() %>%
  na.omit()

most_popular_ingredients <- all_ingredients %>%
  tibble(ingredient = .) %>%
  group_by(ingredient) %>%
  count() %>%
  arrange(-n)

most_popular_ingredients
