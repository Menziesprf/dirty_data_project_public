
library(tidyverse)
library(assertr)
library(readxl)
library(here)


sweets_2015_raw <- read_xlsx(here("raw_data/boing-boing-candy-2015.xlsx")) %>% 
  janitor::clean_names()
sweets_2016_raw <- read_xlsx(here("raw_data/boing-boing-candy-2016.xlsx")) %>% 
  janitor::clean_names()
sweets_2017_raw <- read_xlsx(here("raw_data/boing-boing-candy-2017.xlsx")) %>% 
  janitor::clean_names()

# Final table: Timestamp, Age, Gender, Country, Trick or treating?, 
                #sweets_type, rating,
# There are lots of columns in each dataset to be removed = random silly questions.

# 2017 needs "q_[0-9]" removed

# RENAMING + ADD YEAR COLUMN

working_2015 <- sweets_2015_raw %>% 
  rename(age = how_old_are_you, 
         trick_or_treating = are_you_going_actually_going_trick_or_treating_yourself,
         "100_grand_bar" = x100_grand_bar) %>% 
  bind_cols("year" = "2015")

working_2016 <- sweets_2016_raw %>% 
  rename(age = how_old_are_you,
         trick_or_treating = are_you_going_actually_going_trick_or_treating_yourself,
         gender = your_gender,
         country = which_country_do_you_live_in,
         state = which_state_province_county_do_you_live_in,
         "100_grand_bar" = x100_grand_bar) %>% 
  bind_cols("year" = "2016")



working_2017 <- sweets_2017_raw %>% 
  rename_with(.fn = ~ str_remove_all(., "q[0-9]+_"), .cols = everything()) %>% 
  rename(trick_or_treating = going_out,
         state = state_province_county_etc) %>% 
  bind_cols("year" = "2017")




# PIVOT LONGER

working_2015 <- working_2015 %>% 
  pivot_longer(cols = "100_grand_bar":york_peppermint_patties,
               names_to = "sweets_type",
               values_to = "rating") %>% 
  select(year, age, trick_or_treating, sweets_type, rating)

working_2016 <- working_2016 %>% 
  pivot_longer(cols = "100_grand_bar":york_peppermint_patties,
               names_to = "sweets_type",
               values_to = "rating") %>% 
  select(year, age, trick_or_treating, sweets_type, rating, country, gender)

working_2017 <- working_2017 %>% 
  pivot_longer(cols = "100_grand_bar":york_peppermint_patties,
               names_to = "sweets_type",
               values_to = "rating") %>% 
  select(year, age, trick_or_treating, sweets_type, rating, country, gender)

# DROP RATING NAs

working_2015 <- working_2015 %>% 
  drop_na(rating)

working_2016 <- working_2016 %>% 
  drop_na(rating)

working_2017 <- working_2017 %>% 
  drop_na(rating)


# JOIN TABLES

complete_working <- bind_rows(working_2015, working_2016, working_2017)


# AGE
# Removing all writing, so any explanation of a person's age. 
# This may result in some inaccurate ages but I think this is an issue of data collection
          # eg. "I am NOT 45" will now = "45"
# I think this apporach will salvage as many accurate responses as possible
# Set age limit 2 - 120...fairly arbitrary 


complete_working <- complete_working %>% 
  mutate(age = str_remove_all(age, "[a-zA-Z-!?:\\(\\)'\"/+ ]+")) %>% 
  mutate(age = as.integer(age)) %>% 
  mutate(age = replace(age, age > 120, NA)) %>% 
  mutate(age = replace(age, age <= 2, NA)) 
  
#  SWEETS TYPE
# Should one bother putting all M&Ms together? I vote no
# I will sort the box-o-raisins ones though

complete_working <- complete_working %>% 
  mutate(sweets_type = recode(sweets_type, 
                              "boxo_raisins" = "box_o_raisins",
                              "bonkers_the_candy" = "bonkers",
                              "anonymous_brown_globs_that_come_in_black_and_orange_wrappers_a_k_a_mary_janes" = "anonymous_brown_globs_that_come_in_black_and_orange_wrappers"))

# COUNTRIES

complete_working <- complete_working %>% 
  mutate(country = str_to_lower(country)) %>% 
  mutate(country = recode(country,
                          "not the usa or canada" = "OTHER",
                          "us|us of a|trumpistan" = "usa",
                          "ahem....amerca" = "usa",
                          "the yoo ess of aaayyyyyy" = "usa",
                          "california" = "usa",
                          "new york" = "usa",
                          "pittsburgh" = "usa",
                          "north carolina" = "usa",
                          "new jersey" = "usa",
                          "alaska" = "usa",
                          "england" = "uk",
                          "scotland" = "uk",
                          "españa" = "spain",
                          "a tropical island south of the equator" = "OTHER",
                          "neverland" = "OTHER",
                          "this one" = "OTHER",
                          "denial" = "OTHER",
                          "there isn't one for old men" = "OTHER",
                          "somewhere" = "OTHER",
                          "one of the best ones" = "OTHER",
                          "god's country" = "OTHER",
                          "eua" = "OTHER",
                          "see above" = "OTHER",
                          "earth" = "OTHER",
                          "insanity lately" = "OTHER",
                          "a" = "OTHER",
                          "can" = "OTHER",
                          "canae" = "OTHER",
                          "narnia" = "OTHER",
                          "i don't know anymore" = "OTHER",
                          "fear and loathing" = "OTHER",
                          "endland" = "OTHER",
                          "atlantis" = "OTHER")) %>% 
  mutate(country = case_when(
    str_detect(country, "united k") ~ "uk",
    str_detect(country, "usa") ~ "usa",
    str_detect(country, "u.s") ~ "usa",
    str_detect(country, "united") ~ "usa",
    str_detect(country, "merica") ~ "usa",
    str_detect(country, "murica") ~ "usa",
    str_detect(country, "state") ~ "usa",
    str_detect(country, "murrika") ~ "usa",
    str_detect(country, "state") ~ "usa",
    str_detect(country, "canada") ~ "canada",
    str_detect(country, "netherlands") ~ "netherlands",
    str_detect(country, "[0-9]") ~ "OTHER",
    str_detect(country, "cascadia") ~ "OTHER",
    TRUE ~ country))

complete_working <- complete_working %>% 
  mutate(country = na_if(country, "OTHER"))

# Remove next if you want individual countries other than uk, us, canada
complete_working <- complete_working %>% 
  mutate(country = case_when(
    !country %in% c("usa", "uk", "canada", NA) ~ "other",
    TRUE ~ country))

# GENDER column fine

complete_working %>% 
  select(gender) %>% 
  distinct() %>% 
  pull()


# WRITE AT LAST!

clean_sweets <- complete_working


write_csv(clean_sweets, here("clean_data/sweets_cleaned.CSV"))
