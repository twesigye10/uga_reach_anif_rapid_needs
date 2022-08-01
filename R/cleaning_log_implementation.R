# using the cleaning log to clean the data

library(tidyverse)
library(lubridate)
library(glue)


# read data

df_cleaning_log <- read_csv("inputs/combined_checks_anif.csv") %>% 
  mutate(adjust_log = ifelse(is.na(adjust_log), "apply_suggested_change", adjust_log)) %>%
  filter(adjust_log != "delete_log", !is.na(value), !is.na(uuid)) %>% 
  mutate(sheet = NA, index = NA, relevant = NA) %>% 
  select(uuid, type, name, value, issue_id, sheet, index, relevant, issue)

data_nms <- names(readxl::read_excel(path = "inputs/ANIF_Rapid_Assessment_Tool.xlsx", n_max = 0))
c_types <- ifelse(str_detect(string = data_nms, pattern = "_other$"), "text", "guess")

df_raw_data <- readxl::read_excel(path = "inputs/ANIF_Rapid_Assessment_Data.xlsx", sheet = "inputs/ANIF_Rapid_Assessment_Tool.xlsx", col_types = c_types) %>% 
  mutate(across(.cols = everything(), .fns = ~ifelse(str_detect(string = ., pattern = fixed(pattern = "N/A", ignore_case = TRUE)), "NA", .)))

df_survey <- readxl::read_excel("inputs/ANIF_Rapid_Assessment_Tool.xlsx", sheet = "survey")
df_choices <- readxl::read_excel("inputs/ANIF_Rapid_Assessment_Tool.xlsx", sheet = "choices")



# find all new choices to add to choices sheet ----------------------------

# gather choice options based on unique choices list
df_grouped_choices<- df_choices %>% 
  group_by(list_name) %>% 
  summarise(choice_options = paste(name, collapse = " : "))

# get new name and ad_option pairs to add to the choices sheet
filter(type %in% c("change_response", "add_option")) %>% 
  left_join(df_survey, by = "name") %>% 
  filter(str_detect(string = type.y, pattern = "select_one|select one|select_multiple|select multiple")) %>% 
  separate(col = type.y, into = c("select_type", "list_name"), sep =" ", remove = TRUE, extra = "drop") %>% 
  left_join(df_grouped_choices, by = "list_name") %>%
  filter(!str_detect(string = choice_options, pattern = value ) ) %>%
  rename(choice = value ) %>%
  select(name, choice) %>%
  distinct() %>% # to make sure there are no duplicates
  arrange(name)


# create kobold object ----------------------------------------------------

kbo <- kobold::kobold(survey = df_survey, 
                      choices = df_choices, 
                      data = df_raw_data, 
                      cleaning = df_cleaning_log)


# modified choices for the survey tool ------------------------------------

df_choises_modified <- butteR:::xlsform_add_choices(kobold = kbo, new_choices = new_vars)

# special treat for variables for select_multiple, we need to add the columns to the data itself

df_survey_sm <- df_survey %>% 
  mutate(q_type = case_when(str_detect(string = type, pattern = "select_multiple|select multiple") ~ "sm",
                            str_detect(string = type, pattern = "select_one|select one") ~ "so",
                            TRUE ~ type)) %>% 
  select(name, q_type)

# construct new columns for select multiple

new_vars_sm <- new_vars %>% 
  left_join(df_survey_sm, by = "name") %>% 
  filter(q_type == "sm") %>% 
  mutate(new_cols = paste0(name,"/",choice))


# add new columns to the raw data -----------------------------------------

df_raw_data_modified <- df_raw_data %>% 
  butteR:::mutate_batch(nm = new_vars_sm$new_cols, value = F )


# make some clean up ------------------------------------------------------

kbo_modified <- kobold::kobold(survey = df_survey %>% filter(name %in% colnames(df_raw_data_modified)), 
                               choices = df_choises_modified, 
                               data = df_raw_data_modified, 
                               cleaning = df_cleaning_log )
kbo_cleaned <- kobold::kobold_cleaner(kbo_modified)
  

# handling added responses after starting data collection -----------------

df_final_cleaned_data <- kbo_cleaned$data %>% 
  mutate(across(.cols = contains("/"), .fns = ~ifelse(is.na(.), FALSE, .)))


# write final modified data -----------------------------------------------

write_csv(df_final_cleaned_data, file = paste0("outputs/", butteR::date_file_prefix(), "_clean_data.csv"))

# output data with composite indicators

df_with_composites <- create_composite_indicators_anif(input_df = df_final_cleaned_data)

write_csv(df_with_composites, file = paste0("outputs/", butteR::date_file_prefix(), "_clean_data_with_composite_indicators.csv"))
