# checks for data collection

library(checksupporteR)
library(tidyverse)
library(lubridate)
library(glue)


# read data ---------------------------------------------------------------

df_tool_data <- readxl::read_excel(path = "inputs/ANIF_Rapid_Assessment_Data.xlsx") %>% 
  mutate(i.check.uuid = `_uuid`,
         i.check.start_date = as_date(start),
         i.check.enumerator_id = enumerator_id,
         i.check.district_name = district_name,
         i.check.point_number = point_number,
         start = as_date(start),
         end = as_datetime(end))

df_survey <- readxl::read_excel(path = "inputs/ANIF_Rapid_Assessment_Tool.xlsx", sheet = "survey")
df_choices <- readxl::read_excel(path = "inputs/ANIF_Rapid_Assessment_Tool.xlsx", sheet = "choices")

df_sample_data <- sf::st_read("inputs/", quiet = TRUE)

# output holder -----------------------------------------------------------

logic_output <- list()


# Time checks -------------------------------------------------------------

# Time interval for the survey
min_time_of_survey <- 20
max_time_of_survey <- 120


df_survey_time <- check_survey_time(input_tool_data = df_tool_data, 
                                    input_min_time = min_time_of_survey,
                                    input_max_time = max_time_of_survey)

add_checks_data_to_list(input_list_name = "logic_output",input_df_name = "df_survey_time")


# check the time between surveys
min_time_btn_surveys <- 5

df_time_btn_surveys <- check_time_interval_btn_surveys(input_tool_data = df_tool_data, 
                                                       input_min_time = min_time_btn_surveys)

add_checks_data_to_list(input_list_name = "logic_output",input_df_name = "df_time_btn_surveys")


# outlier checks ----------------------------------------------------------

df_c_outliers <- checksupporteR::check_outliers_cleaninginspector(input_tool_data = df_tool_data)

add_checks_data_to_list(input_list_name = "logic_output",input_df_name = "df_c_outliers")

# spatial checks ----------------------------------------------------------

sample_pt_nos <- df_sample_data %>% 
  pull(unique_pt_number) %>% 
  unique()

# duplicate point numbers
df_duplicate_pt_nos <- check_duplicate_pt_numbers(input_tool_data = df_tool_data, 
                                                  input_sample_pt_nos_list = sample_pt_nos)

add_checks_data_to_list(input_list_name = "logic_output", input_df_name = "df_duplicate_pt_nos")

# point number does not exist in sample

df_pt_number_not_in_sample <- check_pt_number_not_in_samples(input_tool_data = df_tool_data, 
                                                             input_sample_pt_nos_list = sample_pt_nos)

add_checks_data_to_list(input_list_name = "logic_output", input_df_name = "df_pt_number_not_in_sample")


# check for exceeded threshold distance

threshold_dist <- 150

df_greater_thresh_distance <- check_threshold_distance(input_sample_data = df_sample_data, 
                                                       input_tool_data = df_tool_data, 
                                                       input_threshold_dist = threshold_dist)

add_checks_data_to_list(input_list_name = "logic_output", input_df_name = "df_greater_thresh_distance")


# others checks -----------------------------------------------------------

df_others_data <- extract_other_specify_data(input_tool_data = df_tool_data, 
                                             input_survey = df_survey, 
                                             input_choices = df_choices)

add_checks_data_to_list(input_list_name = "logic_output", input_df_name = "df_others_data")



# logical checks for different responses ----------------------------------

# educ_level_above_primary_but_cannot_count_money_in_possession_calculate_money_left_calculate_money_saved_for_future_calculate_money_items_can_buy

df_educ_level_above_primary_but_cannot_count_money <- df_tool_data %>%
  filter(respondent_education %in% c("completed_primary", "incomplete_secondary", "completed_secondary", "incomplete_tertiary", "completed_tertiary")&
           knowledge_financial_literacy =="none") %>% 
  mutate(i.check.type = "change_response",
         i.check.name = "respondent_education",
         i.check.current_value = respondent_education,
         i.check.value = "",
         i.check.issue_id = "logic_c_educ_level_above_primary_but_cannot_count",
         i.check.issue = glue("knowledge_financial_literacy:{knowledge_financial_literacy}"),
         i.check.other_text = "",
         i.check.checked_by = "",
         i.check.checked_date = as_date(today()),
         i.check.comment = "accept", 
         i.check.reviewed = "",
         i.check.adjust_log = "",
         i.check.uuid_cl = "",
         i.check.so_sm_choices = "") %>% 
  dplyr::select(starts_with("i.check"))%>% 
  rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))

add_checks_data_to_list(input_list_name = "logic_output", input_df_name = "df_educ_level_above_primary_but_cannot_count_money")    
    

# hh_members_currently_eating_less_than_two_meals_yet_they_reported_eating_more_than_in_home_country

df_hh_currently_eating_less_than_two_meals <- df_tool_data %>%
  filter(average_daily_fd_consumption == "less_than_two_meals", food_amount_change == "we_are_now_eating_more_than_we_were_in_our_home_country") %>% 
  mutate(i.check.type = "change_response",
         i.check.name = "average_daily_fd_consumption",
         i.check.current_value = average_daily_fd_consumption,
         i.check.value = "",
         i.check.issue_id = "logic_c_hh_currently_eating_less_than_two_meals",
         i.check.issue = glue("average_daily_fd_consumption: {average_daily_fd_consumption}"),
         i.check.other_text = "",
         i.check.checked_by = "",
         i.check.checked_date = as_date(today()),
         i.check.comment = "accept", 
         i.check.reviewed = "",
         i.check.adjust_log = "",
         i.check.uuid_cl = "",
         i.check.so_sm_choices = "") %>% 
  dplyr::select(starts_with("i.check"))%>% 
  rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))

add_checks_data_to_list(input_list_name = "logic_output", input_df_name = "df_hh_currently_eating_less_than_two_meals")    

# hh_not_having_any_food_categories_in_stock

df_does_not_have_any_food_category_in_stock <- df_tool_data %>% 
  filter()
  

    
    
    
  
  
  
  




