# checks for data collection

library(checksupporteR)
library(tidyverse)
library(lubridate)
library(glue)


# read data ---------------------------------------------------------------

df_tool_data <- readxl::read_excel(path = "inputs/ANIF_Rapid_Assessment_Data.xlsx") %>% 
  mutate(i.check.uuid = `_uuid`,
         i.check.start_date = as_date(start),
         i.check.enumerator_id = as.character(enumerator_id),
         i.check.district_name = district_name,
         i.check.point_number = point_number) %>% 
  filter(i.check.start_date > as_date("2022-07-27"))

df_survey <- readxl::read_excel(path = "inputs/ANIF_Rapid_Assessment_Tool.xlsx", sheet = "survey")
df_choices <- readxl::read_excel(path = "inputs/ANIF_Rapid_Assessment_Tool.xlsx", sheet = "choices")

df_sample_data <- sf::st_read("inputs/anif_rapid_settlement_samples.gpkg", quiet = TRUE)

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

if("status" %in% colnames(df_sample_data)){
  sample_pt_nos <- df_sample_data %>% 
    mutate(unique_pt_number = paste0(status, "_", Name)) %>% 
    pull(unique_pt_number) %>% 
    unique()
}else{
  sample_pt_nos <- df_sample_data %>% 
    mutate(unique_pt_number = Name) %>% 
    pull(unique_pt_number) %>% 
    unique()
}

# duplicate point numbers
df_duplicate_pt_nos <- check_duplicate_pt_numbers(input_tool_data = df_tool_data, 
                                                  input_sample_pt_nos_list = sample_pt_nos)

add_checks_data_to_list(input_list_name = "logic_output", input_df_name = "df_duplicate_pt_nos")

# point number does not exist in sample

df_pt_number_not_in_sample <- check_pt_number_not_in_samples(input_tool_data = df_tool_data, 
                                                             input_sample_pt_nos_list = sample_pt_nos)

add_checks_data_to_list(input_list_name = "logic_output", input_df_name = "df_pt_number_not_in_sample")


# check for exceeded threshold distance

threshold_dist <- 300

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

# educ level above primary but cannot count money in possession

df_educ_level_above_primary_but_cannot_count_money <- df_tool_data %>%
  filter(respondent_education %in% c("completed_primary", "incomplete_secondary", "completed_secondary", "incomplete_tertiary", "completed_tertiary") ,
           knowledge_financial_literacy =="none") %>% 
  mutate(i.check.type = "add_option",
         i.check.name = "knowledge_financial_literacy",
         i.check.current_value = knowledge_financial_literacy,
         i.check.value = "count_money_in_possession",
         i.check.issue_id = "logic_c_educ_level_above_primary_but_cannot_count",
         i.check.issue = glue("respondent_education:{respondent_education}"),
         i.check.other_text = "",
         i.check.checked_by = "",
         i.check.checked_date = as_date(today()),
         i.check.comment = "FO to follow up with enumerator", 
         i.check.reviewed = "",
         i.check.adjust_log = "",
         i.check.so_sm_choices = "") %>%
  slice(rep(1:n(), each = 2)) %>% 
  group_by(i.check.uuid, i.check.start_date, i.check.enumerator_id, i.check.district_name, 
           i.check.point_number, i.check.type,  i.check.name,  i.check.current_value) %>% 
  mutate(rank = row_number(),
         i.check.type = ifelse(rank == 2, "remove_option", i.check.type),
         i.check.value = ifelse(rank == 2, i.check.current_value, i.check.value)) %>%
  dplyr::select(starts_with("i.check"))%>% 
  rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))

add_checks_data_to_list(input_list_name = "logic_output", input_df_name = "df_educ_level_above_primary_but_cannot_count_money")    


# If completed primary and above but can not allocate money to priorities i.e. "do you......" is "none", check
df_money_usage <- df_tool_data %>% 
  filter(respondent_education %in% c("completed_primary", "incomplete_secondary", "completed_secondary", "incomplete_tertiary", 
                                     "completed_tertiary") , money_usage == "none") %>%
  mutate(i.check.type = "remove_option",
         i.check.name = "money_usage",
         i.check.current_value = money_usage,
         i.check.value = "",
         i.check.issue_id = "logic_c_money_usage",
         i.check.issue = glue("respondent_education: {respondent_education}, but money_usage: {money_usage}"),
         i.check.other_text = "",
         i.check.checked_by = "",
         i.check.checked_date = as_date(today()),
         i.check.comment = "", 
         i.check.reviewed = "",
         i.check.adjust_log = "",
         i.check.so_sm_choices = "") %>%
  dplyr::select(starts_with("i.check.")) %>% 
  rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))

add_checks_data_to_list(input_list_name = "logic_output", input_df_name = "df_money_usage")


# average meals eaten a day i.e "average_daily_fd_consumption" = "three_meals" and "more_than_three_meals" and amount of food hh 
# members are currently eating i.e. "food_amount_change = "we_are_now_eating_less_than_we_were_in_our_home_country", check
df_average_daily_fd_consumption <- df_tool_data %>% 
  filter(average_daily_fd_consumption %in% c("more_than_three_meals", "three_meals") , 
         food_amount_change == "we_are_now_eating_less_than_we_were_in_our_home_country") %>%
  mutate(i.check.type = "change_response",
         i.check.name = "food_amount_change",
         i.check.current_value = food_amount_change,
         i.check.value = "",
         i.check.issue_id = "logic_c_food_amount_change",
         i.check.issue = glue("food_amount_change: {food_amount_change}, but average_daily_fd_consumption: {average_daily_fd_consumption}"),
         i.check.other_text = "",
         i.check.checked_by = "",
         i.check.checked_date = as_date(today()),
         i.check.comment = "", 
         i.check.reviewed = "",
         i.check.adjust_log = "",
         i.check.so_sm_choices = "") %>% 
  dplyr::select(starts_with("i.check.")) %>% 
  rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))

add_checks_data_to_list(input_list_name = "logic_output", input_df_name = "df_average_daily_fd_consumption")


# average meals eaten a day i.e average_daily_fd_consumption = "less_than_two_meals" and 
# food_amount_change = "we_are_now_eating_more_than_we_were_in_our_home_country"
df_food_amount_change <- df_tool_data %>% 
  filter(str_detect(string = food_amount_change, pattern = "we_are_now_eating_more_than_we_were_in_our_home_country") , 
         average_daily_fd_consumption == "less_than_two_meals") %>%
  mutate(i.check.type = "change_response",
         i.check.name = "average_daily_fd_consumption",
         i.check.current_value = as.character(average_daily_fd_consumption),
         i.check.value = "",
         i.check.issue_id = "logic_c_average_daily_fd_consumption",
         i.check.issue = glue("food_amount_change: {food_amount_change}, but average_daily_fd_consumption: {average_daily_fd_consumption}"),
         i.check.other_text = "",
         i.check.checked_by = "",
         i.check.checked_date = as_date(today()),
         i.check.comment = "", 
         i.check.reviewed = "",
         i.check.adjust_log = "",
         i.check.so_sm_choices = "")%>% 
  dplyr::select(starts_with("i.check.")) %>% 
  rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))

add_checks_data_to_list(input_list_name = "logic_output", input_df_name = "df_food_amount_change")


# If hh currently has no access to cooking fuel i.e. cooking_fuel_access = "no" and 
# barriers_accessing_cooking_fuel = "we_do_not_face_any_barriers_to_accessing_fuel_for_cooking", check
df_cooking_fuel_access <- df_tool_data %>% 
  filter( barriers_accessing_cooking_fuel %in% c("we_do_not_face_any_barriers_to_accessing_fuel_for_cooking"), 
         cooking_fuel_access == "no") %>%
  mutate(i.check.type = "remove_option",
         i.check.name = "barriers_accessing_cooking_fuel",
         i.check.current_value = barriers_accessing_cooking_fuel,
         i.check.value = "",
         i.check.issue_id = "logic_c_barriers_accessing_cooking_fuel_no",
         i.check.issue = glue("barriers_accessing_cooking_fuel: {barriers_accessing_cooking_fuel}, but cooking_fuel_access: {cooking_fuel_access}"),
         i.check.other_text = "",
         i.check.checked_by = "",
         i.check.checked_date = as_date(today()),
         i.check.comment = "", 
         i.check.reviewed = "",
         i.check.adjust_log = "",
         i.check.so_sm_choices = "") %>% 
  dplyr::select(starts_with("i.check.")) %>% 
  rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))

add_checks_data_to_list(input_list_name = "logic_output", input_df_name = "df_cooking_fuel_access")


# If hh currently has no access electricity i.e. access_to_electricity = "no_we_do_not_have_access_to_any_source_of_electricity" and time walking to electricity source i.e
# time_walking_to_electricity_source = "less_than_30_minutes", check
df_access_to_electricity <- df_tool_data %>% 
  filter(access_to_electricity %in% c("no_we_do_not_have_access_to_any_source_of_electricity"), 
         time_walking_to_electricity_source == "less_than_30_minutes") %>%
  mutate(i.check.type = "change_response",
         i.check.name = "access_to_electricity",
         i.check.current_value = access_to_electricity,
         i.check.value = "",
         i.check.issue_id = "logic_c_access_to_electricity_no",
         i.check.issue = glue("access_to_electricity: {access_to_electricity}, but 
                              time_walking_to_electricity_source: {time_walking_to_electricity_source}"),
         i.check.other_text = "",
         i.check.checked_by = "",
         i.check.checked_date = as_date(today()),
         i.check.comment = "", 
         i.check.reviewed = "",
         i.check.adjust_log = "",
         i.check.so_sm_choices = "") %>%
  dplyr::select(starts_with("i.check.")) %>% 
  rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))

add_checks_data_to_list(input_list_name = "logic_output", input_df_name = "df_access_to_electricity")


# If hh currently accesses electricity throuh solar i.e. access_to_electricity = "yes_we_have_a_solar_panel" but   
# among assets hh owns there is no solar i.e hh_access_to_asset_ownership != "solar_panel"

df_access_to_electricity_solar <- df_tool_data %>% 
  filter(access_to_electricity == "yes_we_have_a_solar_panel", 
         !str_detect(string = hh_access_to_asset_ownership, pattern = "solar_panel")) %>%
  mutate(i.check.type = "add_option",
         i.check.name = "hh_access_to_asset_ownership",
         i.check.current_value = hh_access_to_asset_ownership,
         i.check.value = "solar_panel",
         i.check.issue_id = "logic_c_hh_access_to_asset_ownership_solar",
         i.check.issue = glue("access_to_electricity: {access_to_electricity}, but 
                              hh_access_to_asset_ownership: {hh_access_to_asset_ownership}"),
         i.check.other_text = "",
         i.check.checked_by = "",
         i.check.checked_date = as_date(today()),
         i.check.comment = "", 
         i.check.reviewed = "",
         i.check.adjust_log = "",
         i.check.so_sm_choices = "") %>%
  dplyr::select(starts_with("i.check.")) %>% 
  rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))

add_checks_data_to_list(input_list_name = "logic_output", input_df_name = "df_access_to_electricity_solar")


# If among assets hh owns there is solar i.e hh_access_to_asset_ownership = "solar_panel" but hh currently accesses 
# electricity has no solar i.e. access_to_electricity != "yes_we_have_a_solar_panel", check

df_hh_access_to_asset_ownership <- df_tool_data %>% 
  filter(str_detect(string = hh_access_to_asset_ownership, pattern = "solar_panel"), 
         access_to_electricity != "yes_we_have_a_solar_panel") %>%
  mutate(i.check.type = "change_response",
         i.check.name = "access_to_electricity",
         i.check.current_value = access_to_electricity,
         i.check.value = "yes_we_have_a_solar_panel",
         i.check.issue_id = "logic_c_access_to_electricity_yes_solar",
         i.check.issue = glue("hh_access_to_asset_ownership: {hh_access_to_asset_ownership}, but 
                              access_to_electricity: {access_to_electricity}"),
         i.check.other_text = "",
         i.check.checked_by = "",
         i.check.checked_date = as_date(today()),
         i.check.comment = "", 
         i.check.reviewed = "",
         i.check.adjust_log = "",
         i.check.so_sm_choices = "") %>%
  dplyr::select(starts_with("i.check.")) %>% 
  rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))

add_checks_data_to_list(input_list_name = "logic_output", input_df_name = "df_hh_access_to_asset_ownership")


# hh main source of food is i.e. family_main_fd_source = "humanitarian_assistance" but hh has never received humanitarian assistance i.e.
# access_humanitarian_assistance = "no"
df_family_main_fd_source <- df_tool_data %>% 
  filter(family_main_fd_source %in% c("humanitarian_assistance"), access_humanitarian_assistance == "no") %>%
  mutate(i.check.type = "change_response",
         i.check.name = "access_humanitarian_assistance",
         i.check.current_value = access_humanitarian_assistance,
         i.check.value = "yes",
         i.check.issue_id = "logic_c_access_humanitarian_assistance_no",
         i.check.issue = glue("access_humanitarian_assistance: {access_humanitarian_assistance}, 
                              but family_main_fd_source: {family_main_fd_source}"),
         i.check.other_text = "",
         i.check.checked_by = "",
         i.check.checked_date = as_date(today()),
         i.check.comment = "", 
         i.check.reviewed = "",
         i.check.adjust_log = "",
         i.check.so_sm_choices = "") %>% 
  dplyr::select(starts_with("i.check.")) %>% 
  rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))

add_checks_data_to_list(input_list_name = "logic_output", input_df_name = "df_family_main_fd_source")


# If number of school aged children is not equal to number currently enrolled in school  i.e. 
#children_enrolled_in_school != school_aged_children 
df_children_enrolled_in_school <- df_tool_data %>% 
  filter(children_enrolled_in_school != school_aged_children ) %>%
  mutate(i.check.type = "change_response",
         i.check.name = "children_enrolled_in_school",
         i.check.current_value = as.character(children_enrolled_in_school),
         i.check.value = "",
         i.check.issue_id = "logic_c_children_enrolled_in_school_less",
         i.check.issue = glue("children_enrolled_in_school: {children_enrolled_in_school}, but school_aged_children: {school_aged_children}"),
         i.check.other_text = "",
         i.check.checked_by = "",
         i.check.checked_date = as_date(today()),
         i.check.comment = "", 
         i.check.reviewed = "",
         i.check.adjust_log = "",
         i.check.so_sm_choices = "") %>% 
  dplyr::select(starts_with("i.check.")) %>% 
  rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))

add_checks_data_to_list(input_list_name = "logic_output", input_df_name = "df_children_enrolled_in_school")


# If average daily meals are i.e. average_daily_fd_consumption = "three_meals", "more_than_three_meals" 
# and food in stock i.e food_category = none, check
df_food_stock_category <- df_tool_data %>% 
  filter(average_daily_fd_consumption %in% c("three_meals", "more_than_three_meals"), food_category == "none") %>%
  mutate(i.check.type = "change_response",
         i.check.name = "food_category",
         i.check.current_value = food_category,
         i.check.value = "",
         i.check.issue_id = "logic_c_food_category_stock_none",
         i.check.issue = glue("food_category: {food_category}, 
                              but average_daily_fd_consumption: {average_daily_fd_consumption}"),
         i.check.other_text = "",
         i.check.checked_by = "",
         i.check.checked_date = as_date(today()),
         i.check.comment = "", 
         i.check.reviewed = "",
         i.check.adjust_log = "",
         i.check.so_sm_choices = "") %>% 
  dplyr::select(starts_with("i.check.")) %>% 
  rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))

add_checks_data_to_list(input_list_name = "logic_output", input_df_name = "df_food_stock_category")


df_combined_checks <- bind_rows(logic_output)

# output the resulting data frame
write_csv(x = df_combined_checks, file = paste0("outputs/", butteR::date_file_prefix(), "_combined_checks_anif.csv"), na = "")


  
  
  
  




