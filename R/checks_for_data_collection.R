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



# spatial checks ----------------------------------------------------------

sample_pt_nos <- df_sample_data %>% 
  pull(unique_pt_number) %>% 
  unique()

# duplicate point numbers
df_duplicate_pt_nos <- check_duplicate_pt_numbers(input_tool_data = df_tool_data, 
                                                  input_sample_pt_nos_list = sample_pt_nos)

add_checks_data_to_list(input_list_name = "logic_output", input_df_name = "df_duplicate_pt_nos")













