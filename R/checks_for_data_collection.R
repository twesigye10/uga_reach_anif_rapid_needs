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

df_survey <- readxl::read_excel("inputs/ANIF_Rapid_Assessment_Tool.xlsx", sheet = "survey")
df_choices <- readxl::read_excel("inputs/ANIF_Rapid_Assessment_Tool.xlsx", sheet = "choices")

df_sample_data <- sf::st_read("inputs/", quiet = TRUE)

# output holder -----------------------------------------------------------

logic_output <- list()


# check time --------------------------------------------------------------

df_
















