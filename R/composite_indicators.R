# function for creating composite indicators

create_composite_indicators_anif <- function(input_df) {
  input_df %>% 
    mutate(i.region = ifelse(district_name %in% c("isingiro", "kamwenge", "kikuube", "kyegegwa"), "south_west", "west_nile"),
           i.respondent_age = case_when(respondent_age <= 39 ~ "age_btn_18_39", 
                                        respondent_age <= 59 ~ "age_btn_40_59",
                                        TRUE ~ "age_greater_59"),
           i.respondent_education = case_when(respondent_education %in% c("completed_tertiary") ~ "Higher",
                                              respondent_education %in% c("incomplete_secondary", "complete_primary", "incomplete_primary") ~ "Low",
                                              respondent_education %in% c("complete_secondary", "incomplete_tertiary") ~ "Middle",
                                              respondent_education %in% c("none") ~ "None",
                                              TRUE ~ "Other"
           ),
           
           int.date_arrival_interval = interval(as_date(date_arrival), as_date(as_datetime(today))),
           int.length_since_date_arrival = time_length(int.date_arrival_interval, "year"),
           i.date_arrival = case_when(int.length_since_date_arrival <= 0.25 ~ "last_3_months",
                                      int.length_since_date_arrival <= 1 ~ "more_than_3_months",
                                      TRUE ~ "NA"
           )
           
    ) %>% 
    select(-starts_with("int."))
}
