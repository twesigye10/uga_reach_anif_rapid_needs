library(tidyverse)
library(srvyr)
library(janitor)
library(glue)
library(lubridate)

# source some scripts
source("R/composite_indicators.R")
source("R/make_weights.R")

# load data ---------------------------------------------------------------

df_cleaned <- read_csv("inputs/clean_data_anif.csv")

dap <- read_csv("inputs/r_dap_anif.csv") %>% 
  janitor::clean_names() %>% 
  filter(!variable %in% c("i.date_arrival"), !subset_1 %in% c("i.date_arrival"))

start<- Sys.time() 

# load in individual level population data sets
df_ref_pop <- read_csv("inputs/refugee_population_anif.csv")


# make composite indicator ------------------------------------------------

df_with_composites <- create_composite_indicators_anif(input_df = df_cleaned) %>% 
  mutate(strata = paste0(refugee_settlement, "_refugee"))

# create weights

# refugee weights
ref_weight_table <- make_refugee_weight_table(input_df_ref = df_with_composites, 
                                              input_refugee_pop = df_ref_pop)
df_ref_with_weights <- df_with_composites %>% 
  left_join(ref_weight_table, by = "strata")

# set up design objects ---------------------------------------------------

ref_svy <- as_survey(.data = df_ref_with_weights, strata = strata, weights = weights )


# different analyses ---------------------------------------
outputs <-list()


# refugee -----------------------------------------------------------------

dap_refugee <- dap %>% 
  filter(split %in% c("all", "refugee_only"))

# no subsets

refugee_variables_no_subsets <- dap_refugee %>% 
  pull(variable) %>% unique()

outputs$ref_region <- butteR::survey_collapse(df = ref_svy,
                                              vars_to_analyze = refugee_variables_no_subsets, 
                                              disag = "i.region") %>% 
  mutate(population = "refugee")

# refugee overall, no additional subset
outputs$ref_overall <- butteR::survey_collapse(df = ref_svy,
                                               vars_to_analyze = refugee_variables_no_subsets) %>% 
  mutate(population = "refugee")

#  subsets
dap_refugee_subset1 <- dap %>% 
  filter( split %in%  c("all","refugee_only"), !is.na(subset_1))

# refugee overall, subset 1
dap_refugee_subset_split <- dap_refugee_subset1 %>% 
  split(.$subset_1)

ref_overall_subset1 <-list()

for(i in seq_along(dap_refugee_subset_split)){ 
  print(i)
  subset_temp <- dap_refugee_subset_split[[i]]
  subset_value <- unique(subset_temp$subset_1)
  vars_temp <- subset_temp %>% pull(variable)
  ref_overall_subset1[[subset_value]] <- butteR::survey_collapse(df = ref_svy,
                                                                 vars_to_analyze = vars_temp ,
                                                                 disag = c( subset_value) 
  )
}

outputs$ref_overall_subset1 <- bind_rows(ref_overall_subset1) %>% 
  mutate(population = "refugee")

# refugee overall by region & subset 1
ref_region_subset1 <- list()

for(i in seq_along(dap_refugee_subset_split)){ 
  print(i)
  subset_temp <-dap_refugee_subset_split[[i]]
  subset_value <- unique(subset_temp$subset_1)
  vars_temp <- subset_temp %>% pull(variable)
  ref_region_subset1[[subset_value]] <- butteR::survey_collapse(df = ref_svy,
                                                                vars_to_analyze = vars_temp ,
                                                                disag = c( "i.region", subset_value) 
  )
}
outputs$ref_region_subset1 <- bind_rows(ref_region_subset1) %>% 
  mutate(population = "refugee")

# merge analysis ----------------------------------------------------------

full_analysis_long <- bind_rows(outputs)
end<- Sys.time()
end-start

full_analysis_long %>%
  write_csv(paste0("outputs/", butteR::date_file_prefix(), "_full_analysis_lf.csv"),na="")


