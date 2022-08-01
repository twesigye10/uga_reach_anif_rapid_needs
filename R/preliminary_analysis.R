library(tidyverse)
library(srvyr)
library(janitor)
library(glue)



# load data ---------------------------------------------------------------

df_cleaned <- read_csv("outputs/clean_data_anif.csv")

dap <- read_csv("outputs/r_dap.csv") %>% 
  janitor::clean_names()

start<- Sys.time() 

# load in individual level population data sets
df_ref_pop <- read_csv("inputs/refugee_population.csv")


# make composite indicator ------------------------------------------------

df_with_composites <- create_composite_indicators_anif(input_df = df_cleaned) %>% 
  mutate(strata = case_when(status == "refugee" ~ paste0(i.refugee_settlement, "_refugee"),
                            TRUE ~ status
  ))

# different analyses ---------------------------------------
outputs <-list()








