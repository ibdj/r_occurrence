library(tidyverse) #general data management
library(googlesheets4) #to import directly from google sheets
library(readr) #to use the write_rds function

read_sheet('https://docs.google.com/spreadsheets/d/1adm-3HKTyeDMY-WPOWWSKN37hGuj5EWNMjJIh3D6tkc/edit#gid=0', sheet = 1) %>% 
  pivot_longer(cols = 4:ncol(.),
               names_to = "location",
               values_to = "date") %>%
  drop_na() %>% 
  mutate(
    status = "present"
  ) %>% 
  write_rds(file.path("data-raw","daniels1970.rds"))
