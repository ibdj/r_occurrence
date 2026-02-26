
library(tidyverse) #general data management
library(googlesheets4) #to import directly from google sheets
library(readr) #to use the write_rds function

read_sheet('https://docs.google.com/spreadsheets/d/1CyLKuOP-YKJEXYNP9AdInznvHkJmOrjrr1t9Mve0FAg/edit#gid=142878456', sheet = 2) %>% 
  select(
    spe_no, 
    scientificname,
    verbatim_loc,
    search
  ) %>% 
  write_rds(file.path("data-raw","tvposeg1933.rds"))

tfomb1943_loc<-read_sheet('https://docs.google.com/spreadsheets/d/1eCoCCZs8harPXOhajLo4jgfCnkN1RUuqrX_lm-Kdqxc/edit#gid=874720369') %>% 
  select(
    location_no,
    verbatimLocality,
    decimalLatitude,
    decimalLongitude
  ) 




read_sheet('https://docs.google.com/spreadsheets/d/1Go_5CFsCp8wScSjvItqRM834988wEEYzE2SfBWR9YDs/edit#gid=0') %>% 
  select(no,
         genus,
         specificepithet,
         s1,
         s2,
         s3,
         s4,
         t1,
         t2,
         t3,
         t4,
         t5,
         t6,t7,t8,t9,
         a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,n1,n2,n3,n4,n5,n6,n7,n8,y1,y2,y3,y4,y5,y6,y7,y8,y9) %>% 
  pivot_longer(cols = 4:ncol(.),
               names_to = "location",
               values_to = "occurrence"
  ) %>%
  mutate(
    occurrence = as.character(occurrence),
    occurrence = if_else(occurrence == "x","present","absent") 
    )%>% 
  write_rds(file.path("data-raw","kruuse1902.rds"))


read_sheet('https://docs.google.com/spreadsheets/d/1enf76zz9EBApZVkqWaT1F826Y46OJ5NTKqB8nbGrtug/edit#gid=1166476565', sheet = 'data', skip = 1) %>%
  write_rds(file.path("data-raw","ginr_herbarium.rds"))
