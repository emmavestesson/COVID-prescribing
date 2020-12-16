library(tidyverse)
library(lubridate)
# read in data

adms <- readRDS(here::here('data','admissions.rds'))

adms <- adms %>% 
  select(-ends_with('week'), -contains('dow'), -ends_with('day')) %>% 
  mutate(original_end_date=end_date)

max_start_date <- max(adms$start_date)
one_day_stay <- adms %>%
  mutate( end_date=if_else(is.na(end_date), max_start_date,end_date )) %>% 
  filter(end_date-start_date==0)%>% 
  mutate(date=start_date)

multiple_days_stay <- adms %>% 
  mutate(end_date=if_else(is.na(end_date), max_start_date,end_date )) %>% 
  filter(end_date-start_date>0) 

adms_long <- multiple_days_stay %>% 
  pivot_longer(c(start_date, end_date), names_to='type', values_to = 'date') %>% 
  group_by(adms_id, project_id, encounter_key, start_datetime, end_datetime,original_end_date, 
           patient_class, tfc_code, tfc_name, admission_type, admission_source,hosp_days_adms, principal_problem) %>% 
  complete(date=seq(min(date), max(date), by = "day")) 

occupancy <- bind_rows(adms_long, one_day_stay) %>% 
  ungroup()

##  save data ---
saveRDS(occupancy, here::here('data', 'admissions_long.rds'))
