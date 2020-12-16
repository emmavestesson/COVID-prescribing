library(tidyverse)
library(lubridate)
library(vroom)

#### admissions -----
# read in data

adms_raw <- vroom(here::here('data', 'raw', 
                             'patient_hospital_admissions.csv'))
admitted_before_epic <- adms_raw %>% 
  mutate(start_date=date(start_datetime)) %>% 
  filter(start_date< '2019-04-19')


adms <- adms_raw %>% 
  mutate(start_date=date(start_datetime), 
         end_date=date(end_datetime), 
         los=end_date-start_date, 
         start_dow=weekdays(start_date),
         end_dow=weekdays(end_date),
         end_year_month=floor_date(end_date, unit='month')) %>% 
  add_count(project_id, name = 'number_of_stays') %>% 
  mutate(start_first_day_of_week=floor_date(start_date, 'week', week_start = 1),
         end_first_day_of_week=floor_date(end_date, 'week', week_start = 1), 
         end_datetime=replace_na(end_datetime, max(end_datetime, na.rm = TRUE)),
         hosp_days_adms=date(end_datetime)-date(start_datetime) +1, 
         hosp_days_adms=as.numeric(hosp_days_adms), 
         patient_class_booked=patient_class, 
         patient_class= case_when(hosp_days_adms==1 ~ 'Day Case', 
                                  hosp_days_adms>1 ~ 'Inpatient')) %>% 
  mutate(adms_id=row_number(), .after=project_id)  %>%
  select(-hospital_service) %>% 
  mutate(original_end_date=end_date)

patient_episodes <- read_csv(here::here("data","raw","patient_episodes.csv"))
patient_episodes <- patient_episodes %>% 
  mutate(start_date = lubridate::date(start_datetime))
spect <-patient_episodes %>% 
  group_by(consultant_code) %>% 
  fill(spect_nat_code, spect_nat_name) %>% 
  ungroup() %>% 
  arrange(start_datetime) %>% 
  distinct(project_id, encounter_key, .keep_all = TRUE) %>% 
  select(project_id, encounter_key, tfc_code, tfc_name, directorate)

adms <- left_join(adms, spect, by=c('encounter_key', 'project_id'), suffix=c('_adms', '_episodes')) 


## drop records with multiple entries on the same day that are shorter than 1 hour
adms <- adms %>% 
  mutate(los_minutes = end_datetime-start_datetime) %>% 
  add_count(project_id, start_date) %>% 
  mutate(drop=(n>1 & los_minutes<60)) %>% 
  filter(drop==FALSE) %>% 
  select(-drop, -n)


## drop records with multiple entries with the same start and date and the same treatment function
adms <- adms %>% 
  add_count(project_id, start_date, end_date, tfc_name, name = 'dupes') %>% 
  filter(!(dupes>1)) %>% 
  select(-dupes)
saveRDS(adms, here::here('data', 'admissions.rds'))

