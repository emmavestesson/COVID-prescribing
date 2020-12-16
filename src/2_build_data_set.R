library(tidyverse)
library(lubridate)
library(vroom)

### set start of COVID ----

covid_start <- '2020-03-23'

## read in data ----
adms_long <- readRDS(here::here('data','admissions_long.rds'))
meds <-  readRDS(here::here('data','meds.rds'))
inpatient <- readRDS(here::here('data','inpatient_meds_orders.rds'))

tfc_name_keep <- adms_long %>% 
  distinct(adms_id, tfc_name) %>% 
  count(tfc_name) %>% 
  slice_max(order_by = n,n= 20) %>% 
  pull(tfc_name)

adms_long <- adms_long %>% 
  mutate(tfc_name= case_when(tfc_name %in% tfc_name_keep ~ tfc_name, 
                              TRUE ~ 'Other/missing'))

hospital_occupancy_daily_tfc <- adms_long %>% 
  filter(date>= '2019-04-23') %>% 
  count(date, patient_class, tfc_name,  name = 'patient_days')

saveRDS(hospital_occupancy_daily_tfc, 'hospital_occupancy_daily_tfc.rds')


# clean meds data
meds <- inpatient %>% 
  filter(!str_detect(drug_name,'DUMMY DRUG'),
         !str_detect(drug_name,'METHOTREXATE LEVEL'), 
         !str_detect(drug_name, 'BREAST MILK'), 
         !str_detect(drug_name, 'GOSH VENTILATOR ERX'), 
         !str_detect(drug_name, 'DELIVERY'), 
         !str_detect(drug_name, 'TABLET CUTTER'), 
         !str_detect(drug_name, 'ANCILLARIES AND FRIDGE'), 
         !str_detect(drug_name, 'TABLET CRUSHER') 
         ) %>% 
  mutate(drug_therapeutic_class_name=if_else(str_detect(drug_name, 'PN') & is.na(drug_therapeutic_class_name), 'Parenteral nutrition',drug_therapeutic_class_name ),
         drug_therapeutic_class_name=if_else(str_detect(drug_name, 'PARENTERAL') & is.na(drug_therapeutic_class_name), 'Parenteral nutrition',drug_therapeutic_class_name ) ) %>% 
  mutate(drug_pharmaceutical_class_name=if_else(str_detect(drug_name, 'PN') & is.na(drug_pharmaceutical_class_name), 'Parenteral nutrition',drug_pharmaceutical_class_name )) %>% 
  mutate(drug_pharmaceutical_subclass_name=if_else(str_detect(drug_name, 'PN') & is.na(drug_pharmaceutical_subclass_name), 'Parenteral nutrition',drug_pharmaceutical_subclass_name )) %>% 
  filter(!(drug_therapeutic_class_name %in% c('Parenteral nutrition', 'Miscellaneous aids & materials for diagnostic procedures', 'Ingredient','Hypodermic equipment')) | is.na(drug_therapeutic_class_name))


## Only keep unique rows based on project_id, patient_class, tfc_name, date
adms_long <- adms_long %>% 
  distinct(adms_id, project_id, patient_class, tfc_name, directorate, admission_type, admission_source, principal_problem, date, start_datetime, end_datetime, hosp_days_adms, original_end_date)

# join any inpatient medication orders so that I know what speciality they are linked to. 
meds_adms_big <- left_join(adms_long, meds, by=c('project_id', 'date'='start_date'), 
                           suffix=c('_adms', '_meds'))

# join any inpatient medication orders so that I know what speciality they are linked to. 

meds_adms <- meds_adms_big %>% 
  group_by(adms_id) %>% 
  mutate(adm_no_drugs=is.infinite(max(start_datetime_meds, na.rm = TRUE))) %>% 
  ungroup() 

### add theatre

theatre <- readRDS(here::here('data', 'theatre_activity.rds'))

theatre_not_cancelled <- theatre %>% 
  filter(procedure_cancelled==0 & procedure_not_performed==0) 

# I want to add adms_id to the theatre data so I only care about the records that actually match
adms_long_theatre <- inner_join(adms_long, theatre_not_cancelled, by=c('project_id', 'date'='start_date'), 
                  suffix=c('_adms', '_theatre'))

adms_theatre  <- adms_long_theatre
theatre_daily <- adms_theatre %>% 
  count(date,patient_class, name = 'theatre_encounter') 

saveRDS(theatre_daily, here::here('data', 'theatre_daily.rds'))
saveRDS(adms_theatre, here::here('data', 'adms_theatre.rds'))

theatre_visits_per_adm <- adms_theatre %>% 
  group_by(adms_id) %>% 
  summarise(theatre_booked_per_adms=n(), theatre_performed_per_adms=sum(number_of_procedures)) 



meds_adms <- left_join(meds_adms, theatre_visits_per_adm, by=c('adms_id'))
meds_adms <- meds_adms %>% 
  mutate(theatre_booked_per_adms=replace_na(theatre_booked_per_adms,0 ), 
         theatre_performed_per_adms=replace_na(theatre_performed_per_adms,0 )) %>% 
  mutate(theatre_procedure_ever=theatre_performed_per_adms>0) %>% 
  select(-theatre_booked_per_adms)
## add age at admission and sex (demographics) ----

demographics <- vroom::vroom(here::here('data','raw', 'patient_demographics.csv'))

meds_adms <- left_join(meds_adms, demographics, by='project_id') %>% 
  mutate(age_years= lubridate::time_length(date(start_datetime_adms)-date_of_birth,unit = 'years'), 
         age_years=ifelse(age_years<0, 0, age_years)) 

meds_adms <- meds_adms %>% 
  filter(age_years<=25)

saveRDS(meds_adms,here::here('data', '2_clean_data_one_row_per_day.rds'))

one_row_per_admission <- meds_adms %>% 
  group_by(adms_id, project_id, patient_class, tfc_name, admission_type, admission_source, start_datetime_adms, end_datetime_adms,original_end_date, sex, age_years, principal_problem, theatre_performed_per_adms,
           theatre_procedure_ever, hosp_days_adms) %>% 
  summarise(total_drugs_ordered=sum(!is.na(start_datetime_meds)), 
            total_drugs_admin = sum(admins_this_period, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(los_days=lubridate::time_length(end_datetime_adms-start_datetime_adms,unit = 'days'), 
         los_minutes=lubridate::time_length(end_datetime_adms-start_datetime_adms,unit = 'minute'), 
         drugs_per_los=total_drugs_ordered/los_days,
         covid=start_datetime_adms>=covid_start)

one_row_per_admission <- one_row_per_admission %>% 
 mutate(across(c(admission_type, admission_source), as.factor),  
        across(c(admission_type, admission_source), .fns=NULL,  .names="{.col}_orig"),
   admission_type=fct_lump_min(admission_type, 1000, other_level='Other/missing'), 
        admission_source=fct_lump_min(admission_source, 1000, other_level='Other/missing'))

## Save data with one row per admission ----

saveRDS(one_row_per_admission,here::here('data', '1_clean_data_one_row_per_adms.rds'))


#### build data set with medication administrations ----
meds_given <- readRDS(here::here('data','medication_administrations.rds'))

meds_given <- meds_given %>%
filter(!str_detect(drug_name,'DUMMY DRUG'),
       !str_detect(drug_name,'METHOTREXATE LEVEL')) %>%
  mutate(drug_therapeutic_class_name=if_else(str_detect(drug_name, 'PN') & is.na(drug_therapeutic_class_name), 'Parenteral nutrition',drug_therapeutic_class_name ),
         drug_therapeutic_class_name=if_else(str_detect(drug_name, 'PARENTERAL') & is.na(drug_therapeutic_class_name), 'Parenteral nutrition',drug_therapeutic_class_name ) ) %>%
  mutate(drug_pharmaceutical_class_name=if_else(str_detect(drug_name, 'PN') & is.na(drug_pharmaceutical_class_name), 'Parenteral nutrition',drug_pharmaceutical_class_name )) %>%
  mutate(drug_pharmaceutical_subclass_name=if_else(str_detect(drug_name, 'PN') & is.na(drug_pharmaceutical_subclass_name), 'Parenteral nutrition',drug_pharmaceutical_subclass_name ))


# join any medication orders so that I know what speciality they are linked to.
admissions_meds_given_big <- left_join(adms_long, meds_given, by=c('project_id', 'date'='start_date'),
                           suffix=c('_adms', '_meds_given'))
# Filter based on time (second step after joining on project_id and date ( CHECK WHY NOT OK)
admissions_meds_given <- admissions_meds_given_big %>%
  filter((start_datetime_meds_given>=start_datetime_adms & start_datetime_meds_given<= end_datetime_adms) | is.na(start_datetime_meds_given))

admissions_meds_given <- admissions_meds_given %>% 
  group_by(adms_id) %>% 
  mutate(adm_no_drugs=is.infinite(max(start_datetime_meds_given, na.rm = TRUE))) %>% 
  ungroup() 


admissions_meds_given <- left_join(admissions_meds_given, demographics, by='project_id') %>%
  mutate(age_years= lubridate::time_length(date(start_datetime_adms)-date_of_birth,unit = 'years'),
         age_years=ifelse(age_years<0, 0, age_years))

saveRDS(admissions_meds_given, here::here('data', '3_admissions_meds_given.rds'))

hospital_occupancy_daily <- adms_long %>% 
  count(date, patient_class, name = 'patient_days')

meds_given_daily <-  admissions_meds_given %>% 
  count(patient_class,  date, name='meds_given') %>% 
  right_join(hospital_occupancy_daily, by = c('date', 'patient_class')) %>% 
  mutate(meds_given_per_pd = meds_given/patient_days)


saveRDS(meds_given_daily, here::here('data' ,'meds_given_daily.RDS'))



admissions_dtc_wide <- admissions_meds_given %>% 
  mutate(drug_therapeutic_class_name=fct_lump_min(drug_therapeutic_class_name, min=1000, other_level='Other/missing'),
         drug_therapeutic_class_name=fct_explicit_na(drug_therapeutic_class_name, na_level='Other/missing')) %>% 
  distinct( adms_id, project_id,hosp_days_adms, patient_class, tfc_name,   drug_therapeutic_class_name, date) %>%
  mutate(count=1) %>% 
  pivot_wider(id_cols=c(adms_id, project_id,hosp_days_adms, patient_class, tfc_name, date ), names_from = drug_therapeutic_class_name, 
              names_prefix = 'dtc_',
              values_from=count, 
              values_fill=0, 
              names_sort=TRUE) %>% 
  janitor::clean_names() 

dtc_daily <- admissions_dtc_wide %>% 
  group_by(date, patient_class) %>% 
  summarise(across(starts_with('dtc_'), sum)) %>% 
  ungroup() %>% 
  right_join(hospital_occupancy_daily,  by = c('date', 'patient_class')) %>% 
  relocate(patient_days, .after = patient_class) %>% 
  mutate(across(starts_with('dtc_'), list('per_1000_pd'=~./patient_days*1000))) %>% 
  mutate(covid=date>=covid_start) %>% 
  select(date, patient_class,covid,  contains('per_1000_pd')) 

saveRDS(dtc_daily, here::here('data' , 'dtc_daily_meds_given_DOT.RDS'))


admissions_dtc_admin_wide <- admissions_meds_given %>% 
  mutate(drug_therapeutic_class_name=fct_lump_min(drug_therapeutic_class_name, min=1000, other_level='Other/missing'),
         drug_therapeutic_class_name=fct_explicit_na(drug_therapeutic_class_name, na_level='Other/missing')) %>% 
  count(patient_class, tfc_name,   drug_therapeutic_class_name, date, name = 'count') %>%
  pivot_wider(id_cols=c(patient_class, tfc_name, date ), names_from = drug_therapeutic_class_name, 
              names_prefix = 'dtc_',
              values_from=count, 
              values_fill=0, 
              names_sort=TRUE) %>% 
  janitor::clean_names() 


dtc_daily_admin <- admissions_dtc_admin_wide %>% 
  group_by(date, patient_class) %>% 
  summarise(across(starts_with('dtc_'), sum)) %>% 
  ungroup() %>% 
  right_join(hospital_occupancy_daily,  by = c('date', 'patient_class')) %>% 
  relocate(patient_days, .after = patient_class) %>% 
  mutate(across(starts_with('dtc_'), list('per_1000_pd'=~./patient_days*100))) %>% 
  mutate(covid=date>=covid_start) %>% 
  select(date, patient_class,covid,  contains('per_1000_pd')) 

saveRDS(dtc_daily_admin, here::here('data' , 'dtc_daily_meds_given.RDS'))


admissions_dpc_wide <- admissions_meds_given %>% 
  distinct( adms_id, project_id,hosp_days_adms, patient_class, tfc_name,  drug_pharmaceutical_class_name, date) %>%
  mutate(count=1) %>% 
  pivot_wider(id_cols=c(adms_id, project_id,hosp_days_adms, patient_class, tfc_name, date ), names_from = drug_pharmaceutical_class_name, 
              names_prefix = 'dpc_',
              values_from=count, 
              values_fill=0, 
              names_sort=TRUE) %>% 
  janitor::clean_names() 



dpc_daily <- admissions_dpc_wide %>% 
  group_by(date, patient_class) %>% 
  summarise(across(starts_with('dpc_'), sum)) %>% 
  ungroup() %>% 
  right_join(hospital_occupancy_daily,  by = c('date', 'patient_class')) %>% 
  relocate(patient_days, .after = patient_class) %>% 
  mutate(across(starts_with('dpc_'), list('per_1000_pd'=~./patient_days*1000))) %>% 
  mutate(covid=date>=covid_start) %>% 
  select(date, patient_class,covid,  contains('per_1000_pd')) 

saveRDS(dpc_daily, here::here('data' ,'dpc_daily_meds_given.RDS'))
