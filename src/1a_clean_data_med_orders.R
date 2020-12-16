library(tidyverse)
library(lubridate)
library(vroom)

## MEDICATION ORDERS -----
# read in data

meds_raw <- vroom(here::here('data', 'raw', 'patient_medication_orders.csv'))

bank_holidays <- c('2019-01-01', '2019-04-19',
                   '2019-04-22', '2019-05-06',
                   '2019-05-27', '2019-08-26',
                   '2019-12-25', '2019-12-26', 
                   '2020-01-01', '2020-04-10',
                   '2020-04-13', '2020-05-08',
                   '2020-05-08', '2020-05-25',
                   '2020-08-31', '2020-12-25',
                   '2020-12-28') %>% 
  as.Date()


meds <- meds_raw %>% 
  mutate(start_date=date(start_datetime),
         start_dow=weekdays(start_date),
         start_week=isoweek(start_date),
         start_month= month(start_date),
         start_year=year(start_date), 
         start_year_month=ymd(paste0(start_year,'-', start_month, '-01')),
         working_day=case_when(start_dow %in% c('Saturday', 'Sunday') ~ 0,
                               start_date %in% bank_holidays ~ 0 ,
                               TRUE ~1)) %>% 
  group_by(start_year,start_week) %>% 
  mutate(first_day_of_week=min(start_date)) %>% 
  ungroup() 
meds <- meds %>% 
  # filter(medication_order_mode_name == 'Inpatient') %>% 
  filter(!str_detect(drug_name,'DUMMY DRUG'),
         !str_detect(drug_name,'METHOTREXATE LEVEL'), 
         !str_detect(drug_name, 'BREAST MILK'), 
         !str_detect(drug_name, 'GOSH VENTILATOR ERX'), 
         !str_detect(drug_name, 'DELIVERY'), 
         !str_detect(drug_name, 'TABLET CUTTER'), 
         !str_detect(drug_name, 'ANCILLARIES AND FRIDGE'), 
         !str_detect(drug_name, 'LSD PUMP RENTAL'), 
         !str_detect(drug_name, 'LANCETS'), 
         !str_detect(drug_name, 'TABLET CRUSHER') 
  ) 
  
saveRDS(meds, here::here('data', 'meds.rds'))

inpatient <- meds %>% 
  filter(medication_order_mode_name=='Inpatient') 

saveRDS(inpatient, here::here('data', 'inpatient_meds_orders.rds'))

outpatient <- meds %>% 
  filter(medication_order_mode_name=='Outpatient') 
saveRDS(outpatient, here::here('data', 'outpatient_meds_orders.rds'))
