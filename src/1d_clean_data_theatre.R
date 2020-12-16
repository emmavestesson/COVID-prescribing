library(tidyverse)
library(lubridate)

theatre_raw <- read_csv(here::here('data', 'raw', 'patient_theatre_list.csv'))

theatre_summary <- theatre_raw %>% 
  summarise(across(everything(), n_distinct))
theatre_na <- theatre_raw %>% 
  summarise(across(everything(), ~sum(is.na(.))))
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
theatre <- theatre_raw %>% 
  mutate(start_date=date(start_datetime),
         start_dow=weekdays(start_date),
         start_year_month=floor_date(start_date, unit='month'),
         first_day_of_week=floor_date(start_date, unit='week', week_start = 1),
         working_day=case_when(start_dow %in% c('Saturday', 'Sunday') ~ 0,
                               start_date %in% bank_holidays ~ 0 ,
                               TRUE ~1),
         procedure_length =lubridate::time_length(end_datetime-start_datetime,unit = 'minute') )  %>% 
  filter(procedure_length!=0)


saveRDS(theatre, here::here('data', 'theatre_activity.rds'))
