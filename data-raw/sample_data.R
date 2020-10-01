## code to prepare `sample_data` dataset goes here

library(tidyverse)
library(lubridate)

## sample_fixed_data

set.seed(1)
sample_fixed_data =
  tibble(id = 1:1000) %>%
  mutate(sex = sample(c('male', 'female'), 1000, replace = TRUE)) %>%
  mutate(age = rnorm(n = 1000, mean = 65, sd = 15)) %>%
  mutate(race = sample(c('asian', 'black', 'white', 'multiracial', 'other'), 1000, replace = TRUE)) %>%
  mutate(baseline_cr =
           case_when(sex == 'male' ~ rnorm(n = n(), mean = 1, sd = 0.1),
                     sex == 'female' ~ rnorm(n = n(), mean = 0.8, sd = 0.1))) %>%
  mutate(admit_time = sample(as_datetime(mdy_hms('1-1-2019 00:00:00'):mdy_hms('12-31-2019 23:59:00')), 1000, replace=TRUE)) %>%
  mutate(dc_time = admit_time +
           hours(sample(8:167, n(), replace=TRUE)) +
                   minutes(sample(0:59, n(), replace = TRUE))) %>%
  as.data.frame()

usethis::use_data(sample_fixed_data, overwrite = TRUE)

## sample_temporal_data

set.seed(2)
sample_temporal_data =
  tibble(id = sample(1:1000, 100000, replace = TRUE)) %>%
  left_join(., sample_fixed_data %>% select(id, admit_time, baseline_cr, sex, age)) %>%
  arrange(id) %>%
  group_by(id) %>%
  mutate(hours_to_add = cumsum(sample(0:6, n(), replace = TRUE))) %>%
  mutate(mins_to_add = cumsum(sample(0:59, n(), replace = TRUE))) %>%
  mutate(time = admit_time - hours(sample(1:12, n(), replace = TRUE)) +
                                      hours(hours_to_add) + minutes(mins_to_add)) %>%
  ungroup() %>%
  mutate(cr =
           case_when(sex == 'male' ~ baseline_cr + rnorm(n = n(), mean = 0.05, sd = 0.10) + age*0.0005,
                     sex == 'female' ~ baseline_cr + rnorm(n = n(), mean = -0.05, sd = 0.10) + age*0.0001)) %>%
  mutate(cr_abnl =
           case_when(cr < 0.3 ~ 'low',
                     cr >= 1.3 ~ 'high',
                     TRUE ~ 'normal')) %>%
  mutate(cr_high =
           case_when(cr >= 1.3 ~ 'yes',
                     TRUE ~ 'no')) %>%
  select(-admit_time, -baseline_cr, -sex, -age, -hours_to_add, -mins_to_add) %>%
  gather(variable, value, cr, cr_abnl, cr_high) %>%
  mutate(category =
           case_when(variable == 'cr' ~ 'lab',
                     TRUE ~ 'flag')) %>%
  as.data.frame()

set.seed(3)
medications =
  tibble(id = sample(1:1000, 10000, replace = TRUE)) %>%
  left_join(., sample_fixed_data %>% select(id, admit_time, baseline_cr, sex, age)) %>%
  arrange(id) %>%
  group_by(id) %>%
  mutate(hours_to_add = cumsum(sample(12:23, n(), replace = TRUE))) %>%
  mutate(mins_to_add = cumsum(sample(0:59, n(), replace = TRUE))) %>%
  mutate(time = admit_time - hours(sample(1:12, n(), replace = TRUE)) +
           hours(hours_to_add) + minutes(mins_to_add)) %>%
  ungroup() %>%
  mutate(variable = 'med') %>%
  mutate(value =
           sample(c('aspirin', 'acetaminophen', 'diphenhydramine'), n(), replace = TRUE)) %>%
  mutate(category = 'medications') %>%
  select(id, time, variable, value, category) %>%
  arrange(id, time)

sample_temporal_data =
  bind_rows(sample_temporal_data, medications) %>%
  arrange(id, time)

usethis::use_data(sample_temporal_data, overwrite = TRUE)
