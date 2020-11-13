library(tidyverse)
library(data.table)
library(inspectdf)

df <- fread('loan_data_2007_2014.csv',
            na.strings = c(NA, ""))
#read NA and "" as strings

df %>% select(-V1) -> df 
df %>%
  select_if(is.numeric) %>%
  map_df(~ unique(.) %>% length()) %>%
  gather() %>%
  arrange(value) %>%
  mutate(key = as_factor(key)) %>% 
  filter(value<7) %>% 
  arrange(desc(value)) %>% 
  pull(key)
