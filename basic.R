Sys.setenv(LANGUAGE='en')
setwd("C:\\Users\\Lenovo\\Desktop\\MSc Stats\\Dissertation\\Data")
library(ipumsr)
library(tidyverse)
library(sjlabelled)

ddi <- read_ipums_ddi("usa_00007.xml")
data <- read_ipums_micro(ddi)

## There are 16 unlablled columns and the 100 labeled columns to be converted 
# to factors. 
data <- data %>% mutate_if(is.labelled, as_factor)

## Deal with missing values
key_missing <- c("N/A", "N/A ", "Not in universe", "Not Applicable", "Unknown", 
                 "Illegible", "Missing", "Not reported")
for (str in key_missing) {
  data <- data %>% 
    mutate(across(where(is.factor), ~na_if(., str)))
}

## Drop FPG related variables because they require extra computation in the unit
# of household -> Household vs individual? 
## SEI (Score for occupations) might be useless.
useless_col <- c("SAMPLE", "CBSERIAL", "GQ", "HIUFPGBASE", "HIUFPGINC", "HIUID", 
                 "HIUNPERS", "SEI", "HWSEI", "DEPARTS", "ARRIVES")
data <- data[,!(names(data) %in% useless_col)]

## Convert MOMLOC, POPLOC, SPLOC to indicator: = 1 if the person of interest 
# (e.g. the person's mother) lived in the same household. 
data <- data %>% mutate(MOMLOC = ifelse(MOMLOC != 0, 1, 0))
data <- data %>% mutate(POPLOC = ifelse(POPLOC != 0, 1, 0))
data <- data %>% mutate(SPLOC = ifelse(SPLOC != 0, 1, 0))


## keep variables with missing values <= 40% and no missing values
data %>% 
  summarise_all(~ sum(is.na(.)) / nrow(data)) %>%
  select_if(.<= 0.4)
lna_list <- 
  data %>% 
  summarise_all(~ sum(is.na(.)) / nrow(data)) %>%
  select_if(.<= 0.4) %>% 
  colnames()
data_lna <- data %>% 
  select(lna_list)
data_lna # 83

nna_list <- 
  data %>% 
  summarise_all(~ sum(is.na(.)) / nrow(data)) %>%
  select_if(.== 0) %>% 
  colnames()
data_nna <- data %>% 
  select(nna_list)
data_nna # 60

## y-variable
data %>% 
  summarise_all(~ sum(is.na(.)) / nrow(data)) %>% 
  select(FERTYR, NCHILD, NCHLT5, ELDCH, YNGCH)