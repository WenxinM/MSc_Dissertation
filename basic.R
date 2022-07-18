Sys.setenv(LANGUAGE='en')
setwd("C:\\Users\\Lenovo\\Desktop\\MSc Stats\\Dissertation\\Data")
library(ipumsr)
library(tidyverse)
library(sjlabelled)

ddi <- read_ipums_ddi("usa_00010.xml")
data <- read_ipums_micro(ddi)

## There are 16 unlablled columns and the 100 labeled columns to be converted 
# to factors. 
data <- data %>% mutate_if(is.labelled, as_factor)

## Deal with missing values
key_missing <- c("N/A", "N/A ", "Not in universe", "Not Applicable", "Unknown", 
                 "Illegible", "Missing", "Not reported", "Missing/blank",
                 "Uncodable", "Not Reported", "Other or not reported", 
                 "Not reported, blank", "N/A or blank",
                 "N/A (Blank)", "Blank", "Unknown", "Illegible")
for (str in key_missing) {
  data <- data %>% 
    mutate(across(where(is.factor), ~na_if(., str)))
}

## Drop FPG related variables because they require extra computation in the unit
# of household -> Household vs individual? 
## SEI (Score for occupations) might be useless.
# useless_col <- c("SAMPLE", "CBSERIAL", "GQ", "HIUFPGBASE", "HIUFPGINC", "HIUID", 
#                  "HIUNPERS", "SEI", "HWSEI", "DEPARTS", "ARRIVES")
useless_col <- c("SAMPLE", "CBSERIAL", "GQ", "SEX", 
                 "YEAR", "SERIAL", "HHWT", "CLUSTER", "STRATA")
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

## export data
install_formats()
library(rio)
export(factorize(data), "fac_2019.csv")


## deal with variables that take mixed values (both categorical and numeric)
# Special case percentage: 0.034
#% for verification
levels(data_lna$COSTELEC)
as.data.frame(table(data_lna$COSTELEC))
sum(is.na(data_lna$COSTELEC))
data_lna %>% nrow() #%

COSTELEC_SP_str <- 
  c("1 or $2 (2000)",
    "No charge or no electricity used (1990, 2000, 2003-onward ACS/PRCS)", 
    "Electricity not used (1970, 1980)", 
    "Electricity included in rent or no charge (1980)",
    "Electricity included in rent (1970)", 
    "Electricity included in rent or in condo fee (1990, 2000, 2003-onward ACS/PRCS)", 
    "No charge, no electricity used, or electricity included in rent or condo fee (2000-2002 ACS)")
data_lna <- data_lna %>% 
  mutate(COSTELEC_SP = ifelse(COSTELEC %in% COSTELEC_SP_str, 1, 
                              ifelse(is.na(COSTELEC), COSTELEC, 0)))
data_lna <- data_lna %>% 
  mutate_at("COSTELEC", ~(ifelse(. %in% COSTELEC_SP_str, 0, 
                                 as.numeric(as.character(.)))))

# special case percentage: 0.87
COSTFUEL_SP_str <- 
  c("1 or $2 (2000)",
    "No charge or no solid or liquid fuel used (1990, 2000, 2003-onward ACS/PRCS) ",
    "Fuel not used (1970, 1980) ",
    "Fuel included in rent or no charge (1980) ",
    "Fuel included in rent (1970) ",
    "Fuel included in rent or in condo fee (1990, 2000, 2003-onward ACS/PRCS) ",
    "No charge, no fuel used, or fuel included in rent or condo fee (2000-2002 ACS)")
data_lna <- data_lna %>% 
  mutate(COSTFUEL_SP = ifelse(COSTFUEL %in% COSTFUEL_SP_str, 1, 
                              ifelse(is.na(COSTFUEL), COSTFUEL, 0)))
data_lna <- data_lna %>% 
  mutate_at("COSTFUEL", ~(ifelse(. %in% COSTFUEL_SP_str, 0, 
                                 as.numeric(as.character(.)))))

# special case percentage:0.48
COSTGAS_SP_str <- c("1 or $2 (2000)",
                    "Included in electricity payment (2003-onward ACS/PRCS) ",
                    "No charge or no gas used (1990, 2000, 2003-onward ACS/PRCS) ",
                    "Gas not used (1970, 1980) ",
                    "Gas included in rent or no charge (1980) ",
                    "Gas included in rent (1970) ",
                    "Gas included in rent or in condo fee (1990, 2000, 2003-onward ACS/PRCS) ",
                    "No charge, none used, or gas included in rent, condo fee, or electricity payment (2000-2002 ACS) ")
data_lna <- data_lna %>% 
  mutate(COSTGAS_SP = ifelse(COSTGAS %in% COSTGAS_SP_str, 1, 
                             ifelse(is.na(COSTGAS), COSTGAS, 0)))
data_lna <- data_lna %>% 
  mutate_at("COSTGAS", ~(ifelse(. %in% COSTGAS_SP_str, 0, 
                                as.numeric(as.character(.)))))

# special case percentage:0.28
COSTWATR_SP_str <- c("1 or $2 (2000)",
                     "No charge or no used (1990, 2000, 2003-onward ACS/PRCS) ",
                     "Water included in rent or no charge (1970, 1980) ",
                     "Water included in rent or in condo fee (1990, 2000, 2003-onward ACS/PRCS) ",
                     "No charge, none used, or water included in rent or condo fee (2000-2002 ACS)")
data_lna <- data_lna %>% 
  mutate(COSTWATR_SP = ifelse(COSTWATR %in% COSTWATR_SP_str, 1, 
                              ifelse(is.na(COSTWATR), COSTWATR, 0)))
data_lna <- data_lna %>% 
  mutate_at("COSTWATR", ~(ifelse(. %in% COSTWATR_SP_str, 0, 
                                 as.numeric(as.character(.)))))
# as.data.frame(table(data_lna$COSTWATR_SP))

## factor to numeric, some may need to adjust for inflation
data_lna <- data_lna %>% 
  mutate_at("HHINCOME", ~as.numeric(as.character(.))*data_lna$CPI99)

data_lna <- 
  data_lna %>% 
  mutate_at("FAMSIZE", ~ifelse(. == "1 family member present", 1,
                               ifelse(. == "2 family members present", 2,
                                      as.numeric(as.character(.)))))
# 173 obs have "9+" children, collapse as "=9"
data_lna <- 
  data_lna %>% 
  mutate_at("NCHILD", ~ifelse(. == "0 children present", 0,
                              ifelse(. == "1 child present", 1,
                                     ifelse(. == "9+", 9, 
                                            as.numeric(as.character(.))))))

# 1 obs has "9+" children, collapse as "=9" !!
data_lna <- data_lna %>% 
  mutate_at("NCHLT5", ~ifelse(. == "No children under age 5", 0,
                              ifelse(. == "1 child under age 5", 1,
                                     ifelse(. == "9+", 9, 
                                            as.numeric(as.character(.))))))
data_lna <- data_lna %>%
  mutate_at("AGE", ~as.numeric(as.character(.)))

data_lna <- data_lna %>% mutate_at("BIRTHYR", ~as.numeric(as.character(.)))

# topcode 99 has frequency 335
data_lna <- 
  data_lna %>% 
  mutate_at("UHRSWORK", ~ifelse(. == "99 (Topcode)", 99,
                                ifelse(is.na(.), .,
                                       as.numeric(as.character(.)))))
data_lna <- 
  data_lna %>% 
  mutate_at("INCTOT", ~ifelse(. %in% c("None", 
                                       "1 or break even (2000, 2005-onward ACS and PRCS)"),
                              0, ifelse(is.na(.), ., as.numeric(as.character(.)))))
data_lna <- data_lna %>% 
  mutate_at("INCTOT", funs(. * data_lna$CPI99))

data_lna <- 
  data_lna %>% 
  mutate_at("FTOTINC", ~ifelse(. == "No income (1950-2000, ACS/PRCS) ", 0, 
                               ifelse(is.na(.), .,
                                      as.numeric(as.character(.)) * data_lna$CPI99 )))

data_lna <- 
  data_lna %>% 
  mutate_at("INCWAGE", ~ifelse(is.na(.), ., 
                               as.numeric(as.character(.)) * data_lna$CPI99))

data_lna <- 
  data_lna %>% 
  mutate_at("INCBUS00", ~ifelse(is.na(.), .,
                                as.numeric(as.character(.)) * data_lna$CPI99))
data_lna <- 
  data_lna %>% 
  mutate_at("INCSS", ~ifelse(is.na(.), .,
                             as.numeric(as.character(.)) * data_lna$CPI99))
data_lna <- 
  data_lna %>% 
  mutate_at("INCWELFR", ~ifelse(is.na(.), .,
                                as.numeric(as.character(.)) * data_lna$CPI99))
data_lna <- 
  data_lna %>% 
  mutate_at("INCEARN", 
            ~ifelse(. %in% c("1 or break even (2000, 2005-2007 ACS and PRCS)", 
                             "No earnings"), 0, 
                    ifelse(is.na(.), .,
                           as.numeric(as.character(.)) * data_lna$CPI99))) 

data_lna <- 
  data_lna %>% 
  mutate(POVERTY_L = 
           ifelse(POVERTY == "1 percent or less of poverty threshold ", 1,
                  ifelse(is.na(POVERTY), POVERTY, 0)))
data_lna <- 
  data_lna %>% 
  mutate(POVERTY_H = 
           ifelse(POVERTY == "501 percent or more of poverty threshold", 1,
                  ifelse(is.na(POVERTY), POVERTY, 0)))
data_lna <- 
  data_lna %>% 
  mutate_at("POVERTY", 
            ~ifelse(. %in% c("1 percent or less of poverty threshold ",
                             "501 percent or more of poverty threshold"), 0,
                    ifelse(is.na(.), .,
                           as.numeric(as.character(.)))))
data_lna <- 
  data_lna %>% 
  mutate_at("TRANTIME", 
            ~ifelse(is.na(.), ., as.numeric(as.character(.))))


## factor to binary
data_lna <- data_lna %>% mutate_at("FOODSTMP", ~ifelse(. == "Yes", 1, 0))

data_lna <- data_lna %>% mutate_at("FERTYR", ~ifelse(. == "Yes", 1, 0))

data_lna <- data_lna %>% 
  mutate_at("HCOVANY", ~ifelse(. == "With health insurance coverage", 1,
                               ifelse(is.na(.), ., 0)))
data_lna <- data_lna %>% 
  mutate_at("HCOVPRIV", ~ifelse(. == "With private health insurance coverage", 1,
                                ifelse(is.na(.), ., 0)))
data_lna <- data_lna %>% 
  mutate_at("HCOVPRIV", ~ifelse(. == "With private health insurance coverage", 1,
                                ifelse(is.na(.), ., 0)))
data_lna <- data_lna %>% 
  mutate_at("HINSEMP", ~ifelse(. == "Has insurance through employer/union", 1,
                               ifelse(is.na(.), ., 0)))
data_lna <- data_lna %>% 
  mutate_at("HINSPUR", ~ifelse(. == "Has insurance purchased directly", 1,
                               ifelse(is.na(.), ., 0)))
data_lna <- data_lna %>% 
  mutate_at("HINSTRI", ~ifelse(. == "Has insurance through TRICARE", 1,
                               ifelse(is.na(.), ., 0)))
data_lna <- data_lna %>% 
  mutate_at("HCOVPUB", ~ifelse(. == "With public health insurance coverage", 1,
                               ifelse(is.na(.), ., 0)))
data_lna <- data_lna %>% 
  mutate_at("HINSCAID", ~ifelse(. == "Has insurance through Medicaid", 1,
                                ifelse(is.na(.), ., 0)))
data_lna <- data_lna %>% 
  mutate_at("HINSCARE", ~ifelse(. == "Yes", 1,
                                ifelse(is.na(.), ., 0)))
data_lna <- data_lna %>% 
  mutate_at("HINSVA", ~ifelse(. == "Has insurance through VA", 1,
                              ifelse(is.na(.), ., 0)))
data_lna <- data_lna %>% 
  mutate_at("HINSIHS", ~ifelse(. == "Has insurance through Indian Health Service", 1,
                               ifelse(is.na(.), ., 0)))
data_lna <- data_lna %>% 
  mutate_at("SCHOOL", ~ifelse(. == "Yes, in school", 1,
                              ifelse(is.na(.), ., 0)))

data_lna <- 
  data_lna %>% 
  mutate_at("DIFFREM", 
            ~ifelse(. == "Has cognitive difficulty", 1,
                    ifelse(is.na(.), ., 0))) 
data_lna <- 
  data_lna %>% 
  mutate_at("DIFFPHYS", 
            ~ifelse(. == "Has ambulatory difficulty", 1,
                    ifelse(is.na(.), ., 0))) 
data_lna <- 
  data_lna %>% 
  mutate_at("DIFFMOB", 
            ~ifelse(. == "Has independent living difficulty", 1,
                    ifelse(is.na(.), ., 0)))
data_lna <- 
  data_lna %>% 
  mutate_at("DIFFCARE", 
            ~ifelse(. == "Yes", 1,
                    ifelse(is.na(.), ., 0)))
data_lna <- 
  data_lna%>% 
  mutate_at("DIFFSENS", 
            ~ifelse(. == "Has vision or hearing difficulty", 1,
                    ifelse(is.na(.), ., 0))) 
data_lna <- 
  data_lna %>% 
  mutate_at("VETSTAT", 
            ~ifelse(. == "Veteran", 1,
                    ifelse(is.na(.), ., 0)))



## list of factors (create dummies)
list_dummy <- c("STATEFIP", "METRO", "OWNERSHP", "MARST", "RACE", "BPL",
                "ANCESTR1", "LANGUAGE", "SPEAKENG", "RACHSING", "EDUC",
                "SCHLTYPE", "EMPSTAT", "LABFORCE", "CLASSWKR", "WKSWORK2",
                "WORKEDYR", "MIGRATE1", "PWSTATE2", "TRANWORK")
## drop variables (some can be helpful if analyzing data on the level of household)
further_drop <-  c("OWNERSHPD", "MULTGEND", "RELATE", "RELATED", "RACED", "BPLD",
                   "ANCESTR1D", "LANGUAGED", "EDUCD", "EMPSTATD", "CLASSWKRD",
                   "MIGRATE1D", "VETSTATD")

## check again >0.4
## duplicate, manually remove? 
## EDUC
## OCC/IND
