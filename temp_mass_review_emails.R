library(skimmr)
library(tidyverse)
library(readxl)
library(writexl)

get_param(date_currel = "2023-12-01")


###


# this method using base R import takes only 373 sec with May 2022 release
data <- read.ebd(path_ebd_main, preimp) 

# # tidy import takes way longer, a total of 877 sec, but could be useful for smaller data
# data <- read_delim(path_ebd_main, col_select = preimp,
#                    name_repair = make.names, # base R nomencl. with periods for spaces
#                    quote = "", na = c(""," ", NA), show_col_types = F)


### sensitive species ###
senssp <- read.ebd(senspath, preimp)


data_unv <- read.ebd("EBD/ebd_IN_unv_smp_relDec-2023_unvetted.txt", preimp) 


### combing the two ###
data <- bind_rows(data, senssp) %>% 
  bind_rows(data_unv) %>% 
  # filtering unvetted as well as exotic species (provisional and escapee)
  filter(!(EXOTIC.CODE %in% c("P", "X")))


### adding useful columns ###
data <- data %>% 
  # trimming whitespace in breeding codes
  mutate(BREEDING.CODE = str_trim(BREEDING.CODE)) %>% 
  # group ID and dates
  mutate(GROUP.ID = ifelse(is.na(GROUP.IDENTIFIER), SAMPLING.EVENT.IDENTIFIER, GROUP.IDENTIFIER), 
         OBSERVATION.DATE = as.Date(OBSERVATION.DATE), 
         YEAR = year(OBSERVATION.DATE), 
         MONTH = month(OBSERVATION.DATE),
         DAY.M = day(OBSERVATION.DATE)) %>% 
  # migratory year and month information
  mutate(M.YEAR = if_else(MONTH > 5, YEAR, YEAR-1), # from June to May
         M.MONTH = if_else(MONTH > 5, MONTH-5, 12-(5-MONTH))) 


###

users_emails <- read_csv("eBirders_IN_data.csv") %>% 
  mutate(user_id = str_replace_all(user_id, "USER", "obsr")) %>% 
  distinct(user_id, email) %>% 
  magrittr::set_colnames(c("OBSERVER.ID", "EMAIL"))

eBird_users <- read.delim(userspath, sep = "\t", header = T, quote = "", 
                          stringsAsFactors = F, na.strings = c(""," ",NA)) %>% 
  transmute(OBSERVER.ID = observer_id,
            FULL.NAME = paste(first_name, last_name, sep = " ")) %>% 
  left_join(users_emails, by = "OBSERVER.ID") %>% 
  filter(!is.na(EMAIL))



# CHC from UK
data_uk_chc <- data %>% 
  filter(STATE.CODE == "IN-UL") %>% 
  group_by(OBSERVER.ID) %>% 
  filter(COMMON.NAME == "Common Hawk-Cuckoo") %>% 
  reframe(CONFIRMED.ONLY = case_when(any(APPROVED == 0) ~ FALSE,
                                    TRUE ~ TRUE)) %>% 
  ungroup() 

data_uk_hipi <- data %>% 
  filter(STATE.CODE == "IN-UL") %>% 
  group_by(OBSERVER.ID) %>% 
  filter(COMMON.NAME == "Hill Pigeon") %>% 
  reframe(CONFIRMED.ONLY = case_when(any(APPROVED == 0) ~ FALSE,
                                    TRUE ~ TRUE)) %>% 
  ungroup() 

data_uk_rcdo <- data %>% 
  filter(STATE.CODE == "IN-UL") %>% 
  group_by(OBSERVER.ID) %>% 
  filter(COMMON.NAME == "Red Collared-Dove") %>% 
  reframe(CONFIRMED.ONLY = case_when(any(APPROVED == 0) ~ FALSE,
                                    TRUE ~ TRUE)) %>% 
  ungroup() 

data_uk_aps <- data %>% 
  filter(STATE.CODE == "IN-UL") %>% 
  group_by(OBSERVER.ID) %>% 
  filter(COMMON.NAME == "Asian Palm Swift") %>% 
  reframe(CONFIRMED.ONLY = case_when(any(APPROVED == 0) ~ FALSE,
                                    TRUE ~ TRUE)) %>% 
  ungroup() 


# InCo from NE
data_ne_inco <- data %>% 
  filter(STATE.CODE %in% c("IN-SK", "IN-AS", "IN-AR", "IN-NL", "IN-ML",
                           "IN-MN", "IN-MZ", "IN-TR") |
           COUNTY.CODE %in% c("IN-WB-JA", "IN-WB-KA", "IN-WB-DA", "IN-WB-AL")) %>% 
  group_by(OBSERVER.ID) %>% 
  filter(COMMON.NAME == "Indian Cormorant") %>% 
  reframe(CONFIRMED.ONLY = case_when(any(APPROVED == 0) ~ FALSE,
                                    TRUE ~ TRUE)) %>% 
  ungroup()

data_unv %>% 
  filter(STATE.CODE == "IN-WB",
         COMMON.NAME == "Indian Cormorant") %>% 
  distinct(SAMPLING.EVENT.IDENTIFIER, COUNTY.CODE)


write_xlsx(path = "emails.xlsx",
           x = list("uk_chcu" = data_uk_chc,
                    "uk_hipi" = data_uk_hipi,
                    "uk_rcdo" = data_uk_rcdo,
                    "uk_apsw" = data_uk_aps,
                    "ne_inco" = data_ne_inco))
