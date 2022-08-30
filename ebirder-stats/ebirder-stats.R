# need spreadsheet that includes checklist links 

library(tidyverse)
library(lubridate)


# data ------------------------------------------------------------------------------

# load data
load("EBD/ebd_IN_relJul-2022.RData")

data0 <- data %>% 
  group_by(SAMPLING.EVENT.IDENTIFIER) %>% 
  slice(1) %>% 
  ungroup()

# form responses with checklist links
responses <- read_csv("ebirder-stats/names_lists.csv") %>% 
  filter(!is.na(Name)) %>% 
  select(-c(2:7, 10)) %>% 
  rename_with(~ toupper(.x) %>% str_replace(pattern = " ", replacement = ".")) %>% 
  separate(CHECKLIST.URL, c("PRE", "SAMPLING.EVENT.IDENTIFIER"), sep = "checklist/") %>% 
  select(NAME, SAMPLING.EVENT.IDENTIFIER)

# lists specifically from before Aug (for people who ended up with NA in "responses" object)
responses2 <- read_csv("ebirder-stats/names_lists_2.csv") %>% 
  left_join(responses) %>% # to join names
  select(c(13, 17)) %>% 
  rename(CHECKLIST.URL = ...13) %>% 
  separate(CHECKLIST.URL, c("PRE", "SAMPLING.EVENT.IDENTIFIER"), sep = "checklist/") %>% 
  select(NAME, SAMPLING.EVENT.IDENTIFIER) %>% 
  filter(!is.na(SAMPLING.EVENT.IDENTIFIER))

# ebird user info
eBird_users <- read.delim("EBD/ebd_users_relMay-2022.txt", sep = "\t", header = T, quote = "", 
                          stringsAsFactors = F, na.strings = c(""," ",NA)) %>% 
  transmute(OBSERVER.ID = observer_id,
            EBIRD.NAME = paste(first_name, last_name, sep = " "))


# wrangling -------------------------------------------------------------------------

# checklist and user IDs
temp <- responses %>% 
  left_join(data0) %>% 
  distinct(SAMPLING.EVENT.IDENTIFIER, OBSERVER.ID, NAME) %>% 
  left_join(eBird_users)

ids <- responses2 %>% 
  left_join(data0) %>% 
  distinct(SAMPLING.EVENT.IDENTIFIER, OBSERVER.ID, NAME) %>% 
  left_join(eBird_users) %>% 
  bind_rows(temp) %>% 
  # retaining users with no info but removing users with extra NA row
  group_by(NAME) %>% 
  arrange(OBSERVER.ID) %>% 
  slice(1) %>% 
  ungroup() %>% 
  distinct(OBSERVER.ID, NAME, EBIRD.NAME)

data_filt <- data %>% filter(OBSERVER.ID %in% ids$OBSERVER.ID)
data0_filt <- data0 %>% filter(OBSERVER.ID %in% ids$OBSERVER.ID)

# removing full data objects
rm(data, data0)


# oldest and latest lists
data1 <- data0_filt %>% 
  mutate(LAST.EDITED.DATE = as_date(LAST.EDITED.DATE),
         OBSERVATION.DATE = as_date(OBSERVATION.DATE)) %>% 
  group_by(OBSERVER.ID) %>% 
  summarise(LATEST.LIST.DATE = max(OBSERVATION.DATE),
            OLDEST.LIST.DATE = min(OBSERVATION.DATE),
            OLDEST.EDIT.DATE = min(LAST.EDITED.DATE))

# numbers of lists
data2 <- data0_filt %>% 
  group_by(OBSERVER.ID) %>% 
  mutate(TOT.LISTS = n_distinct(SAMPLING.EVENT.IDENTIFIER)) %>% 
  filter(ALL.SPECIES.REPORTED == 1) %>% 
  summarise(TOT.LISTS = min(TOT.LISTS),
            C.LISTS = n_distinct(SAMPLING.EVENT.IDENTIFIER),
            COMPLETE.RATIO = 100*C.LISTS/TOT.LISTS)

# complete checklist frequency
temp <- data0_filt %>% 
  filter(ALL.SPECIES.REPORTED == 1, YEAR == 2022) %>% 
  group_by(OBSERVER.ID, YEAR, MONTH) %>% 
  summarise(CLIST.FREQ = replace_na(n_distinct(SAMPLING.EVENT.IDENTIFIER), 0)) %>% 
  group_by(OBSERVER.ID) %>% 
  summarise(CLIST.FREQ.2022 = ceiling(mean(CLIST.FREQ)))

data3 <- data0_filt %>% 
  filter(ALL.SPECIES.REPORTED == 1) %>% 
  group_by(OBSERVER.ID, YEAR, MONTH) %>% 
  summarise(CLIST.FREQ = replace_na(n_distinct(SAMPLING.EVENT.IDENTIFIER), 0)) %>% 
  group_by(OBSERVER.ID) %>% 
  summarise(CLIST.FREQ.ALL = ceiling(mean(CLIST.FREQ))) %>% 
  left_join(temp) %>% 
  mutate(CLIST.FREQ.2022 = replace_na(CLIST.FREQ.2022, 0))
  
# media
data4 <- data_filt %>% 
  group_by(OBSERVER.ID) %>% 
  filter(HAS.MEDIA == 1) %>% 
  summarise(OBS.W.MEDIA = n_distinct(COMMON.NAME)) %>% 
  right_join(data0_filt %>% distinct(OBSERVER.ID)) %>% 
  mutate(OBS.W.MEDIA = replace_na(OBS.W.MEDIA, 0))


# final metadata sheet --------------------------------------------------------------

data_final <- ids %>% 
  left_join(data1) %>% 
  left_join(data2) %>% 
  left_join(data3) %>% 
  left_join(data4)
  
write_csv(data_final, "ebirder-stats/ebirder-stats.csv")
