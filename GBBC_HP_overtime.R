library(tidyverse)
library(writexl)


senspath <- "EBD/ebd_sensitive_relFeb-2023_IN.txt" 
groupaccspath <- "group-accounts/ebd_users_GA_relFeb-2023.csv"

dataset_str <- "ebd_IN_prv_rel" # or "ebd_IN_rel" if no unvetted data
source("monthly-param-auto.R")

load(maindatapath)
source("https://raw.githubusercontent.com/birdcountindia/bci-functions/main/summaries.R")



data0 <- data %>% 
  filter(STATE.CODE == "IN-HP", 
         MONTH == 2) %>% 
  # GBBCs
  filter((YEAR == 2014 & DAY.M %in% 14:17) |
           (YEAR == 2015 & DAY.M %in% 13:16) |
           (YEAR == 2016 & DAY.M %in% 12:15) |
           (YEAR == 2017 & DAY.M %in% 17:20) |
           (YEAR == 2018 & DAY.M %in% 16:19) |
           (YEAR == 2019 & DAY.M %in% 15:18) |
           (YEAR == 2020 & DAY.M %in% 14:17) |
           (YEAR == 2021 & DAY.M %in% 12:15) |
           (YEAR == 2022 & DAY.M %in% 18:21) |
            (YEAR == 2023 & DAY.M %in% 17:20))

summary <- data0 %>% 
  group_by(YEAR) %>% 
  basic_stats(pipeline = T, prettify = F)


# top 5 common species, over time

comm_spec <- data0 %>% 
  group_by(YEAR) %>% 
  mutate(TOT.LISTS = n_distinct(GROUP.ID)) %>% 
  group_by(YEAR, COMMON.NAME) %>% 
  summarise(NO.LISTS = n_distinct(GROUP.ID),
            REP.FREQ = 100*NO.LISTS/min(TOT.LISTS)) %>% 
  arrange(desc(REP.FREQ)) %>% 
  slice(1:5) %>% 
  ungroup()



write_xlsx(x = list("Stats" = summary, 
                    "Common species" = comm_spec),
           path = "GBBC_HP_overtime.xlsx")
  