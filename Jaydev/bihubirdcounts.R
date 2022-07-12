library(tidyverse)
library(lubridate)

load("EBD/ebd_IN_relMar-2022.RData")

data0 <- data %>% 
  filter(STATE == "Assam")

details <- readxl::read_xlsx("Jaydev/All_BBC_details.xlsx") %>% 
  mutate(Month = case_when(Month == "January" ~ 1,
                           Month == "April" ~ 4,
                           Month == "October" ~ 10)) %>% 
  summarise(`BBC Details` = `BBC Details`,
            OBSERVATION.DATE = as_date(paste0(Year,"-",Month,"-",Days)))

data1 <- data0 %>% 
  inner_join(details)

write_csv(data1, file = "Jaydev/bihubirdcounts.csv")
