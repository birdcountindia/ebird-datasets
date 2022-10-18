### automated parameters for master monthly script

require(tidyverse)
require(lubridate)
require(glue)

# (only works when using new rel. data in the month it comes out, e.g., working with relJun in July)

# to make code robust against (day == 31) in which case the other lines produce NA:

# date under consideration for current leaderboard
cur_date <- if (today() %>% day() == 31) { 
  (today() - days(1)) %>% floor_date(unit = "month")
} else {
  today() %>% floor_date(unit = "month")
}

rel_date <- if (today() %>% day() == 31) {
  ((today() - days(1)) - months(1)) %>%
    floor_date(unit = "month")
} else {
  (today() - months(1)) %>%
    floor_date(unit = "month")
}

cur_year <- cur_date %>% year()
cur_month_num <- cur_date %>% month()
cur_month_lab <- cur_date %>% month(label = T, abbr = T)

rel_year <- rel_date %>% year()
rel_month_num <- rel_date %>% month()
rel_month_lab <- rel_date %>% month(label = T, abbr = T) 


# for PJ's metrics
CurMonth <- rel_month_num
CurYear <- rel_year
PrevYear <- rel_year - 1
Months <- seq((today() - months(6)), (today() - months(1)), by = "month") %>% month()


zippath <- glue("EBD/{dataset_str}{rel_month_lab}-{rel_year}.zip")
rawfile <- glue("{dataset_str}{rel_month_lab}-{rel_year}.txt")
rawpath <- glue("EBD/{rawfile}")


maindatapath <-  glue("EBD/ebd_IN_rel{rel_month_lab}-{rel_year}.RData")
slicedatapath <-  glue("EBD/ebd_IN_rel{rel_month_lab}-{rel_year}_slice.RData")
pmpdatapath <- glue("EBD/pmp_rel{rel_month_lab}-{rel_year}.RData")
mcdatapath <-  glue("EBD/ebd_IN_rel{rel_month_lab}-{rel_year}_{toupper(rel_month_lab)}.RData")

coveragedatapath <- glue("ebirding-coverage/data/coverage_rel{rel_month_lab}-{rel_year}.csv")
coveragemappath1 <- glue("ebirding-coverage/maps/coverage_rel{rel_month_lab}-{rel_year}_annot.png")
coveragemappath2 <- glue("ebirding-coverage/maps/coverage_rel{rel_month_lab}-{rel_year}_plain.png")
