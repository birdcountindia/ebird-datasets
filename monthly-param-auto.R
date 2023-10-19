### automated parameters for master monthly script

require(tidyverse)
require(lubridate)
require(glue)

# (only works when using new rel. data in the month it comes out, e.g., working with relJun in July)

# to make code robust against (day == 31) in which case the other lines produce NA:

# date under consideration for current leaderboard
cur_date <- if (today() %>% day() < 16) { 
  (today() - months(1)) %>% floor_date(unit = "month")
} else {
  today() %>% floor_date(unit = "month")
}

rel_date <- if (today() %>% day() < 16) {
  ((today() - months(1)) - months(1)) %>%
    floor_date(unit = "month")
} else {
  (today() - months(1)) %>%
    floor_date(unit = "month")
}

# for PJ's metrics
prevrel_date <- if (today() %>% day() < 16) {
  ((today() - months(1)) - months(2)) %>%
    floor_date(unit = "month")
} else {
  (today() - months(2)) %>%
    floor_date(unit = "month")
}
# 
# cur_date <- "2023-08-01"
# rel_date <- "2023-07-01"
# prevrel_date <- "2023-06-01"

cur_year <- cur_date %>% year()
cur_month_num <- cur_date %>% month()
cur_month_lab <- cur_date %>% month(label = T, abbr = T)

rel_year <- rel_date %>% year()
rel_month_num <- rel_date %>% month()
rel_month_lab <- rel_date %>% month(label = T, abbr = T) 


# for PJ's metrics
PrevMonthNum <- prevrel_date %>% month()
PrevMonthLab <- prevrel_date %>% month(label = T, abbr = T) 
CurMonthLab <- rel_month_lab
CurMonth <- rel_month_num
CurYear <- rel_year
PrevYear <- rel_year - 1
Months <- seq((as_date(cur_date) - months(6)), (as_date(cur_date) - months(1)), 
              by = "month") %>% month()

#


path_zip <- glue("EBD/{dataset_str}{rel_month_lab}-{rel_year}.zip")

file_ebd_main <- glue("{dataset_str}{rel_month_lab}-{rel_year}.txt")
file_ebd_unv <- glue("{dataset_str}{rel_month_lab}-{rel_year}_unvetted.txt")
file_sed <- glue("{dataset_str}{rel_month_lab}-{rel_year}_sampling.txt")

path_ebd_main <- get_full_path(file_ebd_main)
path_ebd_unv <- get_full_path(file_ebd_unv)
path_sed <- get_full_path(file_sed)

maindatapath <-  glue("EBD/ebd_IN_rel{rel_month_lab}-{rel_year}.RData")
slicedatapath <-  glue("EBD/ebd_IN_rel{rel_month_lab}-{rel_year}_slice.RData")
pmpdatapath <- glue("EBD/pmp_rel{rel_month_lab}-{rel_year}.RData")
mcdatapath <-  glue("EBD/ebd_IN_rel{rel_month_lab}-{rel_year}_{toupper(rel_month_lab)}.RData")
ycdatapath <-  glue("EBD/ebd_IN_rel{rel_month_lab}-{rel_year}_{rel_year}.RData")

# for country
coveragedatapath <- glue("ebirding-coverage/data/{rel_year}-{rel_month_num}/rel{rel_month_lab}-{rel_year}_IN.csv")
coveragemappath1 <- glue("ebirding-coverage/maps/{rel_year}-{rel_month_num}/rel{rel_month_lab}-{rel_year}_IN_annot.png")
coveragemappath2 <- glue("ebirding-coverage/maps/{rel_year}-{rel_month_num}/rel{rel_month_lab}-{rel_year}_IN_plain.png")

# for states
state_info <- read_csv("ebirding-coverage/state_info.csv", show_col_types = F)

coveragepaths_st <- state_info %>% 
  # paths without file names
  mutate(PATH.DATA = glue("ebirding-coverage/data/{rel_year}-{rel_month_num}/"),
         PATH.MAPS = glue("ebirding-coverage/maps/{rel_year}-{rel_month_num}/")) %>% 
  mutate(DATA = glue("{PATH.DATA}rel{rel_month_lab}-{rel_year}_IN-{STATE.CODE}.csv"),
         MAP1 = glue("{PATH.MAPS}rel{rel_month_lab}-{rel_year}_IN-{STATE.CODE}_annot.png"),
         MAP2 = glue("{PATH.MAPS}rel{rel_month_lab}-{rel_year}_IN-{STATE.CODE}_plain.png"))

# creating folders if they don't exist
if (!dir.exists(coveragepaths_st$PATH.DATA[1])) {
  dir.create(coveragepaths_st$PATH.DATA[1])
}

if (!dir.exists(coveragepaths_st$PATH.MAPS[1])) {
  dir.create(coveragepaths_st$PATH.MAPS[1])
}
