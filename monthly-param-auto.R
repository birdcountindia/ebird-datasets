
dataset_str <- "ebd_IN_unv_smp_rel" # or "ebd_IN_rel" if no unvetted data

### automated parameters for master monthly script

require(tidyverse)
require(lubridate)
require(glue)


get_param(extra = TRUE)
# get_param(date_currel = "2023-09-01", extra = TRUE)


#

path_zip <- glue("EBD/{dataset_str}{currel_month_lab}-{currel_year}.zip")

file_ebd_main <- glue("{dataset_str}{currel_month_lab}-{currel_year}.txt")
file_ebd_unv <- glue("{dataset_str}{currel_month_lab}-{currel_year}_unvetted.txt")
file_sed <- glue("{dataset_str}{currel_month_lab}-{currel_year}_sampling.txt")

path_ebd_main <- get_full_path(file_ebd_main)
path_ebd_unv <- get_full_path(file_ebd_unv)
path_sed <- get_full_path(file_sed)

maindatapath <-  glue("EBD/ebd_IN_rel{currel_month_lab}-{currel_year}.RData")
slicedatapath <-  glue("EBD/ebd_IN_rel{currel_month_lab}-{currel_year}_slice.RData")
pmpdatapath <- glue("EBD/pmp_rel{currel_month_lab}-{currel_year}.RData")
mcdatapath <-  glue("EBD/ebd_IN_rel{currel_month_lab}-{currel_year}_{toupper(currel_month_lab)}.RData")
ycdatapath <-  glue("EBD/ebd_IN_rel{currel_month_lab}-{currel_year}_{currel_year}.RData")


# coverage paths

# for country
coveragedatapath <- glue("ebirding-coverage/data/{currel_year}-{str_pad(currel_month_num, width=2, pad='0')}/rel{currel_month_lab}-{currel_year}_IN.csv")
coveragemappath1 <- glue("ebirding-coverage/maps/{currel_year}-{str_pad(currel_month_num, width=2, pad='0')}/rel{currel_month_lab}-{currel_year}_IN_annot.png")
coveragemappath2 <- glue("ebirding-coverage/maps/{currel_year}-{str_pad(currel_month_num, width=2, pad='0')}/rel{currel_month_lab}-{currel_year}_IN_plain.png")

# for states
state_info <- read_csv("ebirding-coverage/state_info.csv", show_col_types = F)

coveragepaths_st <- state_info %>% 
  # paths without file names
  mutate(PATH.DATA = glue("ebirding-coverage/data/{currel_year}-{str_pad(currel_month_num, width=2, pad='0')}/"),
         PATH.MAPS = glue("ebirding-coverage/maps/{currel_year}-{str_pad(currel_month_num, width=2, pad='0')}/")) %>% 
  mutate(DATA = glue("{PATH.DATA}rel{currel_month_lab}-{currel_year}_IN-{STATE.CODE}.csv"),
         MAP1 = glue("{PATH.MAPS}rel{currel_month_lab}-{currel_year}_IN-{STATE.CODE}_annot.png"),
         MAP2 = glue("{PATH.MAPS}rel{currel_month_lab}-{currel_year}_IN-{STATE.CODE}_plain.png"))


# growth paths

path_growth <- glue("ebirding-growth/{currel_year}-{str_pad(currel_month_num, width=2, pad='0')}/")


# creating folders if they don't exist
if (!dir.exists(coveragepaths_st$PATH.DATA[1])) {
  dir.create(coveragepaths_st$PATH.DATA[1])
}

if (!dir.exists(coveragepaths_st$PATH.MAPS[1])) {
  dir.create(coveragepaths_st$PATH.MAPS[1])
}

if (!dir.exists(path_growth)) {
  dir.create(path_growth)
}
