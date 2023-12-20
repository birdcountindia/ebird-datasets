library(lubridate)
library(tidyverse)
library(glue)
library(skimmr)
library(magick)
library(scales) # for comma format of numbers
library(grid)
library(googledrive)
library(googlesheets4)

# library(data.table) # required, but later command explicitly calls for it


source("functions.R")


### required files in directory: ###
# - latest EBD release .zip/.txt file
# - latest sensitive species data .txt file 
# - latest users data .txt file 
# - previous group accounts list .csv file
# - spatial data (pre-processed) as "maps.RData" file
# - BCI logo with translucent bg frame as .png file
# - five different R scripts in BCI-metrics/ folder
###   ###


#### updating latest users, groupaccs, senssp data ####

source("EBD/latest_non-EBD_paths.R")


#### parameters ####

# update when latest available
senspath <- "EBD/ebd_sensitive_relNov-2023_IN.txt" 
groupaccspath <- "group-accounts/ebd_users_GA_relNov-2023.csv"


preimp <- c("CATEGORY","EXOTIC.CODE","COMMON.NAME","OBSERVATION.COUNT",
            "LOCALITY.ID","LOCALITY.TYPE","REVIEWED","APPROVED","LAST.EDITED.DATE",
            "STATE","STATE.CODE","COUNTY","COUNTY.CODE",
            "LATITUDE","LONGITUDE","OBSERVATION.DATE","TIME.OBSERVATIONS.STARTED","OBSERVER.ID",
            "PROTOCOL.TYPE","DURATION.MINUTES","EFFORT.DISTANCE.KM","LOCALITY","BREEDING.CODE",
            "NUMBER.OBSERVERS","ALL.SPECIES.REPORTED","GROUP.IDENTIFIER","SAMPLING.EVENT.IDENTIFIER",
            "TRIP.COMMENTS","SPECIES.COMMENTS", "HAS.MEDIA")

# for PJ's metrics
preimp_metrics <- c("COMMON.NAME", "STATE.CODE", "COUNTY.CODE", "OBSERVATION.DATE",
                    # "OBSERVATION.COUNT",
                    "OBSERVER.ID", "SAMPLING.EVENT.IDENTIFIER", "ALL.SPECIES.REPORTED",
                    "GROUP.IDENTIFIER", "HAS.MEDIA", "APPROVED", "COUNTY")


### automation parameters ###

# paths to latest versions of user & GA info, and sensitive species data
load("EBD/latest_non-EBD_paths.RData")

# (only works when using new rel. data in the month it comes out, e.g., working with relJun in July)
source("monthly-param-auto.R")

#### authenticating GDrive/GSheets for upload of coverage, metrics ####

drive_auth(email = "birdcountindia@ncf-india.org")
gs4_auth(email = "birdcountindia@ncf-india.org")


#### unzipping EBD download (if not done already) ####

unzip_ebd()

# SED
if (dataset_str == "ebd_IN_unv_smp_rel") {
  if (!file.exists(rawpath_sed) & file.exists(zippath)) {
    unzip(zipfile = zippath, files = rawfile_sed, exdir = "EBD") # don't add trailing slash in path
    print("Data download unzipped.")
  } else if (!file.exists(rawpath_sed) & !file.exists(zippath)) {
    print("Latest data download does not exist!")
  } else {
    print("Data download already unzipped.")
  }
}

#### main data processing steps ####

### main EBD ###

# this method using base R import takes only 373 sec with May 2022 release
data <- read.ebd(path_ebd_main, preimp) 

# # tidy import takes way longer, a total of 877 sec, but could be useful for smaller data
# data <- read_delim(path_ebd_main, col_select = preimp,
#                    name_repair = make.names, # base R nomencl. with periods for spaces
#                    quote = "", na = c(""," ", NA), show_col_types = F)


### sensitive species ###
senssp <- read.ebd(senspath, preimp)

### combing the two ###
data <- bind_rows(data, senssp) %>% 
  # filtering unvetted as well as exotic species (provisional and escapee)
  filter(APPROVED == 1 & !(EXOTIC.CODE %in% c("P", "X")))


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


### sed ###
data_sed <- read.ebd(path_sed, preimp)

rm(.Random.seed)
save(data, data_sed, 
     file = maindatapath)


### sliced data ###
data_slice_G <- data %>% group_by(GROUP.ID) %>% slice(1) %>% ungroup()
# data_slice_S <- data %>% group_by(SAMPLING.EVENT.IDENTIFIER) %>% slice(1) %>% ungroup()

if (exists("data_slice_S")) {
  save(data_slice_G, data_slice_S, file = slicedatapath)
} else {
  save(data_slice_G, file = slicedatapath)
}

#### filtering for PMP (only for alternate months) ####

pmp_months <- seq(1, 12, by = 6) # Jan and Jul

if (real_month_num %in% pmp_months) {
  
  data_pmp <- data %>% group_by(GROUP.ID) %>%
    filter(any(OBSERVER.ID == "obsr2607928")) %>% # PMP's eBird account ID
    ungroup()
  
  save(data_pmp, file = pmpdatapath)
  
} else {
  print("Quitting from filtering for PMP.")
}



#### filtering for monthly challenge ####

data_mc <- data %>% filter(YEAR == currel_year, MONTH == currel_month_num)

rm(.Random.seed)
save(data_mc, file = mcdatapath)



#### filtering for yearly challenge (only for January) ####

if (real_month_num == 1) {
  
  data_yc <- data %>% filter(YEAR == currel_year)
  
  save(data_yc, file = ycdatapath)
  
} else {
  print("Quitting from filtering for yearly challenge.")
}


#### generating PJ's monthly metrics out of EBD ####

print(glue("Generating metrics for {currel_month_lab} {currel_year} from {path_ebd_main}"))

source("BCI-metrics/ebdMetrics.R")

#### generating monthly coverage stats and maps ####

source("ebirding-coverage/ebirding-coverage.R")
