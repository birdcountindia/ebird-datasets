############ creating latest group accounts list ### ### ### 

library(lubridate)
library(tidyverse)
library(glue)

#### variable parameters (update manually) ####

userspath <- "EBD/ebd_users_relFeb-2023.txt"
prev_gapath <- "group-accounts/ebd_users_GA_relMay-2022.csv"

dataset_str <- "ebd_IN_prv_rel" # or "ebd_IN_rel" if no unvetted data

preimp <- c("OBSERVER.ID")


#### automated parameters ####

cur_year <- today() %>% year()
cur_month_num <- today() %>% month()
cur_month_lab <- today() %>% month(label = T, abbr = T)

rel_year <- (today() - months(1)) %>% year()
rel_month_num <- (today() - months(1)) %>% month()
rel_month_lab <- (today() - months(1)) %>% month(label = T, abbr = T) 

path_zip <- glue("EBD/{dataset_str}{rel_month_lab}-{rel_year}.zip")
file_ebd_main <- glue("{dataset_str}{rel_month_lab}-{rel_year}.txt")
path_ebd_main <- glue("EBD/{file_ebd_main}")

latestusersrel <- str_extract(userspath, "(?<=rel)[^.]*(?=.|$)")
groupaccspath <- glue("group-accounts/ebd_users_GA_rel{latestusersrel}.csv")


#### creating list of group accounts ####

# If variable parameter "userspath" has been updated to latest, the "groupaccspath" string
# is also updated automatically but the file does not exist. Hence, the below lines run.
# If "userspath" has not been updated to latest, the groupaccspath file exists, so
# this won't run.

if (file.exists(userspath) & !file.exists(groupaccspath)) {
  
  if (!file.exists(path_ebd_main) & file.exists(path_zip)) {
    unzip(zipfile = path_zip, files = file_ebd_main, exdir = "EBD")
  } else if (!file.exists(path_ebd_main) & !file.exists(path_zip)) {
    print("Latest data download does not exist!")
    }

  ### main EBD ###
  nms <- names(read.delim(path_ebd_main, nrows = 1, sep = "\t", header = T, quote = "", 
                          stringsAsFactors = F, na.strings = c(""," ", NA)))
  nms[!(nms %in% preimp)] <- "NULL"
  nms[nms %in% preimp] <- NA
  data <- read.delim(path_ebd_main, colClasses = nms, sep = "\t", header = T, quote = "",
                     stringsAsFactors = F, na.strings = c(""," ",NA)) 
  
  data <- data %>% distinct(OBSERVER.ID)

  eBird_users <- read.delim(userspath, sep = "\t", header = T, 
                            quote = "", stringsAsFactors = F, na.strings = c(""," ",NA))
  eBird_users <- eBird_users %>% 
    transmute(OBSERVER.ID = observer_id,
              FULL.NAME = paste(first_name, last_name, sep = " "))
  data0 <- left_join(data, eBird_users)
  
  
  # textual filters for group accounts
  keywords <- c("Group","group","Survey","survey","Atlas","atlas","rganization",
                "rganisation","oundation","AWC","awc","Census","census","Bird","bird",
                "Count","count","niversity","ollege","Centre","Center","centre","center",
                "School","school","Club","club","City","city","State","state",
                "202","201","200","rnitholog","arathon","ociety","istoric","nstitut",
                "etwork","Team","team","estival","Fest","fest","esearch","ecord",
                "roject","iodivers","ational","eserve","rotect","Lodge","lodge","esort",
                "anctuary","ildlife","epartment","Dept","dept","onservation","Trust",
                "trust","ollect","Monitor","monitor")
  
  # other group accounts without keywords in name
  accounts <- c("GBCN Goa", "Dolphin Bengaluru", "obsr1288653") # same Dolphin but ID 
  
  data1 <- data0 %>% 
    filter(str_detect(FULL.NAME, paste(keywords, collapse = "|")) |
             str_detect(FULL.NAME, paste(accounts, collapse = "|"))) %>% 
    select(OBSERVER.ID)
  
  
  prev_galist <- read_csv(prev_gapath) %>% select(-FULL.NAME) 
  
  
  data2 <- data1 %>% 
    # joining GA.1, GA.2 and justification columns for old users
    left_join(prev_galist) %>% 
    left_join(eBird_users) %>% 
    arrange(GA.1)
  
  temppath <- glue('{str_extract(groupaccspath, "[^.]*(?=.|$)")}_0.csv')
  
  write_csv(data2, file = temppath, na = "")
  
} else if (!file.exists(userspath) & !file.exists(groupaccspath)) {
  print("Latest users data does not exist.")
} else {
  print("Latest group accounts data already exists!")
}

# will modify the .csv by assigning GA.1 and GA.2 values with justifications (for new accs)
