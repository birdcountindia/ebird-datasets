############ creating latest group accounts list ### ### ### 

library(lubridate)
library(tidyverse)
library(glue)
library(skimmr)

source("functions.R")

#### variable parameters (update manually) ####

userspath <- "EBD/ebd_users_relNov-2023.txt"
prev_gapath <- "group-accounts/ebd_users_GA_relMay-2023.csv"


#### automated parameters ####

source("monthly-param-auto.R") 
# when this also functionised, can add extra = FALSE argument here too

latestusersrel <- str_extract(userspath, "(?<=rel)[^.]*(?=.|$)")
groupaccspath <- glue("group-accounts/ebd_users_GA_rel{latestusersrel}.csv")


#### creating list of group accounts ####

# If variable parameter "userspath" has been updated to latest, the "groupaccspath" string
# is also updated automatically but the file does not exist. Hence, the below lines run.
# If "userspath" has not been updated to latest, the groupaccspath file exists, so
# this won't run.

if (!file.exists(userspath) & !file.exists(groupaccspath)) {
  print("Latest users data does not exist.")
} else if (file.exists(userspath) & !file.exists(groupaccspath)) {
  
  unzip_ebd()
  
  preimp <- c("OBSERVER.ID")
  
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
                "trust","ollect","Monitor","monitor","mateur","aturalists")
  
  # other group accounts without keywords in name
  accounts <- c("GBCN Goa")
  
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
  
} else {
  print("Latest group accounts data already exists!")
}

# will modify the .csv by assigning GA.1 and GA.2 values with justifications (for new accs)
